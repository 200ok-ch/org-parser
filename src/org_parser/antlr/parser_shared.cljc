(ns org-parser.antlr.parser-shared
  (:require [clojure.string :as str]
            [org-parser.antlr.postprocess :as postprocess]))

(declare footnote-link->ast text->ast)

(defn text-node [s]
  [:text [:text-normal s]])

#?(:cljs
   (defn- token-start-index [token]
     (or (some-> token .-start)
         (when (and token (fn? (.-getStartIndex token)))
           (.getStartIndex token))
         0)))

#?(:cljs
   (defn- token-stop-index [token default-start]
     (or (some-> token .-stop)
         (when (and token (fn? (.-getStopIndex token)))
           (.getStopIndex token))
         default-start)))

(defn ctx-span [ctx]
  #?(:clj
     (let [start-token (.getStart ctx)
           stop-token (.getStop ctx)
           start-idx (.getStartIndex start-token)
           stop-idx (if stop-token (.getStopIndex stop-token) start-idx)]
       [start-idx (inc stop-idx)])
     :cljs
     (let [start-token (or (.-start ctx)
                           (.getStart ctx))
           stop-token (or (.-stop ctx)
                          (.getStop ctx))
           start-idx (token-start-index start-token)
           stop-idx (token-stop-index stop-token start-idx)]
       [start-idx (inc stop-idx)])))

(defn with-span [node ctx]
  (with-meta node {:span (ctx-span ctx)}))

(defn failure [reason start raw]
  {:failure? true
   :backend :antlr
   :reason reason
   :start start
   :raw raw})

(def planning-keywords
  {"SCHEDULED" :planning-kw-scheduled
   "DEADLINE" :planning-kw-deadline
   "CLOSED" :planning-kw-closed})

(def affiliated-keywords
  #{"HEADER" "NAME" "PLOT" "RESULTS" "CAPTION" "AUTHOR" "DATE" "TITLE"})

(def text-style-tags
  {"*" :text-sty-bold
   "/" :text-sty-italic
   "_" :text-sty-underlined
   "=" :text-sty-verbatim
   "~" :text-sty-code
   "+" :text-sty-strikethrough})

(def code-like-style-tags
  #{:text-sty-verbatim :text-sty-code})

(def noparse-block-names
  #{"src" "example" "export" "comment"})

(def ts-mod-units
  #{"h" "d" "w" "m" "y"})

(defn ctx-text [ctx]
  (some-> ctx .getText))

(defn valid-ts-time? [raw]
  (boolean (re-matches #"\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?" raw)))

(defn valid-ts-unit? [unit]
  (contains? ts-mod-units (some-> unit str/lower-case)))

(defn- ts-mod-at-least->ast [ctx]
  (when ctx
    (let [value (ctx-text (.tsModValue ctx))
          unit (ctx-text (.tsModUnit ctx))]
      (when (and value (valid-ts-unit? unit))
        [:ts-mod-at-least [:ts-mod-value value] [:ts-mod-unit unit]]))))

(defn- ts-repeater->ast [ctx]
  (let [typ (ctx-text (.tsRepeaterType ctx))
        value (ctx-text (.tsModValue ctx))
        unit (ctx-text (.tsModUnit ctx))
        at-least (ts-mod-at-least->ast (.tsModAtLeast ctx))]
    (when (and typ value (valid-ts-unit? unit))
      (cond-> [:ts-repeater
               [:ts-repeater-type typ]
               [:ts-mod-value value]
               [:ts-mod-unit unit]]
        at-least (conj at-least)))))

(defn- ts-warning->ast [ctx]
  (let [typ (ctx-text (.tsWarningType ctx))
        value (ctx-text (.tsModValue ctx))
        unit (ctx-text (.tsModUnit ctx))
        at-least (ts-mod-at-least->ast (.tsModAtLeast ctx))]
    (when (and typ value (valid-ts-unit? unit))
      (cond-> [:ts-warning
               [:ts-warning-type typ]
               [:ts-mod-value value]
               [:ts-mod-unit unit]]
        at-least (conj at-least)))))

(defn- ts-modifier->ast [ctx]
  (or (some-> ctx .tsRepeater ts-repeater->ast)
      (some-> ctx .tsWarning ts-warning->ast)))

(defn- ts-modifiers->ast [ctx]
  (let [mods (if ctx
               (mapv ts-modifier->ast (.tsModifier ctx))
               [])]
    (when (every? some? mods)
      (into [:ts-modifiers] mods))))

(defn- ts-time-node [ctx]
  (let [raw (ctx-text ctx)]
    (when (valid-ts-time? raw)
      [:ts-time raw])))

(defn- ts-inner-w-time->ast [ctx]
  (let [date (ctx-text (.tsDate ctx))
        day (some-> ctx .tsDay ctx-text)
        time-node (ts-time-node (.tsTime ctx))]
    (when (and date time-node)
      (cond-> [:ts-inner-w-time [:ts-date date]]
        day (conj [:ts-day day])
        true (conj time-node)))))

(defn- ts-inner->ast [ctx]
  (let [date (ctx-text (.tsDate ctx))
        day (some-> ctx .tsDay ctx-text)
        time-node (some-> ctx .tsTime ts-time-node)
        modifiers (ts-modifiers->ast (.tsModifiers ctx))]
    (when modifiers
      [:ts-inner
       (if time-node
         (cond-> [:ts-inner-w-time [:ts-date date]]
           day (conj [:ts-day day])
           true (conj time-node))
         (cond-> [:ts-inner-wo-time [:ts-date date]]
           day (conj [:ts-day day])))
       modifiers])))

(defn- ts-inner-span->ast [ctx]
  (let [date (ctx-text (.tsDate ctx))
        day (some-> ctx .tsDay ctx-text)
        start-time (ts-time-node (.tsTime ctx 0))
        end-time (ts-time-node (.tsTime ctx 1))
        modifiers (ts-modifiers->ast (.tsModifiers ctx))]
    (when (and date start-time end-time modifiers)
      [:ts-inner-span
       (cond-> [:ts-inner-w-time [:ts-date date]]
         day (conj [:ts-day day])
         true (conj start-time))
       end-time
       modifiers])))

(defn- node-property-line->ast [ctx]
  (with-span
    (cond-> [:node-property-line [:node-property-name (ctx-text (.nodePropertyName ctx))]]
      (.PLUS ctx) (conj [:node-property-plus])
      (.nodePropertyValue ctx) (conj [:node-property-value (text-node (ctx-text (.nodePropertyValue ctx)))]))
    ctx))

(defn- property-drawer->ast [ctx]
  (let [props (mapv node-property-line->ast (.nodePropertyLine ctx))]
    (if (or (not= "PROPERTIES" (ctx-text (.drawerName (.drawerBeginLine ctx))))
            (some :failure? props))
      (failure :invalid-property-drawer :property-drawer (ctx-text ctx))
      (with-span (into [:property-drawer] props) ctx))))

(defn- fixed-width-line->ast [ctx]
  (with-span [:fixed-width-line (or (some-> ctx .fixedWidthValue ctx-text) "")] ctx))

(defn- fixed-width-area->ast [ctx]
  (with-span (into [:fixed-width-area] (map fixed-width-line->ast (.fixedWidthLine ctx))) ctx))

(defn- link-ext-other->ast [ctx]
  (let [raw (ctx-text ctx)
        scheme (or (some-> ctx .linkUrlScheme ctx-text)
                   (some->> (re-matches #"^([^:]+):(.*)$" raw) second))
        rest (or (some-> ctx .linkUrlRest ctx-text)
                 (some->> (re-matches #"^[^:]+:(.*)$" raw) second))]
    (with-span (cond-> [:link-ext-other [:link-url-scheme scheme]]
                 (some? rest) (conj [:link-url-rest rest]))
               ctx)))

(defn- link-ext-other-fields [node]
  [(second (second node))
   (when-let [rest-node (nth node 2 nil)]
     (second rest-node))])

(defn- link-ext-id->ast [ctx]
  (if (= "id" (ctx-text (.linkIdPrefix ctx)))
    (with-span [:link-ext-id (ctx-text (.linkIdValue ctx))] ctx)
    (failure :invalid-link-ext-id :link-ext-id (ctx-text ctx))))

(defn- link-file-location-node [ctx]
  (cond
    (.linkFileLocationLine ctx) [:link-file-loc-lnum (ctx-text (.linkFileLocationLine ctx))]
    (.linkFileLocationHeadline ctx) [:link-file-loc-headline (subs (ctx-text (.linkFileLocationHeadline ctx)) 1)]
    (.linkFileLocationCustomId ctx) [:link-file-loc-customid (subs (ctx-text (.linkFileLocationCustomId ctx)) 1)]
    (.linkFileLocationString ctx) [:link-file-loc-string (ctx-text (.linkFileLocationString ctx))]))

(defn- link-ext-file->ast [ctx]
  (let [raw (ctx-text ctx)
        scheme (some-> ctx .linkFileScheme ctx-text)
        path (ctx-text (.linkFilePath ctx))
        loc-ctx (.linkFileLocation ctx)
        looks-like-other-scheme? (and (nil? scheme)
                                      (not (re-matches #"(?i)[a-z]:.*" path))
                                      (or (re-matches #"(?i)(?:https?|ftp|mailto):.*" path)
                                          (re-matches #"(?i)[a-z][a-z0-9+.-]*://.*" path)))]
    (if (and (seq path)
             (or (nil? scheme) (= "file:" (str/lower-case scheme)))
             (not looks-like-other-scheme?))
      (with-span (cond-> [:link-ext-file path]
                   loc-ctx (conj (link-file-location-node loc-ctx)))
                 ctx)
      (failure :invalid-link-ext-file :link-ext-file raw))))

(defn- text-link->ast [ctx]
  (let [link-ctx (or (some-> ctx .textLinkAngle .linkExtOther)
                     (some-> ctx .textLinkPlain .linkExtOther))
        link (link-ext-other->ast link-ctx)
        [scheme path] (link-ext-other-fields link)]
    (if (:failure? link)
      link
      (with-span
        [:text-link
         (if (.textLinkAngle ctx)
             [:text-link-angle
              [:link-url-scheme scheme]
              [:text-link-angle-path (or path "")]]
            [:text-link-plain
             [:link-url-scheme scheme]
             [:text-link-plain-path (or path "")]])]
        ctx))))

(defn- ts-time->ast [ctx]
  (if-let [node (ts-time-node ctx)]
    (with-span node ctx)
    (failure :invalid-ts-time :ts-time (ctx-text ctx))))

(defn- timestamp-inactive-range->ast [ctx]
  (let [span-ctx (.tsInnerSpan ctx)]
    (if span-ctx
      (if-let [span (ts-inner-span->ast span-ctx)]
        (with-span [:timestamp-inactive-range span] ctx)
        (failure :invalid-timestamp-inactive-range :timestamp-inactive-range (ctx-text ctx)))
      (let [inners (mapv ts-inner-w-time->ast (.tsInnerWTime ctx))]
        (if (every? some? inners)
          (with-span (into [:timestamp-inactive-range] inners) ctx)
          (failure :invalid-timestamp-inactive-range :timestamp-inactive-range (ctx-text ctx)))))))

(defn- timestamp->ast [ctx]
  (let [raw (ctx-text ctx)
        active-tag (if (str/starts-with? raw "<") :timestamp-active :timestamp-inactive)]
    (cond
      (.diarySexpBody ctx)
      (with-span [:timestamp [:timestamp-diary (ctx-text (.diarySexpBody ctx))]] ctx)

      (.tsInnerSpan ctx)
      (if-let [span (ts-inner-span->ast (.tsInnerSpan ctx))]
        (with-span [:timestamp [active-tag span]] ctx)
        (failure :invalid-timestamp :timestamp raw))

      :else
      (let [inners (mapv ts-inner->ast (.tsInner ctx))]
        (if (and (seq inners) (every? some? inners))
          (with-span [:timestamp (into [active-tag] inners)] ctx)
          (failure :invalid-timestamp :timestamp raw))))))

(defn- planning-timestamp->ast [ctx]
  (let [tag (if (= "[" (subs (ctx-text ctx) 0 1)) :timestamp-inactive :timestamp-active)]
    (when-let [inner (ts-inner->ast (.tsInner ctx))]
      [:timestamp [tag inner]])))

(defn- planning-info->ast [ctx]
  (let [kw (ctx-text (.planningKeyword ctx))
        tag (planning-keywords kw)
        timestamp (planning-timestamp->ast (.planningTimestamp ctx))]
    (when (and tag timestamp)
      [:planning-info [:planning-keyword [tag]] timestamp])))

(defn- planning->ast [ctx]
  (let [infos (mapv planning-info->ast (.planningInfo ctx))]
    (if (and (seq infos) (every? some? infos))
      (with-span (into [:planning] infos) ctx)
      (failure :invalid-planning :planning (ctx-text ctx)))))

(defn- clock->ast [ctx]
  (let [kw (ctx-text (.clockKeyword ctx))
        range (timestamp-inactive-range->ast (.timestampInactiveRange ctx))]
    (if (or (not= "CLOCK" kw) (:failure? range))
      (failure :invalid-clock :clock (ctx-text ctx))
      (with-span [:clock
                  range
                  [:clock-duration
                   [:clock-dur-hh (ctx-text (.clockHours ctx))]
                   [:clock-dur-mm (ctx-text (.clockMinutes ctx))]]]
                 ctx))))

(defn- text-entity->ast [ctx]
  (with-span (cond-> [:text-entity [:entity-name (ctx-text (.entityName ctx))]]
               (.LBRACE ctx) (conj [:entity-braces]))
             ctx))

(defn- text-target->ast [ctx]
  (with-span [:text-target [:text-target-name (ctx-text (.textTargetName ctx))]] ctx))

(defn- text-sub->ast [ctx]
  (with-span
    (if-let [curly (.textSubCurlyBody ctx)]
      [:text-sub [:text-subsup-curly (ctx-text curly)]]
      [:text-sub [:text-subsup-word (ctx-text (.textSubWord ctx))]])
    ctx))

(defn- text-macro->ast [ctx]
  (let [args-ctx (.macroArgs ctx)
        args (if args-ctx
               (mapv ctx-text (.macroArg args-ctx))
               [""])]
    (with-span [:text-macro [:macro-name (ctx-text (.macroName ctx))] (into [:macro-args] args)] ctx)))

(defn- eol->ast [ctx]
  (with-span '() ctx))

(defn- horizontal-space->ast [ctx]
  (with-span '() ctx))

(defn- word->ast [ctx]
  (with-span [(ctx-text ctx)] ctx))

(defn- tags->ast [ctx]
  (let [tags (mapv ctx-text (.tagName ctx))]
    (if (seq tags)
      (with-span (into [:tags] tags) ctx)
      (failure :invalid-tags :tags (ctx-text ctx)))))

(defn- diary-sexp->ast [ctx]
  (let [raw (ctx-text (.diarySexpDirectBody ctx))
        normalized (if (str/starts-with? raw "(")
                     (subs raw 1)
                     raw)]
    (with-span [:diary-sexp normalized] ctx)))

(defn- affiliated-keyword-line->ast [ctx]
  (let [name (ctx-text (.affiliatedKeywordName ctx))
        opt (some-> ctx .affiliatedKeywordOption .affiliatedKeywordOptionText ctx-text)
        value (ctx-text (.affiliatedKeywordValue ctx))]
    (cond
      (contains? affiliated-keywords name)
      (with-span [:affiliated-keyword-line
                  (if opt
                    [:affil-kw-key name [:affil-kw-optional opt]]
                    [:affil-kw-key name])
                  [:kw-value value]]
                 ctx)

      (and (str/starts-with? name "ATTR_")
           (nil? opt)
           (re-matches #"[A-Za-z0-9-_]+" (subs name 5)))
      (with-span [:affiliated-keyword-line [:affil-kw-attr (subs name 5)] [:kw-value value]] ctx)

      :else
      (failure :invalid-affiliated-keyword-line :affiliated-keyword-line (ctx-text ctx)))))

(defn- list-item-line->ast [ctx]
  (let [marker-ctx (.listItemMarker ctx)]
    (with-span
      (cond-> (if-let [bullet-ctx (.listItemBullet marker-ctx)]
                [:list-item-line [:indent (ctx-text (.listItemIndent ctx))] [:list-item-bullet (ctx-text bullet-ctx)]]
                [:list-item-line
                 [:indent (ctx-text (.listItemIndent ctx))]
                 [:list-item-counter (ctx-text (.listItemCounter marker-ctx))]
                 [:list-item-counter-suffix (ctx-text (.listItemCounterSuffix marker-ctx))]])
        (.listItemCheckbox ctx) (conj [:list-item-checkbox [:list-item-checkbox-state (ctx-text (.listItemCheckboxState (.listItemCheckbox ctx)))]] )
        (.listItemTagSpec ctx) (conj [:list-item-tag (ctx-text (.listItemTagText (.listItemTagSpec ctx)))])
        true (conj (text-node (ctx-text (.listItemContents ctx)))))
      ctx)))

(defn- table-tableel-line->ast [ctx]
  (if-let [sep (.tableTableelSepLine ctx)]
    [:table-tableel-sep (ctx-text sep)]
    [:table-tableel-line (ctx-text (.tableTableelTextLine ctx))]))

(defn- table-org-line->ast [ctx]
  (cond
    (.tableFormulaLine ctx)
    (let [formula-ctx (.tableFormulaLine ctx)]
      (if (= "TBLFM" (ctx-text (.tableFormulaKeyword formula-ctx)))
        [:table-formula (or (some-> formula-ctx .tableFormulaTextOpt ctx-text) "")]
        (failure :invalid-table :table (ctx-text formula-ctx))))

    (.tableOrgSepLine ctx)
    [:table-row [:table-row-sep (ctx-text (.tableOrgSepLine ctx))]]

    :else
    [:table-row
     (into [:table-row-cells]
           (map (fn [cell] [:table-cell (ctx-text cell)])
                (.tableOrgCell (.tableOrgDataRow ctx))))]))

(defn- table->ast [ctx]
  (cond
    (.tableTableel ctx)
    (with-span [:table
                (into [:table-tableel]
                      (map table-tableel-line->ast (.tableTableelLine (.tableTableel ctx))))]
               ctx)

    (.tableOrg ctx)
    (let [rows (mapv table-org-line->ast (.tableOrgLine (.tableOrg ctx)))]
      (if (some :failure? rows)
        (failure :invalid-table :table (ctx-text ctx))
        (with-span [:table (into [:table-org] rows)] ctx)))

    :else
    (failure :invalid-table :table (ctx-text ctx))))

(defn- text-styled->ast [ctx]
  (let [raw (ctx-text ctx)
        delim (subs raw 0 1)
        inner (subs raw 1 (dec (count raw)))
        tag (get text-style-tags delim)]
    (if (and tag
             (not= inner "")
             (or (not (contains? code-like-style-tags tag))
                 (and (not (str/starts-with? inner " "))
                      (not (str/ends-with? inner " "))
                      (not (re-find #"\S[=~]\s" inner)))))
      (with-span [[tag inner]] ctx)
      (failure :invalid-text-styled :text-styled raw))))

(defn- link-target->ast [ctx]
  (cond
    (.linkTargetId ctx)
    [:link [:link-ext [:link-ext-id (ctx-text (.linkIdValue (.linkTargetId ctx)))]]]

    (.linkTargetExtOther ctx)
    (let [other (.linkTargetExtOther ctx)]
      (let [raw (ctx-text other)
            scheme (or (some-> other .linkUrlScheme ctx-text)
                       (some->> (re-matches #"^([^:]+):(.*)$" raw) second))
            rest (or (some-> other .linkTargetRest ctx-text)
                     (some->> (re-matches #"^[^:]+:(.*)$" raw) second))]
        [:link [:link-ext [:link-ext-other
                           [:link-url-scheme scheme]
                           [:link-url-rest (or rest "")]]]]))

    (.linkTargetIntCustomId ctx)
    [:link [:link-int [:link-file-loc-customid (ctx-text (.linkTargetIntBody (.linkTargetIntCustomId ctx)))]]]

    (.linkTargetIntHeadline ctx)
    [:link [:link-int [:link-file-loc-headline (ctx-text (.linkTargetIntBody (.linkTargetIntHeadline ctx)))]]]

    (.linkTargetIntString ctx)
    [:link [:link-int [:link-file-loc-string (ctx-text (.linkTargetIntBody (.linkTargetIntString ctx)))]]]

    :else nil))

(defn- link-format->ast [ctx]
  (let [link-node (link-target->ast (.linkTarget ctx))
        desc (some-> ctx .linkDescriptionRaw ctx-text)]
    (if link-node
      (with-span (cond-> [:link-format link-node]
                   (some? desc) (conj [:link-description desc]))
                 ctx)
      (failure :invalid-link-format :link-format (ctx-text ctx)))))

(defn- text-sup->ast [ctx]
  (with-span
    (if-let [curly (.textSubCurlyBody ctx)]
      [:text-sup [:text-subsup-curly (ctx-text curly)]]
      [:text-sup [:text-subsup-word (ctx-text (.textSupWord ctx))]])
    ctx))

(defn- text-radio-target->ast [ctx]
  (with-span [:text-radio-target [:text-target-name (ctx-text (.textRadioTargetBody ctx))]] ctx))

(defn- text-part->ast [ctx]
  (cond
    (.timestamp ctx)
    (timestamp->ast (.timestamp ctx))

    (.linkFormat ctx)
    (link-format->ast (.linkFormat ctx))

    (.footnoteLink ctx)
    (footnote-link->ast (.footnoteLink ctx))

    (.textRadioTarget ctx)
    (text-radio-target->ast (.textRadioTarget ctx))

    (.textTarget ctx)
    (text-target->ast (.textTarget ctx))

    (.textLink ctx)
    (text-link->ast (.textLink ctx))

    (.textStyled ctx)
    (first (text-styled->ast (.textStyled ctx)))

    (.textEntity ctx)
    (text-entity->ast (.textEntity ctx))

    (.textSub ctx)
    (text-sub->ast (.textSub ctx))

    (.textSup ctx)
    (text-sup->ast (.textSup ctx))

    (.textMacro ctx)
    (text-macro->ast (.textMacro ctx))

    (.textPlain ctx)
    [:text-normal (ctx-text (.textPlain ctx))]

    (.textFallbackChar ctx)
    [:text-normal (ctx-text (.textFallbackChar ctx))]

    :else nil))

(defn- text-linebreak->ast [ctx]
  [:text-linebreak [:text-linebreak-after (or (some-> ctx .textLinebreakAfter ctx-text) "")]])

(defn- merge-text-normal-nodes [nodes]
  (reduce (fn [acc node]
            (if (and (= :text-normal (first node))
                     (= :text-normal (first (peek acc))))
              (conj (pop acc) [:text-normal (str (second (peek acc)) (second node))])
              (conj acc node)))
          []
          nodes))

(defn- text->ast [ctx]
  (let [segments (mapv text-part->ast (.textSegment ctx))
        linebreak (some-> ctx .textLinebreak text-linebreak->ast)]
    (if (or (some nil? segments)
            (and linebreak (empty? segments) (not (.textLinebreak ctx))))
      (failure :invalid-text :text (ctx-text ctx))
      (with-span (into [:text] (merge-text-normal-nodes (cond-> segments linebreak (conj linebreak))))
                 ctx))))

(defn- noparse-block->ast [ctx]
  (let [begin-ctx (.noparseBlockBeginLine ctx)
        end-ctx (.noparseBlockEndLine ctx)
        begin-marker (some-> begin-ctx .noparseBeginMarker ctx-text str/lower-case)
        end-marker (some-> end-ctx .noparseEndMarker ctx-text str/lower-case)
        begin-name (some-> begin-ctx .noparseBlockName ctx-text str/lower-case)
        end-name (some-> end-ctx .noparseEndName ctx-text str/lower-case)
        params (some-> begin-ctx .noparseBlockParameters ctx-text)
        content (or (some-> ctx .noparseBlockContent ctx-text) "")]
    (if (and (= "begin" begin-marker)
             (= "end" end-marker)
             (contains? noparse-block-names begin-name)
             (seq end-name))
      (with-span (cond-> [:noparse-block
                          (cond-> [:noparse-block-begin-line [:block-name-noparse begin-name]]
                            (and params (not= params "")) (conj [:block-parameters params]))
                          [:noparse-block-content content]
                          [:block-end-line [:block-name end-name]]]
                   true identity)
                 ctx)
      (failure :invalid-noparse-block :noparse-block (ctx-text ctx)))))

(def direct-starts
  #{:eol :s :word :tags :diary-sexp :affiliated-keyword-line :node-property-line
    :property-drawer :noparse-block :fixed-width-line :fixed-width-area :link-ext-other
    :link-ext-id :link-ext-file :text-styled :text-link :link-format :text-sup
    :text-radio-target :footnote-link :footnote-line :ts-time :timestamp-inactive-range
    :clock :list-item-line :table :timestamp :text-entity :text-target :text-sub
    :text-macro :planning :text})

(defn parse-direct [parse-antlr-only raw start]
  (when (contains? direct-starts start)
    (parse-antlr-only raw start)))

(defn- headline->ast [ctx parse-direct raw]
  (let [stars (.getText (.stars ctx))
        keyword-ctx (.keyword ctx)
        priority-ctx (.priority ctx)
        comment-token-ctx (.commentToken ctx)
        title-txt (some-> ctx .title .getText)
        base [:headline [:stars stars]]
        with-keyword (if keyword-ctx
                       (conj base [:keyword (.getText keyword-ctx)])
                       base)
        with-priority (if priority-ctx
                        (conj with-keyword [:priority (.getText (.UPPER priority-ctx))])
                        with-keyword)
        with-comment-token (if comment-token-ctx
                             (conj with-priority [:comment-token])
                             with-priority)]
    (with-span (conj with-comment-token
                     (let [parsed-title (text->ast (.text (.title ctx)))]
                       (if (:failure? parsed-title)
                         (text-node title-txt)
                         parsed-title)))
                ctx)))

(defn- content-line->ast [ctx]
  (let [raw (.getText ctx)
        parsed-text (text->ast (.text ctx))
        text-ast (if (:failure? parsed-text)
                   (text-node raw)
                   parsed-text)]
    (with-span [:content-line text-ast] ctx)))

(defn- drawer-begin-line->ast [ctx]
  (with-span [:drawer-begin-line [:drawer-name (.getText (.drawerName ctx))]] ctx))

(defn- drawer-end-line->ast [ctx]
  (with-span [:drawer-end-line] ctx))

(defn- comment-line->ast [ctx]
  (let [raw (.getText ctx)
        hash-pos (.indexOf raw "#")
        head (subs raw 0 (inc hash-pos))
        rest (subs raw (inc hash-pos))]
    (if (re-matches #"[\t ]*#( |$).*" raw)
      (with-span [:comment-line [:comment-line-head head] [:comment-line-rest rest]] ctx)
      {:failure? true
       :backend :antlr
       :reason :invalid-comment-line
       :raw raw})))

(defn- horizontal-rule->ast [ctx]
  (with-span [:horizontal-rule (.getText ctx)] ctx))

(defn- todo-line->ast [ctx]
  (let [kw (.getText (.todoKeyword ctx))]
    (if (= "TODO" kw)
      (with-span (into [:todo-line]
                       (concat
                        (map #(vector :todo-state (.getText %)) (.todoState ctx))
                        (map #(vector :done-state (.getText %)) (.doneState ctx))))
                 ctx)
      {:failure? true
       :backend :antlr
       :reason :invalid-todo-keyword
       :raw (.getText ctx)})))

(defn- block-begin-line->ast [ctx]
  (let [marker (.getText (.blockBeginMarker ctx))
        name (.getText (.blockName ctx))
        marker* (str/upper-case marker)
        params-ctx (.blockParameters ctx)]
    (cond
      (= "BEGIN" marker*)
      (with-span (cond-> [:block-begin-line [:block-name name]]
                   params-ctx (conj [:block-parameters (.getText params-ctx)]))
                 ctx)

      (= "END" marker*)
      (with-span [:block-end-line [:block-name name]] ctx)

      :else
      {:failure? true
       :backend :antlr
       :reason :invalid-block-begin-marker
       :raw (.getText ctx)})))

(defn- block-end-line->ast [ctx]
  (let [marker (.getText (.blockEndMarker ctx))
        name (.getText (.blockName ctx))]
    (if (= "END" (str/upper-case marker))
      (with-span [:block-end-line [:block-name name]] ctx)
      {:failure? true
       :backend :antlr
       :reason :invalid-block-end-marker
       :raw (.getText ctx)})))

(defn- dynamic-block-begin-line->ast [ctx]
  (let [name (.getText (.dynamicBlockName ctx))
        params-ctx (.dynamicBlockParameters ctx)]
    (with-span (cond-> [:dynamic-block-begin-line [:dynamic-block-name name]]
                 params-ctx (conj [:dynamic-block-parameters (.getText params-ctx)]))
               ctx)))

(defn- dynamic-block-end-line->ast [ctx]
  (with-span [:dynamic-block-end-line] ctx))

(defn- fn-prefix-valid? [ctx]
  (= "fn:" (str/lower-case (.getText ctx))))

(defn- footnote-line->ast [ctx]
  (if (fn-prefix-valid? (.fnPrefix ctx))
    (with-span [:footnote-line
                [:fn-label (.getText (.fnLabel ctx))]
                (text-node (.getText (.text ctx)))]
               ctx)
    {:failure? true
     :backend :antlr
     :reason :invalid-footnote-prefix
     :raw (.getText ctx)}))

(defn- footnote-link->ast [ctx]
  (if (fn-prefix-valid? (.fnPrefix ctx))
    (let [label-ctx (.fnLabel ctx)
          inline-ctx (.fnTextInline ctx)
          base [:footnote-link]
          with-label (if label-ctx
                       (conj base [:fn-label (.getText label-ctx)])
                       base)
          with-inline (if inline-ctx
                        (conj with-label (.getText inline-ctx))
                        with-label)]
      (with-span with-inline ctx))
    {:failure? true
     :backend :antlr
     :reason :invalid-footnote-prefix
     :raw (.getText ctx)}))

(defn- other-keyword-line->ast [ctx]
  (let [name (.getText (.kwName ctx))
        value-ctx (.kwValue ctx)]
    (with-span (cond-> [:other-keyword-line [:kw-name name]]
                 value-ctx (conj [:kw-value (.getText value-ctx)]))
               ctx)))

(defn- line->ast [ctx parse-direct raw]
  (cond
    (.headline ctx) (headline->ast (.headline ctx) parse-direct raw)
    (.todoLine ctx) (todo-line->ast (.todoLine ctx))
    (.blockEndLine ctx) (block-end-line->ast (.blockEndLine ctx))
    (.blockBeginLine ctx) (block-begin-line->ast (.blockBeginLine ctx))
    (.otherKeywordLine ctx) (other-keyword-line->ast (.otherKeywordLine ctx))
    (.dynamicBlockEndLine ctx) (dynamic-block-end-line->ast (.dynamicBlockEndLine ctx))
    (.dynamicBlockBeginLine ctx) (dynamic-block-begin-line->ast (.dynamicBlockBeginLine ctx))
    (.footnoteLine ctx) (footnote-line->ast (.footnoteLine ctx))
    (.commentLine ctx) (comment-line->ast (.commentLine ctx))
    (.horizontalRule ctx) (horizontal-rule->ast (.horizontalRule ctx))
    (.drawerBeginLine ctx) (drawer-begin-line->ast (.drawerBeginLine ctx))
    (.drawerEndLine ctx) (drawer-end-line->ast (.drawerEndLine ctx))
    (.contentLine ctx) (content-line->ast (.contentLine ctx))
    (.emptyLine ctx) (with-span [:empty-line] (.emptyLine ctx))
    :else nil))

(defn- s->ast [ctx parse-direct raw]
  (let [from-lines (map (fn [line-ctx]
                          (if-let [line (.line line-ctx)]
                            (line->ast line parse-direct raw)
                            (with-span [:empty-line] line-ctx)))
                        (.lineWithNl ctx))
        eof-line (some-> ctx .lineAtEof .line (#(line->ast % parse-direct raw)))
        all-lines (if eof-line
                    (concat from-lines [eof-line])
                    from-lines)]
    (with-span (into [:S] (remove nil? all-lines)) ctx)))

(defn parse-result [parser raw start parse-direct eof?]
  (let [result (case start
                 :S (-> (s->ast (.s parser) parse-direct raw)
                        (postprocess/attach-headline-planning parse-direct raw)
                        (postprocess/collapse-table-lines parse-direct raw)
                        (postprocess/strip-nested-s-lines))
                 :s (horizontal-space->ast (.horizontalSpace (.horizontalSpaceEof parser)))
                 :drawer (-> parser .s (#(s->ast % parse-direct raw)) postprocess/drawer-from-s)
                 :dynamic-block (-> parser .s (#(s->ast % parse-direct raw)) postprocess/dynamic-block-from-s)
                 :block (let [np (parse-direct raw :noparse-block)]
                          (if (and np (not (:failure? np)))
                            (with-meta [:block np]
                              (or (meta np) {}))
                            (-> parser .s (#(s->ast % parse-direct raw)) postprocess/block-from-s)))
                 :headline (let [s-ast (-> (s->ast (.s parser) parse-direct raw)
                                           (postprocess/attach-headline-planning parse-direct raw))
                                 lines (rest s-ast)]
                             (if (and (= 1 (count lines))
                                      (= :headline (first (first lines))))
                               (with-meta (first lines)
                                 (merge (meta (first lines))
                                        {:span [0 (count raw)]}))
                               (failure :invalid-headline start raw)))
                 :todo-line (todo-line->ast (.todoLine (.todoLineEof parser)))
                 :eol (eol->ast (.eol (.eolEof parser)))
                 :word (word->ast (.word (.wordEof parser)))
                 :block-begin-line (block-begin-line->ast (.blockBeginLine (.blockBeginLineEof parser)))
                 :block-end-line (block-end-line->ast (.blockEndLine (.blockEndLineEof parser)))
                 :dynamic-block-begin-line (dynamic-block-begin-line->ast (.dynamicBlockBeginLine (.dynamicBlockBeginLineEof parser)))
                 :dynamic-block-end-line (dynamic-block-end-line->ast (.dynamicBlockEndLine (.dynamicBlockEndLineEof parser)))
                 :other-keyword-line (other-keyword-line->ast (.otherKeywordLine (.otherKeywordLineEof parser)))
                 :footnote-line (footnote-line->ast (.footnoteLine (.footnoteLineEof parser)))
                 :footnote-link (footnote-link->ast (.footnoteLink (.footnoteLinkEof parser)))
                 :comment-line (comment-line->ast (.commentLine (.commentLineEof parser)))
                 :horizontal-rule (horizontal-rule->ast (.horizontalRule (.horizontalRuleEof parser)))
                 :content-line (content-line->ast (.contentLine parser))
                 :line (let [line-ast (line->ast (.line (.lineEof parser)) parse-direct raw)]
                         (if (:failure? line-ast)
                           line-ast
                           (with-meta [line-ast]
                             (or (meta line-ast) {}))))
                 :drawer-begin-line (drawer-begin-line->ast (.drawerBeginLine (.drawerBeginLineEof parser)))
                 :drawer-end-line (drawer-end-line->ast (.drawerEndLine (.drawerEndLineEof parser)))
                 :tags (tags->ast (.tags (.tagsEof parser)))
                 :diary-sexp (diary-sexp->ast (.diarySexp (.diarySexpEof parser)))
                 :affiliated-keyword-line (affiliated-keyword-line->ast (.affiliatedKeywordLine (.affiliatedKeywordLineEof parser)))
                 :node-property-line (node-property-line->ast (.nodePropertyLine (.nodePropertyLineEof parser)))
                 :property-drawer (property-drawer->ast (.propertyDrawer (.propertyDrawerEof parser)))
                 :fixed-width-line (fixed-width-line->ast (.fixedWidthLine (.fixedWidthLineEof parser)))
                 :fixed-width-area (fixed-width-area->ast (.fixedWidthArea (.fixedWidthAreaEof parser)))
                 :link-ext-other (link-ext-other->ast (.linkExtOther (.linkExtOtherEof parser)))
                 :link-ext-id (link-ext-id->ast (.linkExtId (.linkExtIdEof parser)))
                 :link-ext-file (link-ext-file->ast (.linkExtFile (.linkExtFileEof parser)))
                 :text-link (text-link->ast (.textLink (.textLinkEof parser)))
                 :ts-time (ts-time->ast (.tsTime (.tsTimeEof parser)))
                 :timestamp-inactive-range (timestamp-inactive-range->ast (.timestampInactiveRange (.timestampInactiveRangeEof parser)))
                 :timestamp (timestamp->ast (.timestamp (.timestampEof parser)))
                 :clock (clock->ast (.clock (.clockEof parser)))
                 :planning (planning->ast (.planning (.planningEof parser)))
                 :list-item-line (list-item-line->ast (.listItemLine (.listItemLineEof parser)))
                 :table (table->ast (.table (.tableEof parser)))
                 :text-styled (text-styled->ast (.textStyled (.textStyledEof parser)))
                 :link-format (link-format->ast (.linkFormat (.linkFormatEof parser)))
                 :text-sup (text-sup->ast (.textSup (.textSupEof parser)))
                 :text-radio-target (text-radio-target->ast (.textRadioTarget (.textRadioTargetEof parser)))
                 :noparse-block (noparse-block->ast (.noparseBlock (.noparseBlockEof parser)))
                 :text-entity (text-entity->ast (.textEntity (.textEntityEof parser)))
                 :text (text->ast (.text (.textEof parser)))
                 :text-target (text-target->ast (.textTarget (.textTargetEof parser)))
                 :text-sub (text-sub->ast (.textSub (.textSubEof parser)))
                 :text-macro (text-macro->ast (.textMacro (.textMacroEof parser)))
                 (failure :unsupported-start start raw))]
    (if (or (:failure? result)
            (eof? parser)
            (contains? #{:block :headline} start))
      result
      (failure :unconsumed-input start raw))))
