(ns org-parser.antlr.parser
  (:require [clojure.string :as str]
            [org-parser.antlr.postprocess :as postprocess])
  (:import [org.antlr.v4.runtime BaseErrorListener BailErrorStrategy CharStreams CommonTokenStream]
           [org.antlr.v4.runtime.misc ParseCancellationException]
           [org.antlr.v4.runtime Token]
           [clojure.lang Reflector]))

(def ^:private lexer-class-name "org_parser.antlr.OrgLexer")
(def ^:private parser-class-name "org_parser.antlr.OrgParser")

(declare parse-direct parse-antlr-only)

(defn- load-class [class-name]
  (Class/forName class-name))

(defn- instantiate [class-name & args]
  (Reflector/invokeConstructor (load-class class-name) (to-array args)))

(defn- text-node [s]
  [:text [:text-normal s]])

(defn- ctx-span [ctx]
  (let [start-token (.getStart ctx)
        stop-token (.getStop ctx)
        start-idx (.getStartIndex start-token)
        stop-idx (if stop-token (.getStopIndex stop-token) start-idx)]
    [start-idx (inc stop-idx)]))

(defn- with-span [node ctx]
  (with-meta node {:span (ctx-span ctx)}))

(defn- with-raw-span [node raw]
  (with-meta node {:span [0 (count raw)]}))

(defn- failure [reason start raw]
  {:failure? true
   :backend :antlr
   :reason reason
   :start start
   :raw raw})

(def ^:private planning-keywords
  {"SCHEDULED" :planning-kw-scheduled
   "DEADLINE" :planning-kw-deadline
   "CLOSED" :planning-kw-closed})

(def ^:private ts-mod-units
  #{"h" "d" "w" "m" "y"})

(defn- ctx-text [ctx]
  (some-> ctx .getText))

(defn- valid-ts-time? [raw]
  (boolean (re-matches #"\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?" raw)))

(defn- valid-ts-unit? [unit]
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
  (with-span (cond-> [:link-ext-other [:link-url-scheme (ctx-text (.linkUrlScheme ctx))]]
               (.linkUrlRest ctx) (conj [:link-url-rest (ctx-text (.linkUrlRest ctx))]))
             ctx))

(defn- link-ext-id->ast [ctx]
  (if (= "id" (ctx-text (.linkIdPrefix ctx)))
    (with-span [:link-ext-id (ctx-text (.linkIdValue ctx))] ctx)
    (failure :invalid-link-ext-id :link-ext-id (ctx-text ctx))))

(defn- link-file-location-node [loc]
  (cond
    (re-matches #"[0-9]+" loc) [:link-file-loc-lnum loc]
    (str/starts-with? loc "*") [:link-file-loc-headline (subs loc 1)]
    (str/starts-with? loc "#") [:link-file-loc-customid (subs loc 1)]
    :else [:link-file-loc-string loc]))

(defn- link-ext-file->ast [ctx]
  (let [scheme (some-> ctx .linkFileScheme ctx-text)
        path (ctx-text (.linkFilePath ctx))
        loc (some-> ctx .linkFileLocation ctx-text)]
    (if (and (seq path)
             (or (nil? scheme) (= "file:" (str/lower-case scheme))))
      (with-span (cond-> [:link-ext-file path]
                   loc (conj (link-file-location-node loc)))
                 ctx)
      (failure :invalid-link-ext-file :link-ext-file (ctx-text ctx)))))

(defn- text-link->ast [ctx]
  (let [link (link-ext-other->ast (.linkExtOther ctx))
        raw (ctx-text ctx)]
    (if (:failure? link)
      link
      (with-span
        [:text-link
         (if (str/starts-with? raw "<")
           [:text-link-angle
            [:link-url-scheme (ctx-text (.linkUrlScheme (.linkExtOther ctx)))]
            [:text-link-angle-path (or (some-> (.linkExtOther ctx) .linkUrlRest ctx-text) "")]]
           [:text-link-plain
            [:link-url-scheme (ctx-text (.linkUrlScheme (.linkExtOther ctx)))]
            [:text-link-plain-path (or (some-> (.linkExtOther ctx) .linkUrlRest ctx-text) "")]])]
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

(defn- parse-node-property-line-direct [s]
  (if-let [[_ name plus value] (re-matches #":(?!END:)([^\s:+]+)(\+)?:(?: (.*))?" s)]
    (with-raw-span
      (cond-> [:node-property-line [:node-property-name name]]
        plus (conj [:node-property-plus])
        (and (some? value) (not= value "")) (conj [:node-property-value (text-node value)]))
      s)
    (failure :invalid-node-property-line :node-property-line s)))

(defn- parse-noparse-block-direct [s]
  (let [[_ begin-line remainder] (re-matches #"(?s)^([^\n]*)\n(.*)$" s)
        [_ _ name params] (when begin-line
                             (re-matches #"[\t ]*#\+(BEGIN|begin)_([^\s]+)(?:[\t ]+(.*))?" begin-line))
        noparse-name? (contains? #{"src" "example" "export" "comment"}
                                 (some-> name str/lower-case))
        end-only (when remainder
                   (re-matches #"[\t ]*#\+(END|end)_([^\s]+)[\t ]*" remainder))
        with-content (when (and remainder (not end-only))
                       (re-matches #"(?s)^(.*\n)([\t ]*#\+(END|end)_([^\s]+)[\t ]*)$" remainder))
        content (cond
                  end-only ""
                  with-content (nth with-content 1)
                  :else nil)
        end-name (cond
                   end-only (nth end-only 2)
                   with-content (nth with-content 4)
                   :else nil)]
    (when (and noparse-name? (some? content) end-name)
      (let [noparse-name (str/lower-case name)
            begin-node (cond-> [:noparse-block-begin-line [:block-name-noparse noparse-name]]
                         (and (some? params) (not= params "")) (conj [:block-parameters params]))
            content-node [:noparse-block-content content]
            end-node [:block-end-line [:block-name end-name]]]
        (with-raw-span [:noparse-block begin-node content-node end-node] s)))))

(defn- ts-inner-w-time-node [date day time]
  [:ts-inner-w-time [:ts-date date] [:ts-day day] [:ts-time time]])

(defn- parse-list-item-line-direct [raw]
  (when-let [[_ indent rest] (re-matches #"([\t ]*)(.*)" raw)]
    (letfn [(tail->nodes [base tail]
              (let [[_ cb tail2] (or (re-matches #"\[([ \-X])\] (.+)" tail)
                                     [nil nil tail])
                    [_ tag contents] (or (re-matches #"(.*?) :: (.*)" tail2)
                                         [nil nil tail2])]
                (cond-> base
                  cb (conj [:list-item-checkbox [:list-item-checkbox-state cb]])
                  tag (conj [:list-item-tag tag])
                  true (conj (text-node contents)))))]
      (or
       (when-let [[_ bullet tail] (re-matches #"([*+\-]) (.+)" rest)]
         (tail->nodes [:list-item-line [:indent indent] [:list-item-bullet bullet]] tail))
       (when-let [[_ counter suffix tail] (re-matches #"([0-9A-Za-z])([\.)]) (.+)" rest)]
         (tail->nodes [:list-item-line [:indent indent] [:list-item-counter counter] [:list-item-counter-suffix suffix]] tail))
       nil))))

(defn- parse-table-direct [raw]
  (let [lines (->> (str/split raw #"\r?\n")
                   (remove #(= % ""))
                   vec)
        trimmed (mapv str/trim lines)]
    (cond
      (and (seq trimmed)
           (every? #(or (re-matches #"\+[-+]+\+" %)
                        (re-matches #"\|.*\|" %))
                   trimmed)
           (some #(re-matches #"\+[-+]+\+" %) trimmed))
      [:table
       (into [:table-tableel]
             (mapv (fn [line]
                     (if (re-matches #"\+[-+]+\+" line)
                       [:table-tableel-sep line]
                       [:table-tableel-line line]))
                   trimmed))]

      (and (seq trimmed)
           (every? #(or (re-matches #"\|.*\|" %)
                        (re-matches #"#\+TBLFM: .*" %))
                   trimmed))
      (let [rows (mapv (fn [line]
                         (cond
                           (re-matches #"#\+TBLFM: .*" line)
                           [:table-formula (subs line 9)]

                           (re-matches #"\|[-+]+\|" line)
                           [:table-row [:table-row-sep line]]

                           :else
                           (let [inside (subs line 1 (dec (count line)))
                                 cells (str/split inside #"\|" -1)]
                             [:table-row (into [:table-row-cells] (map (fn [c] [:table-cell c]) cells))])))
                       trimmed)]
        [:table (into [:table-org] rows)])

      :else nil)))

(defn- parse-text-styled-direct [raw]
  (let [styles {"*" :text-sty-bold
                "/" :text-sty-italic
                "_" :text-sty-underlined
                "=" :text-sty-verbatim
                "~" :text-sty-code
                "+" :text-sty-strikethrough}
        delim (subs raw 0 1)
        tag (get styles delim)]
    (when tag
      (when-let [[_ inner] (re-matches (re-pattern (str "^" (java.util.regex.Pattern/quote delim) "(.+)" (java.util.regex.Pattern/quote delim) "$")) raw)]
        (when (and (not= inner "")
                   (or (not (contains? #{:text-sty-verbatim :text-sty-code} tag))
                       (and (not (str/starts-with? inner " "))
                            (not (str/ends-with? inner " "))
                            (not (re-find #"\S[=~]\s" inner)))))
          [(vector tag inner)])))))

(defn- parse-text-link-direct [raw]
  (or
   (when-let [[_ scheme path] (re-matches #"<([a-zA-Z][a-zA-Z0-9+.-]*):(.*)>" raw)]
     [:text-link [:text-link-angle [:link-url-scheme scheme] [:text-link-angle-path path]]])
   (when-let [[_ scheme path] (re-matches #"([a-zA-Z][a-zA-Z0-9+.-]*):(.*)" raw)]
     [:text-link [:text-link-plain [:link-url-scheme scheme] [:text-link-plain-path path]]])
   nil))

(defn- parse-link-target-node [target]
  (cond
    (or (re-find #"(?<!\\)\[" target)
        (= target "\\"))
    nil

    (re-matches #"id:[a-zA-Z0-9][a-zA-Z0-9-]+" target)
    [:link [:link-ext [:link-ext-id (subs target 3)]]]

    (re-matches #"[a-zA-Z][a-zA-Z0-9+.-]*:.*" target)
    (when-let [[_ scheme rest] (re-matches #"([a-zA-Z][a-zA-Z0-9+.-]*):(.*)" target)]
      [:link [:link-ext [:link-ext-other [:link-url-scheme scheme] [:link-url-rest rest]]]])

    (str/starts-with? target "#")
    [:link [:link-int [:link-file-loc-customid (subs target 1)]]]

    (str/starts-with? target "*")
    [:link [:link-int [:link-file-loc-headline (subs target 1)]]]

    :else
    [:link [:link-int [:link-file-loc-string target]]]))

(defn- parse-link-format-direct [raw]
  (when (and (str/starts-with? raw "[[")
             (str/ends-with? raw "]]"))
    (let [inside (subs raw 2 (- (count raw) 2))
          split-at (.indexOf inside "][")
          [target desc] (if (neg? split-at)
                          [inside nil]
                          [(subs inside 0 split-at)
                           (subs inside (+ split-at 2))])]
      (when-let [link-node (parse-link-target-node target)]
        (cond-> [:link-format link-node]
          (some? desc) (conj [:link-description desc]))))))



(defn- parse-ts-modifier-token [token]
  (when-let [[_ typ val unit at-least-val at-least-unit]
             (re-matches #"(\+\+|\+|\.\+|--|-)(\d+)([hdwmy])(?:/(\d+)([hdwmy]))?" token)]
    (let [base [(if (contains? #{"+" "++" ".+"} typ) :ts-repeater :ts-warning)
                [(if (contains? #{"+" "++" ".+"} typ) :ts-repeater-type :ts-warning-type) typ]
                [:ts-mod-value val]
                [:ts-mod-unit unit]]]
      (if at-least-val
        (conj base [:ts-mod-at-least [:ts-mod-value at-least-val] [:ts-mod-unit at-least-unit]])
        base))))

(defn- parse-ts-modifiers [mods-raw]
  (if (or (nil? mods-raw) (= "" (str/trim mods-raw)))
    []
    (let [tokens (str/split (str/trim mods-raw) #"[ \t]+")
          nodes (mapv parse-ts-modifier-token tokens)]
      (when (every? some? nodes)
        nodes))))

(defn- ts-inner-simple
  ([date day time]
   (ts-inner-simple date day time []))
  ([date day time modifiers]
   (if time
     [:ts-inner
      (cond-> [:ts-inner-w-time [:ts-date date]]
        day (conj [:ts-day day])
        true (conj [:ts-time time]))
      (into [:ts-modifiers] modifiers)]
     [:ts-inner
      (cond-> [:ts-inner-wo-time [:ts-date date]]
        day (conj [:ts-day day]))
      (into [:ts-modifiers] modifiers)])))

(defn- parse-planning-ts [ts]
  (or
   (when-let [[_ d day time mods] (re-matches #"\[(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?(?:[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?))?(?:[ \t]+(.+))?\]" ts)]
     (when-let [mod-nodes (parse-ts-modifiers mods)]
       [:timestamp [:timestamp-inactive (ts-inner-simple d day time mod-nodes)]]))
   (when-let [[_ d day time mods] (re-matches #"<(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?(?:[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?))?(?:[ \t]+(.+))?>" ts)]
     (when-let [mod-nodes (parse-ts-modifiers mods)]
       [:timestamp [:timestamp-active (ts-inner-simple d day time mod-nodes)]]))
   nil))

(defn- parse-simple-timestamp-point [raw]
  (or
   (when-let [[_ d day time mods] (re-matches #"<(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?(?:[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?))?(?:[ \t]+(.+))?>" raw)]
     (when-let [mod-nodes (parse-ts-modifiers mods)]
       [:timestamp [:timestamp-active (ts-inner-simple d day time mod-nodes)]]))
    (when-let [[_ d day time mods] (re-matches #"\[(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?(?:[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?))?(?:[ \t]+(.+))?\]" raw)]
      (when-let [mod-nodes (parse-ts-modifiers mods)]
        [:timestamp [:timestamp-inactive (ts-inner-simple d day time mod-nodes)]]))
    nil))

(defn- parse-ts-inner-node [raw]
  (or
   (when-let [[_ d day time mods] (re-matches #"(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?(?:[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?))?(?:[ \t]+(.+))?" raw)]
     (when-let [mod-nodes (parse-ts-modifiers mods)]
       (ts-inner-simple d day time mod-nodes)))
   nil))

(defn- parse-inline-chunk [raw]
  (letfn [(ok [v]
            (when (and v (not (:failure? v))) v))]
    (or
     (when-let [[_ v] (re-matches #"\{\{\{([A-Za-z0-9_]+\(.*\))\}\}\}" raw)]
       (ok (parse-direct (str "{{{" v "}}}") :text-macro)))
     (ok (parse-direct raw :link-format))
     (ok (parse-direct raw :footnote-link))
     (ok (parse-direct raw :text-link))
     (ok (parse-direct raw :text-target))
     (ok (parse-direct raw :text-entity))
     (ok (parse-direct raw :text-sub))
     (when (str/starts-with? raw "^")
       (or
        (when-let [[_ v] (re-matches #"\^\{([^{}]+)\}" raw)]
          [:text-sup [:text-subsup-curly v]])
        (when-let [[_ v] (re-matches #"\^([^\s]+)" raw)]
          [:text-sup [:text-subsup-word v]])))
     nil)))

(defn- parse-text-direct [raw]
  (if (or (str/starts-with? raw "\n")
          (str/starts-with? raw "\r"))
    (failure :invalid-text :text raw)
    (let [n (count raw)]
      (loop [i 0
             normal ""
             out []]
        (if (>= i n)
          (let [nodes (cond-> out
                        (not= normal "") (conj [:text-normal normal]))]
            (with-raw-span (into [:text] nodes)
                           raw))
          (let [rest (subs raw i)
                styled-cand (some (fn [d]
                                    (let [j (.indexOf rest d 1)]
                                      (when (pos? j)
                                        (let [cand (subs rest 0 (inc j))]
                                          (when-let [s (parse-text-styled-direct cand)]
                                            [cand s])))))
                                  ["*" "/" "_" "=" "~" "+"])
                macro-end (.indexOf rest "}}}")
                macro-cand (when (and (str/starts-with? rest "{{{") (<= 0 macro-end))
                             (subs rest 0 (+ macro-end 3)))
                radio-end (.indexOf rest ">>>")
                radio-cand (when (and (str/starts-with? rest "<<<") (<= 0 radio-end))
                             (subs rest 0 (+ radio-end 3)))
                target-end (.indexOf rest ">>")
                target-cand (when (and (str/starts-with? rest "<<") (<= 0 target-end))
                              (subs rest 0 (+ target-end 2)))
                angled-end (.indexOf rest ">")
                angled-cand (when (and (str/starts-with? rest "<") (<= 0 angled-end))
                              (subs rest 0 (inc angled-end)))
                link-second (.indexOf rest "]]" 2)
                link-cand (when (and (str/starts-with? rest "[[") (<= 0 link-second))
                            (let [base (subs rest 0 (+ link-second 2))
                                  after (subs rest (+ link-second 2))]
                              (if (and (str/starts-with? after "[")
                                       (not (str/starts-with? after "[fn:")))
                                (let [desc-end (.indexOf after "]")]
                                  (if (<= 0 desc-end)
                                    (str base (subs after 0 (inc desc-end)))
                                    base))
                                base)))
                plain-url-cand (when-let [[_ u] (re-find #"^((?:https?|ftp|file):[^\s<>\[\]()]+|mailto:[^\s<>\[\]()]+)" rest)]
                                 u)
                foot-cand (when (str/starts-with? rest "[fn:")
                            (let [end (.indexOf rest "]")]
                              (when (<= 0 end) (subs rest 0 (inc end)))))
                entity-cand (when (str/starts-with? rest "\\")
                              (when-let [[_ n braces] (re-find #"^\\([A-Za-z]+)(\{\})?" rest)]
                                (str "\\" n (or braces ""))))
                sub-cand (or (when-let [[_ v] (re-find #"^_\{([^{}]+)\}" rest)] (str "_{" v "}"))
                             (when-let [[_ v] (re-find #"^_([^\s]+)" rest)] (str "_" v)))
                sup-cand (or (when-let [[_ v] (re-find #"^\^\{([^{}]+)\}" rest)] (str "^{" v "}"))
                             (when-let [[_ v] (re-find #"^\^([^\s_]+)" rest)] (str "^" v)))
                ts-cand (or
                         (when (str/starts-with? rest "<")
                           (let [e (.indexOf rest ">")]
                             (when (<= 0 e) (subs rest 0 (inc e)))))
                         (when (str/starts-with? rest "[")
                           (let [e (.indexOf rest "]")]
                             (when (<= 0 e) (subs rest 0 (inc e))))))
                linebreak-match (re-find #"^\\\\([ \t]*)$" rest)
                chunk (or (when macro-cand [macro-cand (parse-inline-chunk macro-cand)])
                          (when radio-cand [radio-cand [:text-radio-target [:text-target-name (subs radio-cand 3 (- (count radio-cand) 3))]]])
                           (when target-cand [target-cand (parse-inline-chunk target-cand)])
                           (when link-cand [link-cand (parse-inline-chunk link-cand)])
                           (when plain-url-cand [plain-url-cand (parse-inline-chunk plain-url-cand)])
                           (when foot-cand [foot-cand (parse-inline-chunk foot-cand)])
                           (when ts-cand
                             (let [ts-node (parse-direct ts-cand :timestamp)]
                               (when-not (:failure? ts-node)
                                [ts-cand ts-node])))
                          (when angled-cand [angled-cand (parse-inline-chunk angled-cand)])
                          (when styled-cand [(first styled-cand) (first (second styled-cand))])
                          (when entity-cand [entity-cand (parse-inline-chunk entity-cand)])
                          (when sub-cand [sub-cand (parse-inline-chunk sub-cand)])
                          (when sup-cand [sup-cand (parse-inline-chunk sup-cand)]))]
            (cond
              linebreak-match
              (let [after (second linebreak-match)
                    out* (cond-> out
                           (not= normal "") (conj [:text-normal normal]))]
                (with-raw-span (into [:text] (conj out* [:text-linebreak [:text-linebreak-after after]])) raw))

              (and chunk (not (:failure? (second chunk))) (vector? (second chunk)))
              (let [[cand node] chunk
                    out* (cond-> out
                           (not= normal "") (conj [:text-normal normal]))]
                (recur (+ i (count cand)) "" (conj out* node)))

              (str/starts-with? rest "\\\\")
              (let [out* (cond-> out
                           (not= normal "") (conj [:text-normal normal]))]
                (recur (inc i) "" (conj out* [:text-normal "\\"])))

              (and (contains? #{"*" "/" "_" "=" "~" "+"} (subs raw i (inc i)))
                   (not= normal ""))
              (recur i "" (conj out [:text-normal normal]))

              :else
              (recur (inc i) (str normal (subs raw i (inc i))) out))))))))

(defn- parse-direct [raw start]
  (case start
    :eol (if (or (= raw "") (= raw "\n") (= raw "\r"))
           (with-raw-span '() raw)
           (failure :invalid-eol start raw))
    :s (if (re-matches #"[\t ]+" raw)
         (with-raw-span '() raw)
         (failure :invalid-horizontal-space start raw))
    :word (if (re-matches #"[^\r\n\s]+" raw)
            (with-raw-span [raw] raw)
            (failure :invalid-word start raw))
    :tags (if-let [[_ body] (re-matches #":([a-zA-Z0-9_@#%:]+):" raw)]
            (let [tags (remove empty? (str/split body #":"))]
              (if (seq tags)
                (with-raw-span (into [:tags] tags) raw)
                (failure :invalid-tags start raw)))
            (failure :invalid-tags start raw))
    :diary-sexp (if-let [[_ v] (re-matches #"%%(.+)" raw)]
                  (let [normalized (if (str/starts-with? v "(")
                                     (subs v 1)
                                     v)]
                    (with-raw-span [:diary-sexp normalized] raw))
                  (failure :invalid-diary-sexp start raw))
    :affiliated-keyword-line
    (or
     (when-let [[_ k opt v] (re-matches #"\s*#\+(HEADER|NAME|PLOT|RESULTS|CAPTION|AUTHOR|DATE|TITLE)(?:\[([^\]\r\n]+)\])?:\s+([^\]\r\n]+)" raw)]
       (let [key-node (if opt
                        [:affil-kw-key k [:affil-kw-optional opt]]
                        [:affil-kw-key k])]
         (with-raw-span [:affiliated-keyword-line key-node [:kw-value v]] raw)))
      (when-let [[_ backend v] (re-matches #"\s*#\+ATTR_([a-zA-Z0-9-_]+):\s+([^\]\r\n]+)" raw)]
        (with-raw-span [:affiliated-keyword-line [:affil-kw-attr backend] [:kw-value v]] raw))
      (failure :invalid-affiliated-keyword-line start raw))
    :node-property-line
    (parse-antlr-only raw start)
    :property-drawer
    (parse-antlr-only raw start)
    :noparse-block
    (or (parse-noparse-block-direct raw)
        (failure :invalid-noparse-block start raw))
    :fixed-width-line
    (parse-antlr-only raw start)
    :fixed-width-area
    (parse-antlr-only raw start)
    :link-ext-other
    (parse-antlr-only raw start)
    :link-ext-id
    (parse-antlr-only raw start)
    :link-ext-file
    (parse-antlr-only raw start)
    :text-styled
    (if-let [s (parse-text-styled-direct raw)]
      (with-raw-span s raw)
      (failure :invalid-text-styled start raw))
    :text-link
    (parse-antlr-only raw start)
    :link-format
    (if-let [l (parse-link-format-direct raw)]
      (with-raw-span l raw)
      (failure :invalid-link-format start raw))
    :footnote-link
    (parse-antlr-only raw start)
    :footnote-line
    (parse-antlr-only raw start)
    :ts-time
    (parse-antlr-only raw start)
    :timestamp-inactive-range
    (parse-antlr-only raw start)
    :clock
    (parse-antlr-only raw start)
    :list-item-line
    (if-let [li (parse-list-item-line-direct raw)]
      (with-raw-span li raw)
      (failure :invalid-list-item-line start raw))
    :table
    (if-let [t (parse-table-direct raw)]
      (with-raw-span t raw)
      (failure :invalid-table start raw))
    :timestamp
    (parse-antlr-only raw start)
    :text-entity
    (parse-antlr-only raw start)
    :text-target
    (parse-antlr-only raw start)
    :text-sub
    (parse-antlr-only raw start)
    :text-macro
    (parse-antlr-only raw start)
    :planning
    (parse-antlr-only raw start)
    :text
    (parse-text-direct raw)
    nil))

(defn- headline->ast [ctx]
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
                     (let [parsed-title (parse-text-direct title-txt)]
                       (if (:failure? parsed-title)
                         (text-node title-txt)
                         parsed-title)))
               ctx)))

(defn- content-line->ast [ctx]
  (let [raw (.getText ctx)
        parsed-text (parse-text-direct raw)
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
  (let [marker (.getText (.dynamicBeginMarker ctx))
        name (.getText (.dynamicBlockName ctx))
        marker* (str/upper-case marker)
        params-ctx (.dynamicBlockParameters ctx)]
    (cond
      (= "BEGIN" marker*)
      (with-span (cond-> [:dynamic-block-begin-line [:dynamic-block-name name]]
                   params-ctx (conj [:dynamic-block-parameters (.getText params-ctx)]))
                  ctx)

      (= "END" marker*)
      (with-span [:dynamic-block-end-line] ctx)

      :else
      {:failure? true
       :backend :antlr
       :reason :invalid-dynamic-block-begin-marker
       :raw (.getText ctx)})))

(defn- dynamic-block-end-line->ast [ctx]
  (let [marker (.getText (.dynamicEndMarker ctx))]
    (if (= "END" (str/upper-case marker))
      (with-span [:dynamic-block-end-line] ctx)
      {:failure? true
       :backend :antlr
       :reason :invalid-dynamic-block-end-marker
       :raw (.getText ctx)})))

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

(defn- line->ast [ctx]
  (let [raw (.getText ctx)]
    (cond
      (.headline ctx) (headline->ast (.headline ctx))
      (.todoLine ctx) (todo-line->ast (.todoLine ctx))
      (.blockEndLine ctx) (block-end-line->ast (.blockEndLine ctx))
      (.blockBeginLine ctx) (block-begin-line->ast (.blockBeginLine ctx))
      (.otherKeywordLine ctx) (other-keyword-line->ast (.otherKeywordLine ctx))
      (.dynamicBlockEndLine ctx) (dynamic-block-end-line->ast (.dynamicBlockEndLine ctx))
      (.dynamicBlockBeginLine ctx) (if-let [[_ k v] (re-matches #"#\+([A-Z][A-Z0-9_-]*):\s*(.*)" raw)]
                                     (if (contains? #{"BEGIN" "END"} k)
                                       (dynamic-block-begin-line->ast (.dynamicBlockBeginLine ctx))
                                       (with-span [:other-keyword-line [:kw-name k] [:kw-value v]] ctx))
                                     (dynamic-block-begin-line->ast (.dynamicBlockBeginLine ctx)))
      (.footnoteLine ctx) (footnote-line->ast (.footnoteLine ctx))
      (.commentLine ctx) (comment-line->ast (.commentLine ctx))
      (.horizontalRule ctx) (horizontal-rule->ast (.horizontalRule ctx))
      (.drawerBeginLine ctx) (drawer-begin-line->ast (.drawerBeginLine ctx))
      (.drawerEndLine ctx) (drawer-end-line->ast (.drawerEndLine ctx))
      (.contentLine ctx) (content-line->ast (.contentLine ctx))
      (.emptyLine ctx) (with-span [:empty-line] (.emptyLine ctx))
      :else nil)))

(defn- s->ast [ctx]
  (let [from-lines (map (fn [line-ctx]
                          (if-let [line (.line line-ctx)]
                            (line->ast line)
                            (with-span [:empty-line] line-ctx)))
                        (.lineWithNl ctx))
        eof-line (some-> ctx .lineAtEof .line line->ast)
        all-lines (if eof-line
                    (concat from-lines [eof-line])
                    from-lines)]
    (with-span (into [:S] (remove nil? all-lines)) ctx)))

(defn- throwing-error-listener []
  (proxy [BaseErrorListener] []
    (syntaxError [_ _ line col msg _]
      (throw (ParseCancellationException.
              (str "line " line ":" col " " msg))))))

(defn- parser-for [raw]
  (try
    (let [input (CharStreams/fromString raw)
          lexer (instantiate lexer-class-name input)
          tokens (CommonTokenStream. lexer)
          parser (instantiate parser-class-name tokens)
          listener (throwing-error-listener)]
      (.removeErrorListeners lexer)
      (.removeErrorListeners parser)
      (.addErrorListener lexer listener)
      (.addErrorListener parser listener)
      (.setErrorHandler parser (BailErrorStrategy.))
      parser)
    (catch ClassNotFoundException _
      {:failure? true
       :backend :antlr
       :reason :missing-generated-classes
       :message "Generate ANTLR classes with ./script/gen-antlr.sh"})))

(defn- parse-antlr-only
  [raw start]
  (try
    (let [parser (parser-for raw)]
      (if (:failure? parser)
        parser
        (let [result (case start
                       :S (-> (s->ast (.s parser))
                              (postprocess/attach-headline-planning parse-direct raw)
                              (postprocess/collapse-table-lines parse-direct raw)
                              (postprocess/strip-nested-s-lines))
                       :s (s->ast (.s parser))
                       :drawer (-> parser .s s->ast postprocess/drawer-from-s)
                       :dynamic-block (-> parser .s s->ast postprocess/dynamic-block-from-s)
                       :block (let [np (parse-direct raw :noparse-block)]
                                (if (and np (not (:failure? np)))
                                  (with-meta [:block np]
                                    (or (meta np) {}))
                                  (-> parser .s s->ast postprocess/block-from-s)))
                       :headline (let [s-ast (-> (s->ast (.s parser))
                                                 (postprocess/attach-headline-planning parse-direct raw))
                                       lines (rest s-ast)]
                                   (if (and (= 1 (count lines))
                                            (= :headline (first (first lines))))
                                     (with-meta (first lines)
                                       (merge (meta (first lines))
                                              {:span [0 (count raw)]}))
                                     (failure :invalid-headline start raw)))
                       :todo-line (todo-line->ast (.todoLine (.todoLineEof parser)))
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
                       :line (let [line-ast (line->ast (.line (.lineEof parser)))]
                               (if (:failure? line-ast)
                                 line-ast
                                 (with-meta [line-ast]
                                   (or (meta line-ast) {}))))
                       :noparse-block (or (parse-direct raw :noparse-block)
                                          (failure :invalid-noparse-block start raw))
                       :drawer-begin-line (drawer-begin-line->ast (.drawerBeginLine (.drawerBeginLineEof parser)))
                       :drawer-end-line (drawer-end-line->ast (.drawerEndLine (.drawerEndLineEof parser)))
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
                       :text-entity (text-entity->ast (.textEntity (.textEntityEof parser)))
                       :text-target (text-target->ast (.textTarget (.textTargetEof parser)))
                       :text-sub (text-sub->ast (.textSub (.textSubEof parser)))
                       :text-macro (text-macro->ast (.textMacro (.textMacroEof parser)))
                       (failure :unsupported-start start raw))
              eof? (= Token/EOF (.getType (.getCurrentToken parser)))]
          (if (or (:failure? result)
                  eof?
                  (contains? #{:block :headline} start))
            result
            (failure :unconsumed-input start raw)))))
    (catch ParseCancellationException e
      {:failure? true
       :backend :antlr
       :reason :parse-error
       :message (.getMessage e)
       :start start})))

(defn parse
  [raw {:keys [start] :or {start :s}}]
  (if-let [direct (parse-direct raw start)]
    direct
    (parse-antlr-only raw start)))
