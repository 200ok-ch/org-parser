(ns org-parser.antlr.parser
  (:require [clojure.string :as str])
  (:import [org.antlr.v4.runtime BaseErrorListener CharStreams CommonTokenStream]
           [org.antlr.v4.runtime.misc ParseCancellationException]
           [org.antlr.v4.runtime Token]
           [clojure.lang Reflector]))

(def ^:private lexer-class-name "org_parser.antlr.OrgLexer")
(def ^:private parser-class-name "org_parser.antlr.OrgParser")

(declare parse-direct)

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
                              (or (when-let [[_ n braces] (re-find #"^\\([A-Za-z]+)(\{\})?" rest)]
                                    (str "\\" n (or braces "")))))
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
    (parse-node-property-line-direct raw)
    :property-drawer
    (let [lines (str/split raw #"\r?\n")
          begin (first lines)
          ending (last lines)
          middle (vec (butlast (rest lines)))]
      (if (and (<= 2 (count lines))
               (re-matches #":PROPERTIES:[\t ]*" begin)
               (re-matches #":END:[\t ]*" ending))
        (let [props (mapv parse-node-property-line-direct middle)]
          (if (some :failure? props)
            (failure :invalid-property-drawer-content start raw)
            (with-raw-span (into [:property-drawer] props) raw)))
        (failure :invalid-property-drawer start raw)))
    :noparse-block
    (or (parse-noparse-block-direct raw)
        (failure :invalid-noparse-block start raw))
    :fixed-width-line
    (if-let [[_ v] (re-matches #"[\t ]*:(?: |$)(.*)" raw)]
      (with-raw-span [:fixed-width-line v] raw)
      (failure :invalid-fixed-width-line start raw))
    :fixed-width-area
    (let [lines (str/split raw #"\r?\n")
          parsed (mapv #(parse-direct % :fixed-width-line) lines)]
      (if (and (seq lines)
               (not-any? :failure? parsed))
        (with-raw-span (into [:fixed-width-area] parsed) raw)
        (failure :invalid-fixed-width-area start raw)))
    :link-ext-other
    (if-let [[_ scheme rest] (re-matches #"([a-zA-Z][a-zA-Z0-9+.-]*):(.*)" raw)]
      (with-raw-span [:link-ext-other [:link-url-scheme scheme] [:link-url-rest rest]] raw)
      (failure :invalid-link-ext-other start raw))
    :link-ext-id
    (if-let [[_ id] (re-matches #"\[\[id:([a-zA-Z0-9][a-zA-Z0-9-]+)\]\]" raw)]
      (with-raw-span [:link-ext-id id] raw)
      (failure :invalid-link-ext-id start raw))
    :link-ext-file
    (let [path+loc (if (str/starts-with? raw "file:")
                     (subs raw 5)
                     raw)
          [path loc] (str/split path+loc #"::" 2)
          loc-node (when loc
                     (cond
                       (re-matches #"[0-9]+" loc) [:link-file-loc-lnum loc]
                       (str/starts-with? loc "*") [:link-file-loc-headline (subs loc 1)]
                       (str/starts-with? loc "#") [:link-file-loc-customid (subs loc 1)]
                       :else [:link-file-loc-string loc]))]
      (if (seq path)
        (with-raw-span (cond-> [:link-ext-file path]
                         loc-node (conj loc-node))
                       raw)
        (failure :invalid-link-ext-file start raw)))
    :text-styled
    (if-let [s (parse-text-styled-direct raw)]
      (with-raw-span s raw)
      (failure :invalid-text-styled start raw))
    :text-link
    (if-let [l (parse-text-link-direct raw)]
      (with-raw-span l raw)
      (failure :invalid-text-link start raw))
    :link-format
    (if-let [l (parse-link-format-direct raw)]
      (with-raw-span l raw)
      (failure :invalid-link-format start raw))
    :footnote-link
    (or
     (when-let [[_ label] (re-matches #"\[fn:([a-zA-Z0-9-_]+)\]" raw)]
       (with-raw-span [:footnote-link [:fn-label label]] raw))
     (when-let [[_ text] (re-matches #"\[fn::([^\[\]]*)\]" raw)]
       (with-raw-span [:footnote-link text] raw))
     (when-let [[_ label text] (re-matches #"\[fn:([a-zA-Z0-9-_]+):([^\[\]]*)\]" raw)]
       (with-raw-span [:footnote-link [:fn-label label] text] raw))
     (failure :invalid-footnote-link start raw))
    :footnote-line
    (if-let [[_ label text] (re-matches #"\[fn:([a-zA-Z0-9-_]+)\] (.+)" raw)]
      (with-raw-span [:footnote-line [:fn-label label] (text-node text)] raw)
      (failure :invalid-footnote-line start raw))
    :ts-time
    (if (re-matches #"\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?" raw)
      (with-raw-span [:ts-time raw] raw)
      (failure :invalid-ts-time start raw))
    :timestamp-inactive-range
    (or
     (when-let [[_ d day t1 t2] (re-matches #"\[(\d{4}-\d{2}-\d{2})\s+([^\d\s>\]]+)\s+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)-(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)\]" raw)]
       (with-raw-span [:timestamp-inactive-range
                       [:ts-inner-span
                        (ts-inner-w-time-node d day t1)
                        [:ts-time t2]
                        [:ts-modifiers]]]
                      raw))
     (when-let [[_ d1 day1 t1 d2 day2 t2] (re-matches #"\[(\d{4}-\d{2}-\d{2})\s+([^\d\s>\]]+)\s+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)\]--\[(\d{4}-\d{2}-\d{2})\s+([^\d\s>\]]+)\s+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)\]" raw)]
       (with-raw-span [:timestamp-inactive-range
                       (ts-inner-w-time-node d1 day1 t1)
                       (ts-inner-w-time-node d2 day2 t2)]
                      raw))
     (failure :invalid-timestamp-inactive-range start raw))
    :clock
    (or
     (when-let [[_ ts hh mm] (re-matches #"[\t ]*CLOCK:[\t ]*(\[[^\]]+\](?:--\[[^\]]+\])?)[\t ]*=>[\t ]*([0-9]+):([0-9]{2})[\t ]*" raw)]
       (let [range (parse-direct ts :timestamp-inactive-range)]
         (if (:failure? range)
           (failure :invalid-clock-range start raw)
           (with-raw-span [:clock range [:clock-duration [:clock-dur-hh hh] [:clock-dur-mm mm]]] raw))))
     (failure :invalid-clock start raw))
    :list-item-line
    (if-let [li (parse-list-item-line-direct raw)]
      (with-raw-span li raw)
      (failure :invalid-list-item-line start raw))
    :table
    (if-let [t (parse-table-direct raw)]
      (with-raw-span t raw)
      (failure :invalid-table start raw))
    :timestamp
    (or
     (when-let [[_ v] (re-matches #"<%%(.+)>" raw)]
       (with-raw-span [:timestamp [:timestamp-diary v]] raw))
     (when-let [[_ d day t1 t2 mods]
                (re-matches #"<(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)-(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)(?:[ \t]+(.+))?>" raw)]
       (when-let [mod-nodes (parse-ts-modifiers mods)]
         (with-raw-span [:timestamp
                         [:timestamp-active
                          [:ts-inner-span
                           (cond-> [:ts-inner-w-time [:ts-date d]]
                             day (conj [:ts-day day])
                             true (conj [:ts-time t1]))
                           [:ts-time t2]
                           (into [:ts-modifiers] mod-nodes)]]]
                        raw)))
     (when-let [[_ d day t1 t2 mods]
                (re-matches #"\[(\d{4}-\d{2}-\d{2})(?:[ \t]+([^\d\s>\]]+))?[ \t]+(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)-(\d{1,2}:\d{2}(?::\d{2})?(?:[AaPp][Mm])?)(?:[ \t]+(.+))?\]" raw)]
       (when-let [mod-nodes (parse-ts-modifiers mods)]
         (with-raw-span [:timestamp
                         [:timestamp-inactive
                          [:ts-inner-span
                           (cond-> [:ts-inner-w-time [:ts-date d]]
                             day (conj [:ts-day day])
                             true (conj [:ts-time t1]))
                           [:ts-time t2]
                           (into [:ts-modifiers] mod-nodes)]]]
                        raw)))
     (when-let [[_ a b] (re-matches #"(<[^>]+>)--(<[^>]+>)" raw)]
       (let [ia (parse-ts-inner-node (subs a 1 (dec (count a))))
             ib (parse-ts-inner-node (subs b 1 (dec (count b))))]
         (when (and ia ib)
           (with-raw-span [:timestamp [:timestamp-active ia ib]] raw))))
     (when-let [[_ a b] (re-matches #"(\[[^\]]+\])--(\[[^\]]+\])" raw)]
       (let [ia (parse-ts-inner-node (subs a 1 (dec (count a))))
             ib (parse-ts-inner-node (subs b 1 (dec (count b))))]
         (when (and ia ib)
           (with-raw-span [:timestamp [:timestamp-inactive ia ib]] raw))))
     (when-let [t (parse-simple-timestamp-point raw)]
       (with-raw-span t raw))
     (failure :invalid-timestamp start raw))
    :text-entity
    (if-let [[_ name braces] (re-matches #"\\([A-Za-z]+)(\{\})?" raw)]
      (with-raw-span (cond-> [:text-entity [:entity-name name]]
                       braces (conj [:entity-braces]))
                     raw)
      (failure :invalid-text-entity start raw))
    :text-target
    (if-let [[_ name] (re-matches #"<<([^\s<>](?:[^<>]*[^\s<>])?)>>" raw)]
      (with-raw-span [:text-target [:text-target-name name]] raw)
      (failure :invalid-text-target start raw))
    :text-sub
    (or
     (when-let [[_ v] (re-matches #"_\{([^{}]+)\}" raw)]
       (with-raw-span [:text-sub [:text-subsup-curly v]] raw))
     (when-let [[_ v] (re-matches #"_([^\s]+)" raw)]
       (with-raw-span [:text-sub [:text-subsup-word v]] raw))
     (failure :invalid-text-sub start raw))
    :text-macro
    (if-let [[_ name args] (re-matches #"\{\{\{([A-Za-z0-9_]+)\((.*)\)\}\}\}" raw)]
      (let [parts (if (= args "")
                    [""]
                    (vec (str/split args #"(?<!\\),")))]
        (with-raw-span [:text-macro [:macro-name name] (into [:macro-args] parts)] raw))
      (failure :invalid-text-macro start raw))
    :planning
    (let [infos (re-seq #"(SCHEDULED|DEADLINE|CLOSED):\s*([<\[].*?[>\]])" raw)
          parsed (mapv (fn [[_ kw ts]]
                         (when-let [ts-node (parse-planning-ts ts)]
                           [:planning-info
                            [:planning-keyword [(keyword (str "planning-kw-" (str/lower-case kw)))]]
                            ts-node]))
                       infos)]
      (if (and (seq infos)
               (every? some? parsed))
        (with-raw-span (into [:planning] parsed) raw)
        (failure :invalid-planning start raw)))
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
    (let [parsed-title (parse-text-direct title-txt)
          title-node (if (:failure? parsed-title)
                       (text-node title-txt)
                       parsed-title)]
      (with-span (conj with-comment-token title-node) ctx))))

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

(defn- drawer-from-s [s-ast]
  (let [lines (rest s-ast)
        begin (first lines)
        end (last lines)
        middle (butlast (rest lines))]
    (if (and (>= (count lines) 2)
             (= :drawer-begin-line (first begin))
             (= :drawer-end-line (first end))
             (every? #(= :content-line (first %)) middle))
      (into [:drawer begin] middle)
      {:failure? true
       :backend :antlr
       :reason :drawer-shape-mismatch})))

(defn- dynamic-block-from-s [s-ast]
  (let [lines (rest s-ast)
        begin (first lines)
        end (last lines)
        middle (butlast (rest lines))]
    (if (and (>= (count lines) 2)
             (= :dynamic-block-begin-line (first begin))
             (= :dynamic-block-end-line (first end))
             (every? #(= :content-line (first %)) middle))
      (into [:dynamic-block begin] middle)
      {:failure? true
       :backend :antlr
       :reason :dynamic-block-shape-mismatch})))

(defn- block-from-s [s-ast]
  (let [lines (rest s-ast)
        begin (first lines)
        end (last lines)
        middle (butlast (rest lines))]
    (if (and (>= (count lines) 2)
             (= :block-begin-line (first begin))
             (= :block-end-line (first end)))
      [:block (into [:greater-block begin] (concat middle [end]))]
      {:failure? true
       :backend :antlr
       :reason :block-shape-mismatch})))

(defn- node-raw [node raw]
  (let [[start end] (:span (meta node))]
    (subs raw start end)))

(defn- attach-headline-planning [s-ast raw]
  (let [lines (vec (rest s-ast))]
    (loop [idx 0
           acc []]
      (if (>= idx (count lines))
        (with-meta (into [:S] acc) (meta s-ast))
        (let [line (nth lines idx)
              next-line (when (< (inc idx) (count lines)) (nth lines (inc idx)))]
          (if (and (= :headline (first line))
                   next-line
                   (= :content-line (first next-line)))
            (let [next-raw (node-raw next-line raw)
                  planning (parse-direct next-raw :planning)]
              (if (and planning (not (:failure? planning)))
                (recur (+ idx 2) (conj acc (conj line planning)))
                (recur (inc idx) (conj acc line))))
            (recur (inc idx) (conj acc line))))))))

(defn- table-candidate-line? [line raw]
  (and (= :content-line (first line))
       (let [r (node-raw line raw)
             t (str/trim r)]
         (or (re-matches #"\|.*\|" t)
             (re-matches #"\+[-+]+\+" t)
             (re-matches #"#\+TBLFM: .*" t)))))

(defn- collapse-table-lines [s-ast raw]
  (let [lines (vec (rest s-ast))]
    (loop [idx 0
           acc []]
      (if (>= idx (count lines))
        (with-meta (into [:S] acc) (meta s-ast))
        (let [line (nth lines idx)]
          (if (table-candidate-line? line raw)
            (let [end-idx (loop [j idx]
                            (if (and (< j (count lines))
                                     (table-candidate-line? (nth lines j) raw))
                              (recur (inc j))
                              j))
                  group (subvec lines idx end-idx)
                  group-raw (str/join "\n" (map #(node-raw % raw) group))
                  table (parse-direct group-raw :table)]
              (if (and table (not (:failure? table)))
                (let [start (first (:span (meta (first group))))
                      end (second (:span (meta (last group))))
                      table* (with-meta table {:span [start end]})]
                  (recur end-idx (conj acc table*)))
                (recur (inc idx) (conj acc line))))
            (recur (inc idx) (conj acc line))))))))

(defn- strip-nested-s-lines [s-ast]
  (if (= :S (first s-ast))
    (with-meta (into [:S]
                     (remove #(and (sequential? %)
                                   (= :S (first %)))
                             (rest s-ast)))
               (meta s-ast))
    s-ast))

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
      parser)
    (catch ClassNotFoundException _
      {:failure? true
       :backend :antlr
       :reason :missing-generated-classes
       :message "Generate ANTLR classes with ./script/gen-antlr.sh"})))

(defn parse
  [raw {:keys [start] :or {start :s}}]
  (if-let [direct (parse-direct raw start)]
    direct
    (try
      (let [parser (parser-for raw)]
        (if (:failure? parser)
          parser
          (let [result (case start
                          :S (-> (s->ast (.s parser))
                                 (attach-headline-planning raw)
                                 (collapse-table-lines raw)
                                 (strip-nested-s-lines))
                         :s (s->ast (.s parser))
                         :drawer (-> parser .s s->ast drawer-from-s)
                         :dynamic-block (-> parser .s s->ast dynamic-block-from-s)
                          :block (let [np (parse-direct raw :noparse-block)]
                                   (if (and np (not (:failure? np)))
                                     (with-meta [:block np]
                                       (or (meta np) {}))
                                     (let [b (-> parser .s s->ast block-from-s)]
                                       (if (:failure? b)
                                         b
                                         b))))
                          :headline (let [s-ast (-> (s->ast (.s parser))
                                                    (attach-headline-planning raw))
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
         :start start}))))
