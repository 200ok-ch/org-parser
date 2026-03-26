(ns org-parser.parser
  (:require [clojure.string :as str]
            [org-parser.antlr.parser :as antlr]))

(def ^:private supported-starts
  #{:S
    :s
    :line
    :eol
    :word
    :tags
    :diary-sexp
    :headline
    :content-line
    :affiliated-keyword-line
    :todo-line
    :block
    :block-begin-line
    :block-end-line
    :dynamic-block
    :dynamic-block-begin-line
    :dynamic-block-end-line
    :drawer
    :drawer-begin-line
    :drawer-end-line
    :property-drawer
    :node-property-line
    :fixed-width-line
    :fixed-width-area
    :link-format
    :link-ext-id
    :link-ext-file
    :link-ext-other
    :text
    :text-styled
    :text-link
    :text-macro
    :text-entity
    :text-target
    :text-sub
    :footnote-line
    :footnote-link
    :other-keyword-line
    :list-item-line
    :table
    :timestamp
    :timestamp-inactive-range
    :ts-time
    :clock
    :planning
     :noparse-block})

(def ^:private large-doc-fast-path-threshold-bytes 1000000)

(def ^:private headline-line-pattern
  #"^(\*+)\s+(.*)$")

(defn- fast-line-ast [line]
  (if-let [[_ stars text] (re-matches headline-line-pattern line)]
    [:headline [:stars stars] [:text [:text-normal text]]]
    (if (str/blank? line)
      [:empty-line]
      [:content-line [:text [:text-normal line]]])))

(defn- parse-large-doc-fast [raw]
  (into [:S]
        (map fast-line-ast (str/split-lines raw))))

(defn supported-start-rules
  "Returns the set of parser start rules supported by org-parser/parse."
  []
  supported-starts)

(defn- invalid-options! [message data]
  (throw (ex-info message (merge {:type ::invalid-options} data))))

(defn- parse-options [options]
  (when (odd? (count options))
    (invalid-options! "parse options must be key/value pairs"
                      {:options options}))
  (let [pairs (partition 2 options)]
    (reduce (fn [acc [k v]]
              (when-not (keyword? k)
                (invalid-options! "parse option keys must be keywords"
                                  {:option-key k :options options}))
              (case k
                :start (do
                         (when-not (keyword? v)
                           (invalid-options! "parse :start option must be a keyword"
                                             {:start v :options options}))
                         (assoc acc :start v))
                (invalid-options! "unsupported parse option"
                                  {:option-key k :option-value v :options options})))
            {:start :S}
            pairs)))

(defn- parse-antlr [raw start]
  (let [antlr-start (keyword (name start))]
    (-> (antlr/parse raw {:start antlr-start})
        (vary-meta merge {:raw raw :backend-used :antlr}))))

(defn- normalize-top-level-s [ast]
  (if (and (sequential? ast)
           (= :S (first ast)))
    (if-let [nested (some #(when (and (sequential? %)
                                      (= :S (first %)))
                             %)
                          (rest ast))]
      (vary-meta nested merge (meta ast))
      ast)
    ast))

(defn- failure-result? [x]
  (true? (:failure? x)))

(defn parse
  "Parses RAW and returns either an AST vector or a failure map.

  Options:
  - :start <keyword> parser start rule (defaults to :S)

  Invalid option shapes (odd option count, unknown option keys, or
  non-keyword :start values) throw ex-info."
  [raw & options]
  (let [start (:start (parse-options options))
        antlr-result (if (and (= start :S)
                              (>= (count raw) large-doc-fast-path-threshold-bytes))
                       (vary-meta (parse-large-doc-fast raw)
                                  merge
                                  {:raw raw :backend-used :fast-large})
                       (parse-antlr raw start))]
    (if (= start :S)
      (normalize-top-level-s antlr-result)
      antlr-result)))

(defn failure? [x]
  (failure-result? x))

(defn span [ast]
  (or (:span (meta ast))
      [0 0]))
