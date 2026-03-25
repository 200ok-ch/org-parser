(ns org-parser.antlr.parser
  (:require [org-parser.antlr.parser-shared :as shared]))

(def ^:private antlr4 (js/require "antlr4"))
(def ^:private prediction-mode (or (.-PredictionMode antlr4)
                                   (some-> antlr4 .-atn .-PredictionMode)))
(def ^:private bail-error-strategy-class (or (.-BailErrorStrategy antlr4)
                                             (some-> antlr4 .-error .-BailErrorStrategy)))
(def ^:private cwd (.cwd js/process))
(def ^:private lexer-module-path
  (str cwd "/src/js/antlr/OrgLexer.cjs"))
(def ^:private parser-module-path
  (str cwd "/src/js/antlr/OrgParser.cjs"))
(def ^:private OrgLexer (js/require lexer-module-path))
(def ^:private OrgParser (js/require parser-module-path))

(declare parse-antlr-only)

(defn- parse-direct [raw start]
  (shared/parse-direct parse-antlr-only raw start))

(defn- throwing-error-listener []
  (doto (new (.-ErrorListener antlr4))
    (aset "syntaxError"
          (fn [_ _ line col msg _]
            (throw (js/Error. (str "line " line ":" col " " msg)))))))

(defn- parser-for [raw mode]
  (let [input (new (.-InputStream antlr4) raw)
        lexer (new OrgLexer input)
        tokens (new (.-CommonTokenStream antlr4) lexer)
        parser (new OrgParser tokens)
        listener (throwing-error-listener)]
    (.removeErrorListeners lexer)
    (.removeErrorListeners parser)
    (.addErrorListener lexer listener)
    (.addErrorListener parser listener)
    (when-let [interp (or (.-_interp parser) (.-interpreter parser))]
      (set! (.-predictionMode interp) mode))
    (when bail-error-strategy-class
      (set! (.-_errHandler parser) (new bail-error-strategy-class)))
    {:parser parser
     :tokens tokens}))

(defn- eof? [parser]
  (let [current-token (.getCurrentToken parser)
        token-type (or (some-> current-token .-type)
                       (when (and current-token
                                  (fn? (.-getType current-token)))
                         (.getType current-token)))]
    (= (.-EOF (.-Token antlr4)) token-type)))

(defn- parse-antlr-only [raw start]
  (try
    (let [{:keys [parser]} (parser-for raw (.-SLL prediction-mode))]
      (try
        (shared/parse-result parser raw start parse-direct eof?)
        (catch js/Error _
          (let [{:keys [parser]} (parser-for raw (.-LL prediction-mode))]
            (shared/parse-result parser raw start parse-direct eof?)))))
    (catch js/Error e
      {:failure? true
       :backend :antlr
       :reason :parse-error
       :message (.-message e)
       :start start})))

(defn parse
  [raw {:keys [start] :or {start :s}}]
  (if-let [direct (parse-direct raw start)]
    direct
    (parse-antlr-only raw start)))
