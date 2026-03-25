(ns org-parser.antlr.parser
  (:require [org-parser.antlr.parser-shared :as shared]))

(def ^:private antlr4 (js/require "antlr4"))
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

(defn- parser-for [raw]
  (let [input (new (.-InputStream antlr4) raw)
        lexer (new OrgLexer input)
        tokens (new (.-CommonTokenStream antlr4) lexer)
        parser (new OrgParser tokens)
        listener (throwing-error-listener)]
    (.removeErrorListeners lexer)
    (.removeErrorListeners parser)
    (.addErrorListener lexer listener)
    (.addErrorListener parser listener)
    parser))

(defn- eof? [parser]
  (let [current-token (.getCurrentToken parser)
        token-type (or (some-> current-token .-type)
                       (when (and current-token
                                  (fn? (.-getType current-token)))
                         (.getType current-token)))]
    (= (.-EOF (.-Token antlr4)) token-type)))

(defn- parse-antlr-only [raw start]
  (try
    (shared/parse-result (parser-for raw) raw start parse-direct eof?)
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
