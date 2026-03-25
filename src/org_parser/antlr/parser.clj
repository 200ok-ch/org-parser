(ns org-parser.antlr.parser
  (:require [org-parser.antlr.parser-shared :as shared])
  (:import [org.antlr.v4.runtime BaseErrorListener BailErrorStrategy CharStreams CommonTokenStream]
           [org.antlr.v4.runtime.atn PredictionMode]
           [org.antlr.v4.runtime.misc ParseCancellationException]
           [org.antlr.v4.runtime Token]
           [clojure.lang Reflector]))

(def ^:private lexer-class-name "org_parser.antlr.OrgLexer")
(def ^:private parser-class-name "org_parser.antlr.OrgParser")

(declare parse-antlr-only)

(defn- load-class [class-name]
  (Class/forName class-name))

(defn- instantiate [class-name & args]
  (Reflector/invokeConstructor (load-class class-name) (to-array args)))

(defn- parse-direct [raw start]
  (shared/parse-direct parse-antlr-only raw start))

(defn- throwing-error-listener []
  (proxy [BaseErrorListener] []
    (syntaxError [_ _ line col msg _]
      (throw (ParseCancellationException.
              (str "line " line ":" col " " msg))))))

(defn- parser-for [raw prediction-mode]
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
      (.. parser getInterpreter (setPredictionMode prediction-mode))
      (.setErrorHandler parser (BailErrorStrategy.))
      {:parser parser
       :tokens tokens})
    (catch ClassNotFoundException _
      {:failure? true
       :backend :antlr
       :reason :missing-generated-classes
       :message "Generate ANTLR classes with ./script/gen-antlr.sh"})))

(defn- eof? [parser]
  (= Token/EOF (.getType (.getCurrentToken parser))))

(defn- parse-antlr-only [raw start]
  (try
    (let [{:keys [parser] :as parser-state} (parser-for raw PredictionMode/SLL)]
      (if (:failure? parser-state)
        parser-state
        (try
          (shared/parse-result parser raw start parse-direct eof?)
          (catch ParseCancellationException _
            (let [{:keys [parser] :as retry-state} (parser-for raw PredictionMode/LL)]
              (if (:failure? retry-state)
                retry-state
                (shared/parse-result parser raw start parse-direct eof?)))))))
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
