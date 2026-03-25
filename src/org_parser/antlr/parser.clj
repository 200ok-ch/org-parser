(ns org-parser.antlr.parser
  (:require [org-parser.antlr.parser-shared :as shared])
  (:import [org.antlr.v4.runtime BaseErrorListener BailErrorStrategy CharStreams CommonTokenStream]
           [org.antlr.v4.runtime.atn PredictionMode]
           [org.antlr.v4.runtime.misc ParseCancellationException]
           [org.antlr.v4.runtime Token]
           [clojure.lang Reflector]))

(def ^:private lexer-class-name "org_parser.antlr.OrgLexer")
(def ^:private parser-class-name "org_parser.antlr.OrgParser")
(def ^:dynamic *telemetry* nil)
(def ^:dynamic *direct-cache* nil)

(declare parse-antlr-only)

(defn- load-class [class-name]
  (Class/forName class-name))

(defn- instantiate [class-name & args]
  (Reflector/invokeConstructor (load-class class-name) (to-array args)))

(defn- parse-direct [raw start]
  (if-not (contains? shared/direct-starts start)
    nil
    (if *direct-cache*
      (let [cache-key [start raw]]
        (if (contains? @*direct-cache* cache-key)
          (get @*direct-cache* cache-key)
          (let [result (shared/parse-direct parse-antlr-only raw start)]
            (swap! *direct-cache* assoc cache-key result)
            result)))
      (shared/parse-direct parse-antlr-only raw start))))

(defn- note! [event start]
  (when *telemetry*
    (swap! *telemetry*
           (fn [state]
             (-> state
                 (update :events conj {:event event :start start})
                 (update-in [:counts event] (fnil inc 0))
                 (update-in [:starts start event] (fnil inc 0)))))))

(defn with-telemetry [f]
  (let [telemetry (atom {:events [] :counts {} :starts {}})
        result (binding [*telemetry* telemetry]
                 (f))]
    {:result result
     :telemetry @telemetry}))

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
          (let [result (shared/parse-result parser raw start parse-direct eof?)]
            (note! :sll-success start)
            result)
          (catch ParseCancellationException _
            (note! :sll-fallback start)
            (let [{:keys [parser] :as retry-state} (parser-for raw PredictionMode/LL)]
              (if (:failure? retry-state)
                retry-state
                (do
                  (let [result (shared/parse-result parser raw start parse-direct eof?)]
                    (note! :ll-success start)
                    result))))))))
    (catch ParseCancellationException e
      {:failure? true
       :backend :antlr
       :reason :parse-error
       :message (.getMessage e)
       :start start})))

(defn parse
  [raw {:keys [start] :or {start :s}}]
  (binding [*direct-cache* (or *direct-cache* (atom {}))
            shared/*text-node-cache* (or shared/*text-node-cache* (atom {}))
            shared/*line-template-cache* (or shared/*line-template-cache* (atom {}))]
    (if-let [direct (parse-direct raw start)]
      direct
      (parse-antlr-only raw start))))
