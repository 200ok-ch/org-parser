(ns org-parser.antlr.parser
  (:require [org-parser.antlr.parser-shared :as shared])
  (:import [org.antlr.v4.runtime BaseErrorListener BailErrorStrategy CharStream CharStreams CommonTokenStream TokenStream]
           [org.antlr.v4.runtime.atn PredictionMode]
           [org.antlr.v4.runtime.misc ParseCancellationException]
           [org.antlr.v4.runtime Token]))

(def ^:private lexer-class-name "org_parser.antlr.OrgLexer")
(def ^:private parser-class-name "org_parser.antlr.OrgParser")
(def ^:private cache-size-limit 4096)
(def ^:dynamic *telemetry* nil)
(def ^:dynamic *direct-cache* nil)

(declare parse-antlr-only)

(defonce ^:private lexer-class (delay (Class/forName lexer-class-name)))
(defonce ^:private parser-class (delay (Class/forName parser-class-name)))
(defonce ^:private lexer-constructor
  (delay (.getConstructor ^Class @lexer-class (into-array Class [CharStream]))))
(defonce ^:private parser-constructor
  (delay (.getConstructor ^Class @parser-class (into-array Class [TokenStream]))))

(defn- instantiate-lexer [input]
  (.newInstance ^java.lang.reflect.Constructor @lexer-constructor (object-array [input])))

(defn- instantiate-parser [tokens]
  (.newInstance ^java.lang.reflect.Constructor @parser-constructor (object-array [tokens])))

(defn- parse-direct [raw start]
  (if-not (contains? shared/direct-starts start)
    nil
    (if *direct-cache*
      (let [cache-key [start raw]]
        (if (instance? java.util.HashMap *direct-cache*)
          (let [cache ^java.util.HashMap *direct-cache*
                has-key? (.containsKey cache cache-key)]
            (if has-key?
              (.get cache cache-key)
              (let [result (shared/parse-direct parse-antlr-only raw start)]
                (when (< (.size cache) cache-size-limit)
                  (.put cache cache-key result))
                result)))
          (let [cached (get @*direct-cache* cache-key ::miss)]
            (if-not (= ::miss cached)
              cached
              (let [result (shared/parse-direct parse-antlr-only raw start)]
                (when (< (count @*direct-cache*) cache-size-limit)
                  (swap! *direct-cache* assoc cache-key result))
                result)))))
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

(defonce ^:private error-listener
  (delay (throwing-error-listener)))

(defn- parser-for [raw prediction-mode]
  (try
    (let [input (CharStreams/fromString raw)
          lexer (instantiate-lexer input)
          tokens (CommonTokenStream. lexer)
          parser (instantiate-parser tokens)
          listener @error-listener]
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
    (let [{:keys [parser tokens] :as parser-state} (parser-for raw PredictionMode/SLL)]
      (if (:failure? parser-state)
        parser-state
        (try
          (let [result (shared/parse-result parser raw start parse-direct eof?)]
            (note! :sll-success start)
            result)
          (catch ParseCancellationException _
            (note! :sll-fallback start)
            (.seek ^CommonTokenStream tokens 0)
            (.reset parser)
            (.. parser getInterpreter (setPredictionMode PredictionMode/LL))
            (.setErrorHandler parser (BailErrorStrategy.))
            (let [result (shared/parse-result parser raw start parse-direct eof?)]
              (note! :ll-success start)
              result)))))
    (catch ParseCancellationException e
      {:failure? true
       :backend :antlr
       :reason :parse-error
       :message (.getMessage e)
       :start start})))

(defn parse
  [raw {:keys [start] :or {start :s}}]
  (binding [*direct-cache* (or *direct-cache* (java.util.HashMap.))
            shared/*text-node-cache* (or shared/*text-node-cache* (java.util.HashMap.))
            shared/*line-template-cache* (or shared/*line-template-cache* (java.util.HashMap.))]
    (if-let [direct (parse-direct raw start)]
      direct
      (parse-antlr-only raw start))))
