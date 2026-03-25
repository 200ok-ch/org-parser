(ns org-parser.benchmark
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [org-parser.antlr.parser :as antlr-parser]
            [org-parser.core :as core]
            [org-parser.parser :as parser]))

(def ^:private default-warmup-runs 3)
(def ^:private default-measure-runs 8)
(def ^:private min-large-lines 31000)

(defn- now-nanos [] (System/nanoTime))

(defn- elapsed-ms [start-nanos]
  (/ (- (now-nanos) start-nanos) 1000000.0))

(defn- percentile [xs p]
  (let [sorted (vec (sort xs))
        n (count sorted)
        idx (-> (* (dec n) p) Math/round long)]
    (nth sorted idx)))

(defn- summarize [name timings-ms]
  {:name name
   :runs (count timings-ms)
   :min-ms (double (apply min timings-ms))
   :p50-ms (double (percentile timings-ms 0.50))
   :p95-ms (double (percentile timings-ms 0.95))
   :max-ms (double (apply max timings-ms))
   :avg-ms (double (/ (reduce + timings-ms) (count timings-ms)))})

(defn- line-count [s]
  (count (str/split-lines s)))

(defn- large-readme-content []
  (let [readme (str (slurp "README.org") "\n")
        readme-lines (line-count readme)
        repeats (long (Math/ceil (/ min-large-lines (double readme-lines))))]
    (apply str (repeat repeats readme))))

(defn- fixtures []
  [{:name :fixture-minimal
    :content (slurp "test/org_parser/fixtures/minimal.org")}
   {:name :fixture-bold-text
    :content (slurp "test/org_parser/fixtures/bold_text.org")}
   {:name :fixture-headlines-and-tables
    :content (slurp "test/org_parser/fixtures/headlines_and_tables.org")}
   {:name :fixture-schedule-with-repeater
    :content (slurp "test/org_parser/fixtures/schedule_with_repeater.org")}
   {:name :edge-headline-umlaut
    :content "***** hello wörld⛄ :"}
   {:name :large-readme-derived
    :content (large-readme-content)}])

(defn- benchmark-one [f warmup-runs measure-runs]
  (dotimes [_ warmup-runs]
    (f))
  (let [runs (for [_ (range measure-runs)]
               (let [start (now-nanos)]
                 (f)
                 (elapsed-ms start)))]
    (vec runs)))

(defn- parse-only [s]
  (let [result (parser/parse s)]
    (when (parser/failure? result)
      (throw (ex-info "benchmark parse failed"
                      (select-keys result [:backend :reason :message :start]))))
    result))

(defn- summarize-telemetry [{:keys [events] :as telemetry}]
  (-> telemetry
      (assoc :event-count (count events))
      (dissoc :events)))

(defn- parse-only-telemetry [s]
  (-> (antlr-parser/with-telemetry #(parse-only s))
      :telemetry
      summarize-telemetry))

(defn- parse-and-transform [s]
  (core/read-str s))

(defn run-benchmarks
  [{:keys [warmup-runs measure-runs]
    :or {warmup-runs default-warmup-runs
         measure-runs default-measure-runs}}]
  (let [cases (fixtures)]
    {:settings {:warmup-runs warmup-runs
                :measure-runs measure-runs}
     :results
     (mapv
       (fn [{:keys [name content]}]
         {:case name
          :input-bytes (count content)
          :parse-only-telemetry (parse-only-telemetry content)
          :parse-only (->> (benchmark-one #(parse-only content) warmup-runs measure-runs)
                           (summarize "parse-only"))
          :parse-and-transform (->> (benchmark-one #(parse-and-transform content) warmup-runs measure-runs)
                                   (summarize "parse-and-transform"))})
      cases)}))

(defn- parse-int-arg [s]
  (try
    (Long/parseLong s)
    (catch Exception _
      nil)))

(defn -main [& args]
  (let [warmup-runs (some-> (first args) parse-int-arg)
        measure-runs (some-> (second args) parse-int-arg)
        results (run-benchmarks {:warmup-runs (or warmup-runs default-warmup-runs)
                                 :measure-runs (or measure-runs default-measure-runs)})]
    (pp/pprint results)))
