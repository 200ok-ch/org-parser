(ns org-parser.transform
  (:require [clojure.string :as str]
            [org-parser.parser :as parser]))

;; See also parse_org.js in https://github.com/200ok-ch/organice for inspirations

(def conjv (comp vec conj))


(defmulti reducer
  "The reducer multi method takes a RESULT and an AST and dispatches
  on the first element in AST, which is the type (a keyword) of the
  parsed line, e.g. `:headline`, `:content-line`, etc."
  (fn [_ line _] (first line)))


(defn- append-to-document [state ast raw]
  (let [loc (if-let [headlines (state :headlines)]
              [:headlines (dec (count headlines)) :section]
              [:preamble :section])]
    (-> state
        ;; append line to :ast
        (update-in (conj loc :ast) conjv ast)
        ;; append raw string to :raw
        (update-in (conj loc :raw) conjv raw))))


;; add the given ast to the section of the last headline or to the
;; preamble if there are no headlines yet
(defmethod reducer :default [state ast raw]
  (append-to-document state ast raw))


(defn- property-node
  "Takes a PROP (a keyword) and a seq PROPS. Finds the occurence of
   PROP in PROPS and returns the node."
  [prop props]
  (->> props
       (filter #(= prop (first %)))
       first))

(defn- property
  "Takes a PROP (a keyword) and a seq PROPS. Finds the occurence of
   PROP in PROPS and returns a seq of its values."
  [prop props]
  (->> props
       (property-node prop)
       (drop 1)
       vec))

(defn- extract-tags
  "Given a [:text-normal 'xxx'], return the text-normal without tags
   and a vector of tags."
  [[_ s]]
  (let [[tags & _] (re-find #"\s+(:[a-zA-Z0-9_@#%]+)+:\s*$" s)] ;; find tags by regex
    (if (nil? tags)
      [[:text-normal s] []]
      [[:text-normal (subs s 0 (- (count s) (count tags)))]
       (vec (filter #(not (= % "")) (str/split (str/trim tags) #":" )))])))

(defn- extract-tags-from-text [texts]
  (let [lasttext (last texts)]
    (if (= (first lasttext) :text-normal)
      (let [[text tags] (extract-tags lasttext)]
        [(conjv (vec (butlast texts)) text) tags])
      [texts []])))

(defmethod reducer :headline [state [_ & properties] _raw]
  (let [[title tags] (->> properties (property :text) extract-tags-from-text)]
    (update state :headlines
            conjv {:headline {:level (->> properties (property :level) first)
                              :title title
                              :planning (->> properties (property :planning))
                              :keyword (->> properties
                                            (property :keyword)
                                            first)
                              :priority (->> properties
                                             (property :priority)
                                             first)
                              :commented? (->> properties
                                               (property-node :comment-token)
                                               (seq)
                                               (boolean))
                              :tags tags}})))


;; content-line needs to simply drop the keyword
(defmethod reducer :content-line [state [_ ast] raw]
  (append-to-document state ast raw))


(defn- text-reducer [accu element]
  (case accu
    [] [element]
    (let [[lastkey lastval] (last accu)
          [newkey newval] element]
      (if (and (= lastkey newkey) (= newkey :text-normal))
        (conjv (vec (butlast accu)) [newkey (str lastval newval)])
        (conjv accu element)))))

(defn- merge-consecutive-text-normal
  "Merge consecutive :text-normal inside a :text list. They come from
   the parser stopping at any special character like '*', '/', ..."
  [& elements]
  (vec (concat [:text] (reduce text-reducer [] elements))))

(defn- wrap-raw [reducer raw]
  (fn [agg ast]
    (reducer agg ast (apply subs raw (parser/span ast)))))

(defn- transform-ast [rules node]
  (if (and (vector? node) (keyword? (first node)))
    (let [tag (first node)
          children (map #(transform-ast rules %) (rest node))
          f (get rules tag)]
      (if f
        (let [result (apply f children)]
          (if (vector? result)
            (with-meta result (meta node))
            result))
        (with-meta (into [tag] children) (meta node))))
    node))

(defn transform [x]
  (->> x
       (transform-ast
        {:text merge-consecutive-text-normal
         :title merge-consecutive-text-normal ;; :title just a synonym for :text in a headline
         :stars #(vector :level (count %))
         :timestamp identity
         :macro-args #(vector :macro-args (map str/trim %&))})
       (drop 1) ;; drops the initial `:S`
       (reduce (wrap-raw reducer (-> x meta :raw)) {})))
