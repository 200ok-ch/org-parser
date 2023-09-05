(ns org-parser.transform
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

;; See also parse_org.js in https://github.com/200ok-ch/organice for inspirations

(def conjv (comp vec conj))

(def consv (comp vec cons))


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


#_(transform (org-parser.parser/parse "* hello\n** world\n\nasdf"))


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

;; Unused?
(defn- replace-first-property [elements prop f]
  "In a vector like [[:a 1] [:b 2]], replace the first matching tagged
  list with a new tagged list using the mapper function f."
  (let [head (take-while #(not= (first %) prop) elements)
        tail (drop-while #(not= (first %) prop) elements)]
    (concat head [(f (first tail))] (drop 1 tail))))

(comment
  (replace-first-property [[:a 1] [:b 2] [:c 3] [:d 4]] :b identity)
  (replace-first-property [[:a 1] [:b 2] [:c 3] [:d 4]] :b (fn [_] [:b 100]))
  (vec [:a 1])
  (concat [1 2 3] [4] [5]))

(defn- extract-tags [[_ s]]
  "Given a [:text-normal 'xxx'], return the text-normal without tags
  and a vector of tags."
  (let [[tags & _] (re-find #"\s+(:[a-zA-Z0-9_@#%]+)+:\s*$" s)] ;; find tags by regex
    (if (nil? tags)
      [[:text-normal s] []]
      [[:text-normal (subs s 0 (- (count s) (count tags)))]
       (vec (filter #(not (= % "")) (str/split (str/trim tags) #":" )))])))

(comment
  (extract-tags [:text-normal "title   :tag1:tag2:"])
  (str/split (->> "   :tag1:tag2: " str/trim) #":")
  (let [[x & _] nil] x)
  (re-find #"\s+(:[a-zA-Z0-9_@#%]+)+:\s*$" "title    :tag:tag:"))

(defn- extract-tags-from-text [texts]
  (let [lasttext (last texts)]
    (if (= (first lasttext) :text-normal)
      (let [[text tags] (extract-tags lasttext)]
        [(conjv (vec (butlast texts)) text) tags])
      [texts []])))

#_(extract-tags-from-text [[:text-bold "bold"] [:text-x "foo"] [:text-normal "und  :tag:"]])

(defmethod reducer :headline [state [_ & properties] raw]
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
                              :commented? (->> (doto properties prn)
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
        (conjv (vec (butlast accu)) [newkey (str lastval newval)])  ;; WTF?! without vec in (vec (butlast .)) the output is total crap
        (conjv accu element)))))

#_(reduce text-reducer [] [[:text-underlined "underlined"]
                           [:text-normal "a"]
                           [:text-normal "/"]])

#_(reduce text-reducer [] [[:text-normal "asdf"] [:text-normal "jklö"] [:text-bold "test"]])
#_(reduce text-reducer [] [[:text-normal "z"] [:text-normal "a"] [:text-bold "test"] [:text-normal "0"]])
#_((let [[lastkey lastval] (last [])] [lastkey lastval]))
#_((let [x (last [])] x))

#_(text-reducer [] [:text-normal "asdf"])
#_(text-reducer [[:text-normal "asdf"]] [:text-normal "jklö"])
#_(text-reducer [[:text-bold "asdf"]] [:text-normal "jklö"])
#_(text-reducer [[:text-normal "asdf"]] [:text-bold "jklö"])

(defn- merge-consecutive-text-normal [& elements]
  "Merge consecutive :text-normal inside a :text list. They come from
  the parser stopping at any special character like '*', '/', ..."
  (vec (concat [:text] (reduce text-reducer [] elements))))

#_(apply merge-consecutive-text-normal [[:text-normal "asdf"] [:text-normal "jklö"] [:text-bold "test"]])
#_(apply merge-consecutive-text-normal [[:text-normal "foo "] [:text-normal "bar"] [:text-sty-bold "bar"] [:text-normal " baz"]])

(defn- wrap-raw [reducer raw]
  (fn [agg ast]
    (reducer agg ast (apply subs raw (insta/span ast)))))

(defn transform [x]
  (->> x
       (insta/transform
        {:text merge-consecutive-text-normal
         :title merge-consecutive-text-normal ;; :title just a synonym for :text in a headline
         :stars #(vector :level (count %))
         :timestamp identity
         })
       (drop 1) ;; drops the initial `:S`
       (reduce (wrap-raw reducer (-> x meta :raw)) {})))
