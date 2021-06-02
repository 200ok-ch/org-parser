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


#_(transform (org-parser.parser/org "* hello\n** world\n\nasdf"))


(defn- property
  "Takes a PROP (a keyword) and a seq PROPS. Finds the occurence of
  PROP in PROPS and returns a seq of its values."
  [prop props]
  (->> props
       (filter #(= prop (first %)))
       first
       (drop 1)
       vec))

(defn- extract-tags [[_ s]]
  "Given a [:text-normal 'xxx'], return the text-normal without tags
  and the tags as a list."
  (let [[tags & _] (re-find #"\s+(:[a-zA-Z0-9_@#%]+)+:\s*$" s)] ;; find tags by regex
    (if (nil? tags)
      [[:text-normal s] []]
      [[:text-normal (subs s 0 (- (count s) (count tags)))]
       (vec (filter #(not (= % "")) (str/split (str/trim tags) #":" )))])))

#_(->> "test-input" str/trim #(str/split % #":"))
#_(str/split (->> "   :tag1:tag2: " str/trim) #":")
#_(->> "   :tag1:tag2: " str/trim (fn [x] str/split x #":"))
#_(let [[x & _] nil] x)

#_(extract-tags [:text-normal "title   :tag1:tag2:"])

#_(re-find #"\s+(:[a-zA-Z0-9_@#%]+)+:\s*$" "title    :tag:tag:")

(defn- extract-tags-from-text [texts]
  (let [ltext (last texts)]
    (if (= (first ltext) :text-normal)
      (let [[text tags] (extract-tags ltext)]
        [(cons (butlast texts) [text]) tags])
      [texts []])))

#_(extract-tags-from-text [[:text-bold "bold"] [:text-normal "und  :tag:"]])

(defmethod reducer :headline [state [_ & properties] raw]
  (let [level (->> properties
                   (property :stars)
                   first
                   count)
        [title tags] (->> properties (property :text) extract-tags-from-text)
        ;; title2 (reduce text-reducer [] (property :text title))
        ]
    (update state :headlines conjv {:headline {:level level
                                               :title title ;; TODO use reduced title2 instead
                                               :planning (->> properties (property :planning))
                                               :tags  tags
                                               }})))

#_(reducer {} [:headline [:stars "*"] [:text [:text-normal "hello  :tag:"]]
               [:planning
                [:planning-info
                 [:planning-keyword [:planning-kw-closed]]
                 [:timestamp [:timestamp-inactive [:ts-inner [:ts-inner-wo-time [:ts-date "2021-05-22"] [:ts-day "Sat"]] [:ts-modifiers]]]]]]] "")

;; content-line needs to simply drop the keyword
(defmethod reducer :content-line [state [_ ast] raw]
  (append-to-document state ast raw))


(defn- text-reducer [accu element]
  (let [keep (butlast accu)
        [last-key content] (last accu)]
    (if (= last-key (first element))
      (conj keep [last-key (str content (last element))])
      (conj accu element))))

#_(text-reducer [] [:text-normal "asdf"])
#_(text-reducer [[:text-normal "asdf"]] [:text-normal "asdf"])
#_(text-reducer [[:text-bold "asdf"]] [:text-normal "asdf"])
#_(text-reducer [[:text-normal "asdf"]] [:text-bold "asdf"])


;; Merge consecutive :text-normal inside a :text list. They come from
;; the parser stopping at any special character like '*', '/', ...
(defmethod reducer :text [state [_ & ast] raw]
  (append-to-document state (consv :text (reduce text-reducer [] ast)) raw))


(comment
  (reducer {} [:text [:text-normal "a"] [:text-normal "/b"]] "a/b")
  {:preamble {:section {:ast [[:text [:text-normal "a/b"]]], :raw ["a/b"]}}}
  )

(defn- wrap-raw [reducer raw]
  (fn [agg ast]
    (reducer agg ast (apply subs raw (insta/span ast)))))


(defn transform [x]
  (->> x
       (drop 1) ;; drops the initial `:S`
       (reduce (wrap-raw reducer (-> x meta :raw)) {})))
