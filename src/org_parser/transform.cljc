(ns org-parser.transform
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))


(def conjv (comp vec conj))


(defmulti reducer
  "The reducer multi method takes a RESULT and an AST and dispatches
  on the first element in AST, which is the type (a keyword) of the
  parsed line, e.g. `:headline`, `:content-line`, etc."
  (fn [_ line _] (first line)))


;; add the given ast to the section of the last headline or to the
;; preamble if there are no headlines yet
(defmethod reducer :default [state ast raw]
  (let [loc (if-let [headlines (state :headlines)]
              [:headlines (dec (count headlines)) :section]
              [:preamble :section])]
    (-> state
        ;; append line to :ast
        (update-in (conj loc :ast) conjv ast)
        ;; append raw string to :raw
        (update-in (conj loc :raw) conjv raw))))


#_(transform (org-parser.parser/org "* hello\n** world\n\nasdf"))


(defn- property
  "Takes a PROP (a keyword) and a seq PROPS. Finds the occurence of
  PROP in PROPS and returns a seq of its values."
  [prop props]
  (->> props
       (filter #(= prop (first %)))
       first
       (drop 1)))


(defmethod reducer :headline [state [_ & properties] raw]
  (let [level (->> properties
                   (property :stars)
                   first
                   count)
        title (->> properties
                   (property :title)
                   (str/join " "))]
    ;; a headline introduces a new headline
    (update state :headlines conjv {:headline {:level level
                                               :title title}})))


;; content-line needs to simply drop the keyword
(defmethod reducer :content-line [state [_ ast] raw]
  (reducer state ast raw))


(defn- wrap-raw [reducer raw]
  (fn [agg ast]
    (reducer agg ast (apply subs raw (insta/span ast)))))


(defn transform [x]
  (->> x
       (drop 1) ;; drops the initial `:S`
       (reduce (wrap-raw reducer (-> x meta :raw)) {})))
