(ns org-parser.transform
  (:require [clojure.string :as str]))


(defmulti reducer
  "The reducer multi method takes a `result` and a `line` and dispatches
  on the first element in `line`, which is the type (a keyword) of the
  parsed line, e.g. `:headline`, `:content-line`, etc."
  (fn [_ line] (first line)))


(defmethod reducer :default [state line]
  (conj state line))


(defn- property
  "Takes a `prop` (a keyword) and a seq `props`. Finds the occurence of
  `prop` in `props` and returns a seq of its values."
  [prop props]
  (->> props
       (filter #(= prop (first %)))
       first
       (drop 1)))


(defmethod reducer :headline [state [_ & properties]]
  (let [level (->> properties
                   (property :stars)
                   first
                   count)
        title (->> properties
                   (property :title)
                   (str/join " "))]
    (conj state [:headline {:level level
                            :title title}])))


(defn- append
  "Takes `state` which is the current result while transforming and a
  content-line's `text`. Adds the `text` to the text of the last entry
  of `state`."
  [state text]
  (let [keep (pop state)
        [key content] (last state)]
    (conj keep [key (str content text "\n")])))


(defmethod reducer :content-line [state [_ text]]
  (if (-> state last first (= :content))
    (append state text)
    (conj state [:content (str text "\n")])))


(defmethod reducer :empty-line [state [_ _]]
  (reducer state [:content-line ""]))


(defn transform [x]
  (->> x
       (drop 1) ;; drops the initial `:S`
       (reduce reducer [])))
