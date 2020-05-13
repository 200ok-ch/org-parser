(ns org-parser.transform
  (:require [clojure.string :as str]))


(defmulti reducer (fn [_ line] (first line)))


(defmethod reducer :default [state line]
  (update state :result conj line))


(defn- property [prop props]
  (->> props
       (filter #(= prop (first %)))
       first
       (drop 1)))


(defmethod reducer :head-line [state [_ & properties]]
  (let [level (->> properties
                   (property :stars)
                   first
                   count)
        title (->> properties
                   (property :title)
                   (str/join " "))]
    (conj state [:headline {:level level
                            :title title}])))


(defn- append-content [state text]
  (let [keep (-> state butlast vec)
        content (-> state last last)]
    (conj keep [:content (str content text "\n")])))


(defmethod reducer :content-line [state [_ text]]
  (if (-> state last first (= :content))
    (append-content state text)
    (conj state [:content (str text "\n")])))


(defmethod reducer :empty-line [state [_ _]]
  (reducer state [:content-line ""]))


(defn transform [x]
  (reduce reducer [] (drop 1 x)))
