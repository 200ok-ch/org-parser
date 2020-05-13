(ns org-parser.augmentor)


(defmulti reducer (fn [_ line] (first line)))


(defmethod reducer :default [result line]
  (conj result line))


(defn augment [x]
  (reduce reducer [] (drop 1 x)))
