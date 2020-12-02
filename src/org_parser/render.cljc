(ns org-parser.render
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

(defn edn [x]
  (prn-str x))

(defn json [x]
  #?(:clj (json/write-str x)
     :cljs (.stringify js/JSON (clj->js x))))

(defmulti org-mapper first)

(defmethod org-mapper :headline [[_ line]]
  (str (apply str (repeat (:level line) "*")) " " (:title line)))

(defmethod org-mapper :content [[_ line]]
  line)

(defn org [x]
  (str/join "\n" (map org-mapper x)))
