(ns org-parser.render
  #?(:clj (:require [clojure.data.json :as json])))

(defn edn [x]
  (prn-str x))

(defn json [x]
  #?(:clj (json/write-str x)
     :cljs (.stringify js/JSON (clj->js x))))
