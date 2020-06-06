(ns org-parser.render
  #?(:clj (:require [clojure.data.json :as json])))

(defn edn [x]
  (str x))

#?(:cljs (defn json [x]
           (.stringify js/JSON (clj->js x))))

#?(:clj (defn text [x]
  (json/write-str x)))
