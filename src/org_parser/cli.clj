(ns org-parser.cli
  (:gen-class)
  (:require [org-parser.core :as core]
            [org-parser.render :as render]
            [clojure.string :as str]))

(defn -main [path & _args]
  (->> path
       slurp
       core/read-str
       render/json
       str/trim-newline
       println))
