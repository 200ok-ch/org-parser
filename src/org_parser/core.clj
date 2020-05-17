(ns org-parser.core
  (:require [org-parser.parser :as parser]
            [org-parser.transform :as transform]
            [org-parser.render :as render]))


(defn -main [path & args]
  (->> path
       slurp
       parser/org
       transform/transform
       render/text
       println))
