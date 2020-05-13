(ns org-parser.core
  (:require [org-parser.parser :as parser]
            [org-parser.augmentor :as augmentor]
            [org-parser.render :as render]))


(defn -main [path & args]
  (->> path
       slurp
       parser/org
       augmentor/augment
       render/text
       println))
