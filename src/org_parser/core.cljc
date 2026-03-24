(ns org-parser.core
  #?(:clj (:gen-class))
  (:require [org-parser.parser :as parser]
             [org-parser.transform :as transform]
             [org-parser.render :as render]))

(defn read-str
  "Reads one ORG value from input String. Takes optional Options."
  [string & options]
  (-> string
      (as-> input (apply parser/parse input options))
      transform/transform))

(defn write-str
  "Converts x to a ORG-formatted string. Takes optional Options."
  [x & _options]
  (render/render x))
