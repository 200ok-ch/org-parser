(ns org-parser.core
  #?(:clj (:gen-class))
  (:require [org-parser.parser :as parser]
            [org-parser.transform :as transform]
            [org-parser.render :as render]
            [clojure.string :as string]
            ))

(defn read-str
  "Reads one ORG value from input String. Takes optional Options."
  [string & options]
  (-> string
      parser/parse
      transform/transform))

#_(read-str "** headline _underlined_ / +strikethrough+  :tag:baz:  \n foo/bar")
#_(read-str "* headline/foo")
#_(read-str "foo/bar")

(defn write-str
  "Converts x to a ORG-formatted string. Takes optional Options."
  [x & options]
  (render/render x))
