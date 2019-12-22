(ns org-parser.core
  (:require [instaparse.core :as insta]
            [org-parser.parser :as parser]
            [clojure.pprint :refer [pprint]]))


(defn -main [path & args]
  (->> path
       slurp
       (insta/parse parser/org)
       pprint))
