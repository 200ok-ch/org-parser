(ns org-parser.parser
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            ;; [cljs.nodejs :as nodejs]
            ))

(def fs (js/require "fs"))

(defparser org* (.readFileSync fs "resources/org.ebnf" "utf8"))

(defn parse [& args]
  (-> (apply org* args)
      (vary-meta merge {:raw (first args)})))
