(ns org-parser.parser
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            ;; [cljs.nodejs :as nodejs]
            [clojure.string :as str]
            [cljs.analyzer :as ana]
            [shadow.resource :refer [inline]]))

(defparser parser (inline "../../resources/org.ebnf"))

(defn parse [& args]
  (-> (apply parser args)
      (vary-meta merge {:raw (first args)})))
