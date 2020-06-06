(ns org-parser.parser
  (:require #?(:clj [instaparse.core :as insta :refer [defparser]])
            #?(:cljs [cljs-node-io.core :refer [slurp]])
            #?(:cljs [instaparse.core :as insta :refer-macros [defparser]])))


(defparser org (slurp "resources/org.ebnf"))
