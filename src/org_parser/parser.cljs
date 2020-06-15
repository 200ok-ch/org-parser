(ns org-parser.parser
  (:require [cljs-node-io.core :refer [slurp]]
            [instaparse.core :as insta :refer-macros [defparser]]))

(defparser org (slurp "resources/org.ebnf"))
