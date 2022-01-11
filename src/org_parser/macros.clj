(ns org-parser.macros
  (:require [clojure.java.io :as io]))

(defmacro inline [path]
  (slurp (io/resource path)))
