(ns org-parser.render
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

;; See also export_org.js in https://github.com/200ok-ch/organice for inspirations


;; Convenience output helpers used by the CLI entrypoint.
(defn edn [x]
  (prn-str x))

(defn json [x]
  #?(:clj (json/write-str x)
     :cljs (.stringify js/JSON (clj->js x))))


;; This is currently a minimal implementation of rendering the
;; deserialized org data structure back to org syntax.

(defn- serialize-text-element [[tag text]]
  (case tag
    :text-sty-bold (str "*" text "*")
    :text-sty-italic (str "/" text "/")
    :text-sty-underlined (str "_" text "_")
    :text-sty-strikethrough (str "+" text "+")
    :text-sty-verbatim (str "=" text "=")
    :text-sty-code (str "~" text "~")
    text))

(defn- serialize-text [elements]
  (apply str (map serialize-text-element elements)))

(defn- serialize-headline* [headline]
  (str/join " "
            [(apply str (repeat (:level headline) "*"))
             (serialize-text (:title headline))]))

(defn- serialize-section [{:keys [ast]}]
  ast)

(defn- serialize-headline [{:keys [headline section]}]
  (str/join "\n"
            [(serialize-headline* headline)
             (serialize-section section)]))

(defn render [{:keys [preamble headlines]}]
  (str/join "\n"
            (remove nil?
                    (cons
                     ;; settings serialization is not implemented yet
                     (serialize-section (:section preamble))
                     (map serialize-headline headlines)))))
