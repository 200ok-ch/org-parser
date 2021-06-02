(ns org-parser.render
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

;; See also export_org.js in https://github.com/200ok-ch/organice for inspirations


;; FIXME: delete the next 2 functions because it's not the
;; responsiblity of org-parser to render edn or json
(defn edn [x]
  (prn-str x))

(defn json [x]
  #?(:clj (json/write-str x)
     :cljs (.stringify js/JSON (clj->js x))))


;; TODO: This is a minimal implementation of rendering the
;; deserialized org data structure back to an org string. This needs
;; to be extenden to the full feature scope.

(defn- serialize-text-element [[tag text]]
  (case tag
    :text-bold (str "*" text "*")
    :text-bold (str "/" text "/")
    :text-bold (str "_" text "_")
    :text-bold (str "+" text "+")
    :text-bold (str "=" text "=")
    :text-bold (str "~" text "~")
    text))

(defn- serialize-text [elements]
  (apply str (map serialize-text-element elements)))

#_(serialize-text [[:text-normal "hello "] [:text-bold "world"] [:asdf "!"]])

(defn- serialize-headline* [headline]
  (str/join " "
            [(apply str (repeat (:level headline) "*"))
             (serialize-text (:text headline))]))

(defn- serialize-section [{:keys [ast]}]
  ast)

(defn- serialize-headline [{:keys [headline section]}]
  (str/join "\n"
            [(serialize-headline* headline)
             (serialize-section section)]))

(defn org [{:keys [settings preamble headlines]}]
  (str/join "\n"
            (remove nil?
                    (cons
                     ;; TODO: serialize settings
                     (serialize-section (:section preamble))
                     (map serialize-headline headlines)))))


#_(org {:headlines [{:headline {:level 1 :title "foo"}}]})
