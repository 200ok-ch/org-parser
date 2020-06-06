(ns org-parser.core
  (:require #?(:cljs [cljs.nodejs :as nodejs])
             #?(:cljs [cljs-node-io.core :refer [slurp]])
            [org-parser.parser :as parser]
            [org-parser.transform :as transform]
            [org-parser.render :as render]))

(defn -main [path & args]
  (->> path
       slurp
       parser/org
       transform/transform
       #?(:clj render/edn)
       #?(:cljs render/json)
       println))

#?(:cljs (nodejs/enable-util-print!))
#?(:cljs (set! *main-cli-fn* -main))
