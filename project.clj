(defproject org-parser "0.1.28-SNAPSHOT"
  :description "A parser for the Org mode markup language for Emacs"
  :url "https://github.com/200ok-ch/org-parser"
  :license {:name "GNU Affero General Public License v3.0"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.866"]
                 [cljs-node-io "1.1.2"]
                 [org.clojure/data.json "1.0.0"]
                 [instaparse "1.4.10"]]
  :main ^:skip-aot org-parser.cli
  :target-path "target/%s"
  :repl-options {:init-ns org-parser.core}
  :plugins [[lein-cljsbuild "1.1.8"]
            [lein-doo "0.1.10"]]
  :profiles {:uberjar {:aot :all}}
  :doo {:build "test-cljs-with-node"}
  :cljsbuild
  {:builds [{:id "main"
             :source-paths ["src"]
             :compiler {:optimizations :none
                        :target :nodejs
                        :output-to "target/org-parser.js"
                        :main org-parser.core
                        :pretty-print true}}
            {:id "test-cljs-with-node"
             :source-paths ["src" "test"]
             :compiler {:main org-parser.test-runner
                        :target :nodejs
                        :output-to
                        "out/test_cljs_with_node.js"
                        :optimizations :none}}]})
