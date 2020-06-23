(ns org-parser.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            org-parser.core-test
            org-parser.parser-test
            org-parser.parser-mean-test
            org-parser.transform-test))


;; This isn't strictly necessary, but is a good idea depending
;; upon your application's ultimate runtime engine.
(enable-console-print!)


(doo-tests
 'org-parser.core-test
 'org-parser.parser-test
 'org-parser.parser-mean-test
 'org-parser.transform-test)
