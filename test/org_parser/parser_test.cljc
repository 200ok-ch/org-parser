(ns org-parser.parser-test
  (:refer-clojure :exclude [keyword])
  (:require [org-parser.parser :as parser]
            [instaparse.core :as insta]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is testing]])))


;; if parse is successful it returns a vector otherwise a map


(deftest word
  (let [parse #(parser/org % :start :word)]
    (testing "single"
      (is (= ["a"]
             (parse "a"))))
    (testing "single with trailing space"
      (is (map? (parse "ab "))))
    (testing "single with trailing newline"
      (is (map? (parse "a\n"))))))


(deftest tags
  (let [parse #(parser/org % :start :tags)]
    (testing "single"
      (is (= [:tags "a"]
             (parse ":a:"))))
    (testing "multiple"
      (is (= [:tags "a" "b" "c"]
             (parse ":a:b:c:"))))
    (testing "with all edge characters"
      (is (= [:tags "az" "AZ" "09" "_@#%"]
             (parse ":az:AZ:09:_@#%:"))))))


(deftest headline
  (let [parse #(parser/org % :start :head-line)]
    (testing "boring"
      (is (= [:head-line [:stars "*"] [:title "hello" "world"]]
             (parse "* hello world"))))
    (testing "with priority"
      (is (= [:head-line [:stars "**"] [:priority "A"] [:title "hello" "world"]]
             (parse "** [#A] hello world"))))
    (testing "with tags"
      (is (= [:head-line [:stars "***"] [:title "hello" "world"] [:tags "the" "end"]]
             (parse "*** hello world :the:end:"))))
    (testing "with priority and tags"
      (is (= [:head-line [:stars "****"] [:priority "B"] [:title "hello" "world"] [:tags "the" "end"]]
             (parse "**** [#B] hello world :the:end:"))))
    (testing "title cannot have multiple lines"
      (is (map? (parse "* a\nb"))))
    (testing "with comment flag"
      (is (= [:head-line [:stars "*****"] [:comment-token] [:title "hello" "world"]]
             (parse "***** COMMENT hello world"))))))


(deftest sections
  (let [parse parser/org]
    (testing "boring"
      (is (= [:S
              [:head-line [:stars "*"] [:title "hello" "world"]]
              [:content-line "this is the first section"]
              [:head-line [:stars "**"] [:title "and" "this"]]
              [:content-line "is another section"]]
             (parse "* hello world
this is the first section
** and this
is another section"))))
    (testing "boring with empty lines"
      (is (=[:S
             [:head-line [:stars "*"] [:title "hello" "world"]]
             [:content-line "this is the first section"]
             [:empty-line]
             [:head-line [:stars "**"] [:title "and" "this"]]
             [:empty-line]
             [:content-line "is another section"]]
            (parse "* hello world
this is the first section

** and this

is another section"))))))


(deftest affiliated-keyword
  (let [parse #(parser/org % :start :affiliated-keyword-line)]
    (testing "header"
      (is (= [:affiliated-keyword-line [:key "HEADER"] [:value "hello world"]]
             (parse "#+HEADER: hello world"))))
    (testing "name"
      (is (= [:affiliated-keyword-line [:key "NAME"] [:value "hello world"]]
             (parse "#+NAME: hello world"))))
    (testing "PLOT"
      (is (= [:affiliated-keyword-line [:key "PLOT"] [:value "hello world"]]
             (parse "#+PLOT: hello world"))))
    (testing "results"
      (is (= [:affiliated-keyword-line [:key "RESULTS"] [:value "hello world"]]
             (parse "#+RESULTS: hello world"))))
    (testing "results"
      (is (= [:affiliated-keyword-line [:key "RESULTS" [:optional "asdf"]] [:value "hello world"]]
             (parse "#+RESULTS[asdf]: hello world"))))
    (testing "caption"
      (is (= [:affiliated-keyword-line [:key "CAPTION"] [:value "hello world"]]
             (parse "#+CAPTION: hello world"))))
    (testing "caption"
      (is (= [:affiliated-keyword-line [:key "CAPTION" [:optional "qwerty"]] [:value "hello world"]]
             (parse "#+CAPTION[qwerty]: hello world"))))))


;; this is a special case of in-buffer-settings
(deftest todo
  (let [parse #(parser/org % :start :todo-line)]
    (testing "todos"
      (is (= [:todo-line [:todo-state "TODO"] [:done-state "DONE"]]
             (parse "#+TODO: TODO | DONE"))))))


(deftest greater-block-begin
  (let [parse #(parser/org % :start :greater-block-begin-line)]
    (testing "greater-block-begin"
      (is (= [:greater-block-begin-line [:greater-block-name "CENTER"] [:greater-block-parameters "some params"]]
             (parse "#+BEGIN_CENTER some params"))))))


(deftest greater-block-end
  (let [parse #(parser/org % :start :greater-block-end-line)]
    (testing "greater-block-end"
      (is (= [:greater-block-end-line [:greater-block-name "CENTER"]]
             (parse "#+END_CENTER"))))))


(deftest drawer-begin
  (let [parse #(parser/org % :start :drawer-begin-line)]
    (testing "drawer-begin"
      (is (= [:drawer-begin-line [:drawer-name "SOMENAME"]]
             (parse ":SOMENAME:"))))))


(deftest drawer-end
  (let [parse #(parser/org % :start :drawer-end-line)]
    (testing "drawer-end"
      (is (= [:drawer-end-line]
             (parse ":END:"))))))


(deftest dynamic-block-begin
  (let [parse #(parser/org % :start :dynamic-block-begin-line)]
    (testing "dynamic-block-begin"
      (is (= [:dynamic-block-begin-line [:dynamic-block-name "SOMENAME"] [:dynamic-block-parameters "some params"]]
             (parse "#+BEGIN: SOMENAME some params"))))))


(deftest dynamic-block-end
  (let [parse #(parser/org % :start :dynamic-block-end-line)]
    (testing "dynamic-block-end"
      (is (= [:dynamic-block-end-line]
             (parse "#+END:"))))))


(deftest footnote-line
  (let [parse #(parser/org % :start :footnote-line)]
    (testing "footnote with fn label"
      (is (= [:footnote-line [:fn-label "some-label"] [:text [:text-normal "some contents"]]]
             (parse "[fn:some-label] some contents"))))
    (testing "footnote with number label"
      (is (= [:footnote-line [:fn-label "123"] [:text [:text-normal "some contents"]]]
             (parse "[fn:123] some contents"))))
    (testing "invalid footnote with only a number; sorry, this is not a footnote in orgmode"
      (is (insta/failure? (parse "[123] some contents"))))
    ))

(deftest footnote-link
  (let [parse #(parser/org % :start :footnote-link)]
    ;; TODO styled text inside footnote-link is not yet possible because text parses the closing bracket ]
    (testing "footnote link with label"
      (is (= [:footnote-link [:fn-label "123"]]
             (parse "[fn:123]"))))
    (testing "footnote link with label"
      (is (= [:footnote-link "some contents"]
             (parse "[fn::some contents]"))))
    (testing "footnote link with label and text"
      (is (= [:footnote-link [:fn-label "some-label"] "some contents"]
             (parse "[fn:some-label:some contents]"))))
    (testing "footnote link with number and text"
      (is (= [:footnote-link [:fn-label "123"] "some contents"]
             (parse "[fn:123:some contents]"))))
    (testing "footnote link with label and invalid text"
      (is (insta/failure? (parse "[fn:some-label:some [contents]"))))
    (testing "footnote link with label and invalid text"
      (is (insta/failure? (parse "[fn:some-label:some ]contents]"))))
    ))



(deftest list-item-line
  (let [parse #(parser/org % :start :list-item-line)]

    (testing "list-item-line with asterisk"
      (is (= [:list-item-line [:list-item-bullet "*"] [:list-item-contents "a simple list item"]]
             (parse "* a simple list item"))))
    (testing "list-item-line with hyphen"
      (is (= [:list-item-line [:list-item-bullet "-"] [:list-item-contents "a simple list item"]]
             (parse "- a simple list item"))))
    (testing "list-item-line with plus sign"
      (is (= [:list-item-line [:list-item-bullet "+"] [:list-item-contents "a simple list item"]]
             (parse "+ a simple list item"))))
    (testing "list-item-line with counter and dot"
      (is (= [:list-item-line
              [:list-item-counter "1"]
              [:list-item-counter-suffix "."]
              [:list-item-contents "a simple list item"]]
             (parse "1. a simple list item"))))
    (testing "list-item-line with counter and parentheses"
      (is (= [:list-item-line
              [:list-item-counter "1"]
              [:list-item-counter-suffix ")"]
              [:list-item-contents "a simple list item"]]
             (parse "1) a simple list item"))))
    (testing "list-item-line with alphabetical counter and parentheses"
      (is (= [:list-item-line
              [:list-item-counter "a"]
              [:list-item-counter-suffix ")"]
              [:list-item-contents "a simple list item"]]
             (parse "a) a simple list item"))))
    (testing "list-item-line with alphabetical counter and parentheses"
      (is (= [:list-item-line
              [:list-item-counter "A"]
              [:list-item-counter-suffix ")"]
              [:list-item-contents "a simple list item"]]
             (parse "A) a simple list item"))))
    (testing "list-item-line with checkbox"
      (is (= [:list-item-line
              [:list-item-bullet "-"]
              [:list-item-checkbox [:list-item-checkbox-state "X"]]
              [:list-item-contents "a simple list item"]]
             (parse "- [X] a simple list item"))))
    ))


(deftest keyword
  (let [parse #(parser/org % :start :keyword-line)]
    (testing "keyword"
      (is (= [:keyword-line [:keyword-key "HELLO"] [:keyword-value "hello world"]]
             (parse "#+HELLO: hello world"))))))


(deftest node-property
  (let [parse #(parser/org % :start :node-property-line)]
    (testing "node-property"
      (is (= [:node-property-line [:node-property-name "HELLO"]]
             (parse ":HELLO:"))))
    (testing "node-property"
      (is (= [:node-property-line [:node-property-name "HELLO"] [:node-property-plus]]
             (parse ":HELLO+:"))))
    (testing "node-property"
      (is (= [:node-property-line
              [:node-property-name "HELLO"]
              [:node-property-value [:text [:text-normal "hello world"]]]]
             (parse ":HELLO: hello world"))))
    (testing "node-property"
      (is (= [:node-property-line
              [:node-property-name "HELLO"]
              [:node-property-plus]
              [:node-property-value [:text [:text-normal "hello world"]]]]
             (parse ":HELLO+: hello world"))))
    ))

(deftest timestamp
  (let [parse #(parser/org % :start :timestamp)]
    (testing "diary timestamp"
      (is (= [:timestamp [:timestamp-diary ""]]
             (parse "<%%(( <(sexp)().))>"))))
    (testing "date timestamp without day"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"]] [:ts-modifiers]]]]
             (parse "<2020-01-18>"))))
    (testing "date timestamp with day"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"] [:ts-day "Sat"]] [:ts-modifiers]]]]
             (parse "<2020-01-18 Sat>"))))
    (testing "date timestamp with day in other language"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-21"] [:ts-day "Di"]] [:ts-modifiers]]]]
             (parse "<2020-01-21 Di>"))))
    (testing "date timestamp with day containing umlauts"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-21"] [:ts-day "Dönerstag"]] [:ts-modifiers]]]]
             (parse "<2020-01-21 Dönerstag>"))))
    (testing "date timestamp without day and time"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-time "12:00"]] [:ts-modifiers]]]]
             (parse "<2020-01-18 12:00>"))))
    (testing "date timestamp with day and time"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-day "Sat"] [:ts-time "12:00"]] [:ts-modifiers]]]]
             (parse "<2020-01-18 Sat 12:00>"))))
    (testing "date timestamp with day and time with seconds"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-day "Sat"] [:ts-time "12:00:00"]] [:ts-modifiers]]]]
             (parse "<2020-01-18 Sat 12:00:00>"))))

    (testing "timestamp with repeater"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"]]
                                             [:ts-modifiers [:ts-repeater [:ts-repeater-type "+"]
                                                             [:ts-mod-value "1"] [:ts-mod-unit "w"]]]]]]
             (parse "<2020-01-18 +1w>"))))
    (testing "timestamp with warning"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"]]
                                             [:ts-modifiers [:ts-warning [:ts-warning-type "-"]
                                                             [:ts-mod-value "2"] [:ts-mod-unit "d"]]]]]]
             (parse "<2020-01-18 -2d>"))))
    (testing "timestamp with both repeater and warning"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"]]
                                             [:ts-modifiers [:ts-repeater [:ts-repeater-type "+"]
                                                             [:ts-mod-value "1"] [:ts-mod-unit "w"]]
                                              [:ts-warning [:ts-warning-type "-"]
                                               [:ts-mod-value "2"] [:ts-mod-unit "d"]]]]]]
             (parse "<2020-01-18 +1w -2d>"))))
    (testing "timestamp with both warning and repeater"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"]]
                                             [:ts-modifiers [:ts-warning [:ts-warning-type "-"] [:ts-mod-value "2"] [:ts-mod-unit "d"]]
                                              [:ts-repeater [:ts-repeater-type "+"] [:ts-mod-value "1"] [:ts-mod-unit "w"]]]]]]
             (parse "<2020-01-18 -2d +1w>"))))
    (testing "timestamp with time and both warning and repeater"
      (is (= [:timestamp [:timestamp-active [:ts-inner
	                                     [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-time "18:00"]]
	                                     [:ts-modifiers
	                                      [:ts-warning [:ts-warning-type "-"] [:ts-mod-value "2"] [:ts-mod-unit "d"]]
	                                      [:ts-repeater [:ts-repeater-type "+"] [:ts-mod-value "1"] [:ts-mod-unit "w"]]]]]]
             (parse "<2020-01-18 18:00 -2d +1w>"))))

    (testing "timestamp with time span and both warning and repeater"
      (is (= [:timestamp [:timestamp-active [:ts-inner-span
	                                     [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-time "18:00"]]
	                                     [:ts-time "20:00"]
	                                     [:ts-modifiers
	                                      [:ts-warning [:ts-warning-type "-"] [:ts-mod-value "2"] [:ts-mod-unit "d"]]
	                                      [:ts-repeater [:ts-repeater-type "+"] [:ts-mod-value "1"] [:ts-mod-unit "w"]]]]]]
             (parse "<2020-01-18 18:00-20:00 -2d +1w>"))))

    (testing "more than one space between parts of timestamp does not matter"
      (is (= [:timestamp [:timestamp-active [:ts-inner
	                                     [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-time "18:00"]]
	                                     [:ts-modifiers
	                                      [:ts-warning [:ts-warning-type "-"] [:ts-mod-value "2"] [:ts-mod-unit "d"]]
	                                      [:ts-repeater [:ts-repeater-type "+"] [:ts-mod-value "1"] [:ts-mod-unit "w"]]]]]]
             (parse "<2020-01-18    18:00    -2d    +1w>"))))

    (testing "timestamp ranges"
      (is (= [:timestamp [:timestamp-active
	                  [:ts-inner [:ts-inner-wo-time [:ts-date "2020-04-25"]] [:ts-modifiers]]
	                  [:ts-inner [:ts-inner-wo-time [:ts-date "2020-04-28"]] [:ts-modifiers]]]]
             (parse "<2020-04-25>--<2020-04-28>"))))
    (testing "timestamp ranges with times"
      (is (= [:timestamp [:timestamp-active
	                  [:ts-inner [:ts-inner-w-time [:ts-date "2020-04-25"] [:ts-time "08:00"]] [:ts-modifiers]]
	                  [:ts-inner [:ts-inner-w-time [:ts-date "2020-04-28"] [:ts-time "16:00"]] [:ts-modifiers]]]]
             (parse "<2020-04-25 08:00>--<2020-04-28 16:00>"))))

    (testing "inactive timestamps"
      (is (= [:timestamp [:timestamp-inactive [:ts-inner-span
	                                       [:ts-inner-w-time [:ts-date "2020-01-18"] [:ts-time "18:00"]]
	                                       [:ts-time "20:00"]
	                                       [:ts-modifiers
	                                        [:ts-warning [:ts-warning-type "-"] [:ts-mod-value "2"] [:ts-mod-unit "d"]]
	                                        [:ts-repeater [:ts-repeater-type "+"] [:ts-mod-value "1"] [:ts-mod-unit "w"]]]]]]
             (parse "[2020-01-18 18:00-20:00 -2d +1w]"))))

    (testing "syntactically wrong timestamp"
      (is (insta/failure? (parse "<2020-04-25 day wrong>"))))

    (testing "at-least modifier for habits"
      (is (= [:timestamp [:timestamp-active [:ts-inner
	                                     [:ts-inner-wo-time [:ts-date "2009-10-17"] [:ts-day "Sat"]]
	                                     [:ts-modifiers [:ts-repeater
	                                                     [:ts-repeater-type ".+"] [:ts-mod-value "2"] [:ts-mod-unit "d"]
	                                                     [:ts-mod-at-least [:ts-mod-value "4"] [:ts-mod-unit "d"]]]]]]]
             (parse "<2009-10-17 Sat .+2d/4d>"))))

    (testing "accept seconds in time"
      (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-w-time
	                                                [:ts-date "2009-10-17"] [:ts-day "Sat"] [:ts-time "15:30:55"]] [:ts-modifiers]]]]
             (parse "<2009-10-17 Sat 15:30:55>"))))

    (testing "missing leading zeros in time are no problem"
      (is (= [:timestamp [:timestamp-active [:ts-inner
	                                     [:ts-inner-w-time [:ts-date "2009-10-17"] [:ts-day "Sat"] [:ts-time "8:00"]] [:ts-modifiers]]]]
             (parse "<2009-10-17 Sat 8:00>"))))

    (testing "newlines are not recognized as space \\s"
      ;; http://xahlee.info/clojure/clojure_instaparse.html
      (is (insta/failure? (parse "<2020-04-17 F\nri>"))))
    (testing "newlines are not recognized as space"
      ;; http://xahlee.info/clojure/clojure_instaparse.html
      (is (insta/failure? (parse "<2020-04-17\nFri>"))))))

(deftest timestamp
  (let [parse #(parser/org % :start :ts-time)]
    (testing "parse time"
      (is (= [:ts-time "08:00"]
             (parse "08:00"))))
    (testing "parse time without leading zero"
      (is (= [:ts-time "8:00"]
             (parse "8:00"))))
    (testing "parse time with seconds"
      (is (= [:ts-time "08:00:00"]
             (parse "08:00:00"))))
    (testing "parse time a.m."
      (is (= [:ts-time "8:00AM"]
             (parse "8:00AM"))))
    (testing "parse time p.m."
      (is (= [:ts-time "08:00pm"]
             (parse "08:00pm"))))))



(deftest literal-line
  (let [parse #(parser/org % :start :fixed-width-line)]
    (testing "parse fixed-width line starting with colon"
      (is (= [:fixed-width-line [:fw-head ":"] [:fw-rest " "]]
             (parse ": "))))
    (testing "parse fixed-width line starting with colon"
      (is (= [:fixed-width-line [:fw-head ":"] [:fw-rest ""]]
             (parse ":"))))
    (testing "parse fixed-width line starting with colon"
      (is (= [:fixed-width-line [:fw-head ":"] [:fw-rest " literal text"]]
             (parse ": literal text"))))
    (testing "parse fixed-width line starting with spaces"
      (is (= [:fixed-width-line [:fw-head "  :"] [:fw-rest " literal text "]]
             (parse "  : literal text "))))
    (testing "parse fixed-width line starting with colon"
      (is (insta/failure? (parse ":literal text"))))
    ))

(deftest links
  (let [parse #(parser/org % :start :link-format)]
    (testing "parse simple link"
      (is (= [:link-format [:link [:link-ext [:link-ext-other
	                                      [:link-url-scheme "https"]
	                                      [:link-url-rest "//example.com"]]]]]
             (parse "[[https://example.com]]"))))
    (testing "parse simple link that looks like an web address but is not valid"
      (is (= [:link-format [:link [:link-int [:link-file-loc-string "www.example.com"]]]]
             (parse "[[www.example.com]]"))))
    (testing "parse link with description"
      (is (= [:link-format [:link [:link-ext [:link-ext-other
	                                      [:link-url-scheme "https"]
	                                      [:link-url-rest "//example.com"]]]]
	      [:link-description "description words"]]
             (parse "[[https://example.com][description words]]"))))
    (testing "parse id link"
      (is (= [:link-format [:link [:link-ext [:link-ext-id "abc-123"]]]]
             (parse "[[id:abc-123]]"))))
    (testing "parse internal * link"
      (is (= [:link-format [:link [:link-int [:link-file-loc-customid "my-custom-id"]]]]
             (parse "[[#my-custom-id]]"))))
    (testing "parse internal # link"
      (is (= [:link-format [:link [:link-int [:link-file-loc-headline "My Header"]]]]
             (parse "[[*My Header]]"))))
    (testing "parse internal link"
      (is (= [:link-format [:link [:link-int [:link-file-loc-string "A Name"]]]]
             (parse "[[A Name]]"))))
    ))

(deftest id-links
  (let [parse #(parser/org % :start :link-ext-id)]
    (testing "invalid id link"
      (is (insta/failure? (parse "[[id:]]"))))
    (testing "invalid id link"
      (is (insta/failure? (parse "[[id:z]]"))))
    ))

(deftest links-with-escapse
  (let [parse #(parser/org % :start :link-format)]
    ;; remember that "\\" is one backslash!
    (testing "parse link with just one literal backslash"
      (is (insta/failure? (parse "[[\\]]"))))
    (testing "parse link with escaped backslash"
      (is (= [:link-format [:link [:link-int [:link-file-loc-string "\\\\"]]]]
             (parse "[[\\\\]]"))))
    (testing "parse link with unescaped backslash"
      (is (= [:link-format [:link [:link-int [:link-file-loc-string "\\a"]]]]
             (parse "[[\\a]]"))))
    (testing "parse link with unescaped opening bracket"
      (is (insta/failure? (parse "[[a[b]]"))))
    (testing "parse link with escaped opening bracket"
      (is (= [:link-format [:link [:link-int [:link-file-loc-string "\\["]]]]
             (parse "[[\\[]]"))))
    (testing "parse link with escaped closing bracket"
      (is (= [:link-format [:link [:link-int [:link-file-loc-string "\\]"]]]]
             (parse "[[\\]]]"))))
    ))

(deftest links-external-file
  (let [parse #(parser/org % :start :link-ext-file)]
    (testing "parse file link"
      (is (= [:link-ext-file "folder/file.txt"]
             (parse "file:folder/file.txt"))))
    (testing "parse file link"
      (is (= [:link-ext-file "~/folder/file.txt"]
             (parse "file:~/folder/file.txt"))))
    (testing "parse file link containing single colons"
      (is (= [:link-ext-file "~/fol:der/fi:le.txt"]
             (parse "file:~/fol:der/fi:le.txt"))))
    (testing "parse relative file link"
      (is (= [:link-ext-file "./folder/file.txt"]
             (parse "./folder/file.txt"))))
    (testing "parse absolute file link"
      (is (= [:link-ext-file "/folder/file.txt"]
             (parse "/folder/file.txt"))))
    (testing "parse file link with line number"
      (is (= [:link-ext-file "./file.org" [:link-file-loc-lnum "15"]]
             (parse "./file.org::15"))))
    (testing "parse file link with text search string"
      (is (= [:link-ext-file "./file.org" [:link-file-loc-string "foo bar"]]
             (parse "./file.org::foo bar"))))
    (testing "parse file link with text search string containing ::"
      ;; this matches orgmode behavior
      (is (= [:link-ext-file "./file.org" [:link-file-loc-string "foo::bar"]]
             (parse "./file.org::foo::bar"))))
    (testing "parse file link with headline"
      (is (= [:link-ext-file "./file.org" [:link-file-loc-headline "header1: test"]]
             (parse "./file.org::*header1: test"))))
    (testing "parse file link with custom id"
      (is (= [:link-ext-file "./file.org" [:link-file-loc-customid "custom-id"]]
             (parse "./file.org::#custom-id"))))))

(deftest links-external-other-url
  (let [parse #(parser/org % :start :link-ext-other)]
    (testing "parse simple link that looks like an web address but is not valid"
      (is (insta/failure? (parse "www.example.com"))))
    (testing "parse other http link"
      (is (= [:link-ext-other [:link-url-scheme "https"] [:link-url-rest "//example.com"]]
             (parse "https://example.com"))))
    (testing "parse other mailto link"
      (is (= [:link-ext-other [:link-url-scheme "mailto"] [:link-url-rest "info@example.com"]]
             (parse "mailto:info@example.com"))))
    (testing "parse other link with uncommon scheme"
      (is (= [:link-ext-other [:link-url-scheme "zyx"] [:link-url-rest "rest-of uri ..."]]
             (parse "zyx:rest-of uri ..."))))))



(deftest text-styled
  (let [parse #(parser/org % :start :text-styled)]
    (testing "parse bold text"
      (is (= [:text-styled [:text-sty-bold [:text [:text-normal "bold text"]]]]
             (parse "*bold text*"))))
    (testing "parse italic text"
      (is (= [:text-styled [:text-sty-italic [:text [:text-normal "italic text"]]]]
             (parse "/italic text/"))))
    (testing "parse underlined text"
      (is (= [:text-styled [:text-sty-underlined [:text [:text-normal "underlined text"]]]]
             (parse "_underlined text_"))))
    (testing "parse verbatim text"
      (is (= [:text-styled [:text-sty-verbatim "verbatim /abc/ text"]]
             (parse "=verbatim /abc/ text="))))
    (testing "parse code text"
      (is (= [:text-styled [:text-sty-code "code *abc* text"]]
             (parse "~code *abc* text~"))))
    (testing "parse strike-through text"
      (is (= [:text-styled [:text-sty-strikethrough [:text [:text-normal "strike-through text"]]]]
             (parse "+strike-through text+"))))
    ;; parse reluctant
    ;; (testing "parse text-styled alone is not reluctant"
    ;;   (is (not (insta/failure? (parse "/italic/ italic/")))))
    (testing "parse verbatim text reluctantly"
      (is (insta/failure? (parse "=verbatim= text="))))

    ;; parse special cases
    (testing "not parse empty verbatim text"
      (is (insta/failure? (parse "=="))))
    (testing "not parse verbatim text with space around"
      (is (insta/failure? (parse "=verbatim ="))))
    (testing "not parse verbatim text with space around"
      (is (insta/failure? (parse "= verbatim="))))
    (testing "parse verbatim text"
      (is (= [:text-styled [:text-sty-verbatim "verbatim = text"]]
             (parse "=verbatim = text="))))
    (testing "parse verbatim text"
      (is (= [:text-styled [:text-sty-verbatim "="]]
             (parse "==="))))
    (testing "parse verbatim text"
      (is (= [:text-styled [:text-sty-verbatim "a"]]
             (parse "=a="))))
    ))

(deftest text-link
  (let [parse #(parser/org % :start :text-link)]
    (testing "parse angled link"
      (is (= [:text-link [:text-link-angle
                          [:link-url-scheme "http"]
                          [:text-link-angle-path "//example.com/foo?bar=baz&baz=bar"]]]
             (parse "<http://example.com/foo?bar=baz&baz=bar>"))))
    (testing "parse plain link"
      (is (= [:text-link [:text-link-plain
                          [:link-url-scheme "http"]
                          [:text-link-plain-path "//example.com/foo?bar=baz&baz=bar"]]]
             (parse "http://example.com/foo?bar=baz&baz=bar"))))
    ))

(deftest text
  (let [parse #(parser/org % :start :text)]
    (testing "parse text that contains style delimiter"
      (is (= [:text [:text-normal "a"] [:text-normal "/b"]]
             (parse "a/b"))))
    (testing "parse text that contains style delimiter"
      (is (= [:text [:text-normal "a "] [:text-normal "/b"]]
             (parse "a /b"))))
    (testing "parse styled text alone"
      (is (= [:text [:text-styled [:text-sty-bold [:text [:text-normal "bold text"]]]]]
             (parse "*bold text*"))))
    (testing "if given multi-line text, parse bold text" ;; normally, parsing text is line-based
      (is (= [:text [:text-styled [:text-sty-bold [:text [:text-normal "a\nb"]]]]]
             (parse "*a\nb*"))))
    (testing "parse styled text followed by normal text"
      (is (= [:text [:text-styled [:text-sty-bold [:text [:text-normal "bold text"]]]]
              [:text-normal " normal text"]]
             (parse "*bold text* normal text"))))
    (testing "parse normal text followed by styled text"
      (is (= [:text [:text-normal "normal text "]
              [:text-styled [:text-sty-bold [:text [:text-normal "bold text"]]]]]
             (parse "normal text *bold text*"))))
    (testing "parse styled text surrounded by normal text"
      (is (= [:text
              [:text-normal "normal text "]
              [:text-styled [:text-sty-bold [:text [:text-normal "bold text"]]]]
              [:text-normal " more text"]]
             (parse "normal text *bold text* more text"))))
    (testing "parse styled text reluctant"
      (is (= [:text [:text-styled [:text-sty-bold [:text [:text-normal "bold text"]]]]
              [:text-normal " text"]
              [:text-normal "*"]]
             (parse "*bold text* text*"))))
    ;; TODO parse only when "surrounded" by delimiter
    ;; (testing "parse italic text"
    ;;   (is (= [:text [:text-styled [:text-sty-italic
    ;;                         [:text [:text-normal "italic "] [:text-normal "/ text"]]]]]
    ;;          (parse "/italic / text/"))))
    (testing "parse angled text link surrounded by normal text"
      (is (= [:text
              [:text-normal "normal text "]
              [:text-link [:text-link-angle
                           [:link-url-scheme "http"]
                           [:text-link-angle-path "//example.com"]]]
              [:text-normal " more text"]]
             (parse "normal text <http://example.com> more text"))))
    ;; TODO (testing "parse normal text link surrounded by normal text"
    ;;   (is (= [:text
    ;;           [:text-normal "normal text "]
    ;;           [:text-link [:text-link-plain "http://example.com"]]
    ;;           [:text-normal " more text"]]
    ;;          (parse "normal text http://example.com more text"))))
    (testing "parse link surrounded by normal text"
      (is (= [:text
              [:text-normal "normal text "]
              [:link-format [:link [:link-ext [:link-ext-other
                                               [:link-url-scheme "http"] [:link-url-rest "//example.com"]]]]]
              [:text-normal " more text"]]
             (parse "normal text [[http://example.com]] more text"))))
    (testing "parse link followed by footnote"
      (is (= [:text
              [:text-normal "normal text "]
              [:link-format [:link [:link-ext [:link-ext-other
	                                       [:link-url-scheme "http"]
	                                       [:link-url-rest "//example.com"]]]]]
	      [:footnote-link "reserved"]]
             (parse "normal text [[http://example.com]][fn::reserved]"))))
    ;; TODO this is not a subscript (space before)
    ;; (testing "parse non-subscript"
    ;;   (is (= [:text [:text-normal "text _abc"]]
    ;;          (parse "text _abc"))))
    (testing "parse subscript"
      (is (= [:text [:text-normal "text"] [:text-sub [:text-subsup-word "abc"]]]
             (parse "text_abc"))))
    (testing "parse superscript"
      (is (= [:text [:text-normal "text"] [:text-sup [:text-subsup-word "123"]]]
             (parse "text^123"))))
    (testing "parse sub- and superscript"
      (is (= [:text [:text-normal "text"]
              [:text-sup [:text-subsup-word "abc"]]
              [:text-sub [:text-subsup-curly "123"]]]
             (parse "text^abc_{123}"))))

    ;; line breaks
    (testing "parse text followed by line break"
      (is (= [:text [:text-normal "abc "] [:text-linebreak [:text-linebreak-after "  "]]]
             (parse "abc \\\\  "))))
    (testing "parse text followed by line break"
      (is (= [:text [:text-normal "abc "] [:text-normal "\\"] [:text-normal "\\ xyz"]]
             (parse "abc \\\\ xyz"))))

    ;; macros
    (testing "parse macro"
      (is (= [:text [:text-normal "text"] [:text-macro
                                           [:macro-name "my_macro5"]
                                           [:macro-args "0" "'{abc}'"]]]
             (parse "text{{{my_macro5(0,'{abc}')}}}"))))

    ;; targets and radio targets
    (testing "parse target"
      (is (= [:text [:text-normal "text"] [:text-target [:text-target-name "my target"]]]
             (parse "text<<my target>>"))))
    (testing "parse radio target"
      (is (= [:text [:text-normal "text"] [:text-radio-target [:text-target-name "my target"]]]
             (parse "text<<<my target>>>"))))

    ;; entities
    (testing "parse entity"
      (is (= [:text [:text-normal "text"]
              [:text-entity [:entity-name "Alpha"]]
              [:text-normal "-followed"]]
             (parse "text\\Alpha-followed"))))
    (testing "parse entity"
      (is (= [:text [:text-normal "text "]
              [:text-entity [:entity-name "Alpha"] [:entity-braces]]
              [:text-normal "followed"]]
             (parse "text \\Alpha{}followed"))))

    ))


(deftest text-macros
  (let [parse #(parser/org % :start :text-macro)]
    (testing "parse macro without args"
      (is (= [:text-macro [:macro-name "my_macro5"] [:macro-args ""]]
             (parse "{{{my_macro5()}}}"))))
    (testing "parse macro with arg"
      (is (= [:text-macro [:macro-name "my_macro5"] [:macro-args "arg"]]
             (parse "{{{my_macro5(arg)}}}"))))
    (testing "parse macro"
      (is (= [:text-macro [:macro-name "my_macro5"] [:macro-args "x\\,y" " (0)", "'{abc}'"]]
             (parse "{{{my_macro5(x\\,y, (0),'{abc}')}}}"))))
    ))

(deftest text-entities
  (let [parse #(parser/org % :start :text-entity)]
    (testing "parse entity"
      (is (= [:text-entity [:entity-name "Alpha"]]
             (parse "\\Alpha"))))
    (testing "parse entity"
      (is (= [:text-entity [:entity-name "Alpha"] [:entity-braces]]
             (parse "\\Alpha{}"))))
    ))

(deftest text-targets
  (let [parse #(parser/org % :start :text-target)]
    (testing "parse target"
      (is (= [:text-target [:text-target-name "t"]]
             (parse "<<t>>"))))
    (testing "parse invalid target"
      (is (insta/failure?
           (parse "<< t>>"))))
    (testing "parse invalid target"
      (is (insta/failure?
           (parse "<<t >>"))))
    (testing "parse invalid target"
      (is (insta/failure?
           (parse "<< >>"))))
    ))

(deftest text-subscript
  (let [parse #(parser/org % :start :text-sub)]
    ;; TODO make sure preceeding character is non-whitespace
    (testing "parse subscript word"
      (is (= [:text-sub [:text-subsup-word "abc"]]
             (parse "_abc"))))
    (testing "parse subscript number"
      (is (= [:text-sub [:text-subsup-word "123"]]
             (parse "_123"))))
    (testing "parse subscript word/number mixed"
      (is (= [:text-sub [:text-subsup-word "1a2b"]]
             (parse "_1a2b"))))
    (testing "parse subscript star"
      (is (= [:text-sub [:text-subsup-word "*"]]
             (parse "_*"))))
    (testing "parse subscript special"
      (is (= [:text-sub [:text-subsup-word ".,\\a"]]
             (parse "_.,\\a"))))
    (testing "parse subscript special"
      (is (= [:text-sub [:text-subsup-word "-.,\\a"]]
             (parse "_-.,\\a"))))
    (testing "parse subscript special"
      (is (= [:text-sub [:text-subsup-word "+.,\\a"]]
             (parse "_+.,\\a"))))
    (testing "parse subscript in curly braces"
      (is (= [:text-sub [:text-subsup-curly ".,-123abc!"]]
             (parse "_{.,-123abc!}"))))
    (testing "curly braces inside braced subscript are not allowed"
      (is (insta/failure? (parse "text_{{}"))))
    ))
