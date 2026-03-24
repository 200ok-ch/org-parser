(ns org-parser.antlr-parser-test
  (:require [clojure.test :refer :all]
            [org-parser.parser :as parser]))

(defn- ast-contains-tag? [ast tag]
  (cond
    (vector? ast) (or (= tag (first ast))
                      (some #(ast-contains-tag? % tag) (rest ast)))
    (sequential? ast) (some #(ast-contains-tag? % tag) ast)
    :else false))

(deftest antlr-headline
  (let [result (parser/parse "* hello world" :start :headline)]
    (is (= [:headline [:stars "*"] [:text [:text-normal "hello world"]]]
           result))))

(deftest antlr-headline-with-keyword-and-priority
  (is (= [:headline [:stars "**"] [:keyword "TODO"] [:priority "A"] [:text [:text-normal "hello"]]]
         (parser/parse "** TODO [#A] hello" :start :headline))))

(deftest antlr-headline-with-comment-token
  (is (= [:headline [:stars "*"] [:keyword "TODO"] [:comment-token] [:text [:text-normal "hello world"]]]
         (parser/parse "* TODO COMMENT hello world" :start :headline))))

(deftest antlr-content-line
  (is (= [:content-line [:text [:text-normal "plain content"]]]
         (parser/parse "plain content" :start :content-line))))

(deftest antlr-full-s
  (is (= [:S
          [:headline [:stars "*"] [:text [:text-normal "hello"]]]
          [:content-line [:text [:text-normal "body"]]]]
         (parser/parse "* hello\nbody" :start :S))))

(deftest antlr-failure-shape
  (let [result (parser/parse "" :start :headline)]
    (is (parser/failure? result))))

(deftest antlr-headline-regression
  (let [result (parser/parse "* TODO COMMENT hello world" :start :headline)]
    (is (= [:headline [:stars "*"] [:keyword "TODO"] [:comment-token] [:text [:text-normal "hello world"]]]
           result))))

(deftest antlr-dynamic-block-start-is-supported
  (let [result (parser/parse "#+BEGIN: abc\n#+END:" :start :dynamic-block)]
    (is (not (parser/failure? result)))
    (is (= [:dynamic-block
            [:dynamic-block-begin-line [:dynamic-block-name "abc"]]]
           result))))

(deftest todo-line-parses-via-antlr
  (let [result (parser/parse "#+TODO: TODO | DONE" :start :todo-line)]
    (is (= [:todo-line [:todo-state "TODO"] [:done-state "DONE"]]
           result))
    (is (= :antlr (-> result meta :backend-used)))))

(deftest antlr-drawer-semantic-block
  (let [result (parser/parse ":MYDRAWER:\nany\ntext\n:END:" :start :drawer)]
    (is (= [:drawer
            [:drawer-begin-line [:drawer-name "MYDRAWER"]]
            [:content-line [:text [:text-normal "any"]]]
            [:content-line [:text [:text-normal "text"]]]]
           result))))

(deftest default-s-uses-antlr-for-simple-docs
  (let [result (parser/parse "* hello\nworld")]
    (is (= :antlr (-> result meta :backend-used)))))

(deftest default-s-uses-antlr-for-table-heavy-input
  (let [content (slurp "test/org_parser/fixtures/headlines_and_tables.org")
        result (parser/parse content)]
    (is (= :antlr (-> result meta :backend-used)))))

(deftest default-s-parses-plain-urls-without-italicizing
  (let [result (parser/parse "The spec: https://orgmode.org/worg/dev/org-syntax.html#Tables")]
    (is (= :antlr (-> result meta :backend-used)))
    (is (ast-contains-tag? result :text-link))
    (is (not (ast-contains-tag? result :text-sty-italic)))))

(deftest default-s-uses-antlr-for-simple-table-doc
  (let [content "* hello\n| col | val |\n| a   | b   |"
        result (parser/parse content)]
    (is (= :antlr (-> result meta :backend-used)))))

(deftest antlr-comment-line
  (is (= [:comment-line [:comment-line-head "#"] [:comment-line-rest " comment"]]
         (parser/parse "# comment" :start :comment-line))))

(deftest antlr-horizontal-rule
  (is (= [:horizontal-rule " -----"]
         (parser/parse " -----" :start :horizontal-rule))))

(deftest antlr-todo-line
  (is (= [:todo-line [:todo-state "TODO"] [:done-state "DONE"]]
         (parser/parse "#+TODO: TODO | DONE" :start :todo-line))))

(deftest antlr-block-begin-line
  (is (= [:block-begin-line [:block-name "CENTER"] [:block-parameters "some params"]]
         (parser/parse "#+BEGIN_CENTER some params" :start :block-begin-line))))

(deftest antlr-block-end-line
  (is (= [:block-end-line [:block-name "CENTER"]]
         (parser/parse "#+END_CENTER" :start :block-end-line))))

(deftest antlr-dynamic-block-begin-line
  (is (= [:dynamic-block-begin-line [:dynamic-block-name "SOMENAME"] [:dynamic-block-parameters "some params"]]
         (parser/parse "#+BEGIN: SOMENAME some params" :start :dynamic-block-begin-line))))

(deftest antlr-dynamic-block-end-line
  (is (= [:dynamic-block-end-line]
         (parser/parse "#+END:" :start :dynamic-block-end-line))))

(deftest antlr-dynamic-block
  (is (= [:dynamic-block
          [:dynamic-block-begin-line [:dynamic-block-name "abc"]]
          [:content-line [:text [:text-normal "multi"]]]
          [:content-line [:text [:text-normal "line"]]]
          [:content-line [:text [:text-normal "content"]]]]
         (parser/parse "#+begin: abc \nmulti\nline\ncontent\n#+end: " :start :dynamic-block))))

(deftest antlr-block
  (is (= [:block [:greater-block
                  [:block-begin-line [:block-name "QUOTE"]]
                  [:content-line [:text [:text-normal "content"]]]
                  [:block-end-line [:block-name "QUOTE"]]]]
         (parser/parse "#+BEGIN_QUOTE \ncontent\n#+end_QUOTE " :start :block))))

(deftest antlr-footnote-line
  (is (= [:footnote-line [:fn-label "some-label"] [:text [:text-normal "some contents"]]]
         (parser/parse "[fn:some-label] some contents" :start :footnote-line))))

(deftest antlr-footnote-link
  (is (= [:footnote-link [:fn-label "123"]]
         (parser/parse "[fn:123]" :start :footnote-link)))
  (is (= [:footnote-link "some contents"]
         (parser/parse "[fn::some contents]" :start :footnote-link)))
  (is (= [:footnote-link [:fn-label "some-label"] "some contents"]
         (parser/parse "[fn:some-label:some contents]" :start :footnote-link))))

(deftest antlr-other-keyword-line
  (is (= [:other-keyword-line [:kw-name "HELLO"] [:kw-value "hello world"]]
         (parser/parse "#+HELLO: hello world" :start :other-keyword-line))))

(deftest antlr-basic-direct-starts
  (is (= '() (parser/parse "\n" :start :eol)))
  (is (= ["abc"] (parser/parse "abc" :start :word)))
  (is (= [:tags "a" "b" "c"] (parser/parse ":a:b:c:" :start :tags)))
  (is (= [:diary-sexp "nr()<n)-h"] (parser/parse "%%(nr()<n)-h" :start :diary-sexp)))
  (is (= [:affiliated-keyword-line [:affil-kw-key "HEADER"] [:kw-value "hello world"]]
         (parser/parse "#+HEADER: hello world" :start :affiliated-keyword-line))))

(deftest antlr-node-property-line
  (is (= [:node-property-line [:node-property-name "HELLO"]]
         (parser/parse ":HELLO:" :start :node-property-line)))
  (is (= [:node-property-line [:node-property-name "HELLO"] [:node-property-plus]]
         (parser/parse ":HELLO+:" :start :node-property-line)))
  (is (= [:node-property-line [:node-property-name "HELLO"] [:node-property-value [:text [:text-normal "hello world"]]]]
         (parser/parse ":HELLO: hello world" :start :node-property-line))))

(deftest antlr-property-drawer
  (is (= [:property-drawer]
         (parser/parse ":PROPERTIES:\n:END:" :start :property-drawer)))
  (is (= [:property-drawer
          [:node-property-line [:node-property-name "text"] [:node-property-plus] [:node-property-value [:text [:text-normal "my value"]]]]
          [:node-property-line [:node-property-name "PRO"] [:node-property-value [:text [:text-normal "abc"]]]]]
         (parser/parse ":PROPERTIES:\n:text+: my value\n:PRO: abc\n:END:" :start :property-drawer))))

(deftest antlr-fixed-width
  (is (= [:fixed-width-line "literal text "]
         (parser/parse "  : literal text " :start :fixed-width-line)))
  (is (= [:fixed-width-area
          [:fixed-width-line "foo "]
          [:fixed-width-line "bar"]]
         (parser/parse " : foo \n : bar" :start :fixed-width-area))))

(deftest antlr-link-direct-starts
  (is (= [:link-ext-other [:link-url-scheme "https"] [:link-url-rest "//example.com"]]
         (parser/parse "https://example.com" :start :link-ext-other)))
  (is (= [:link-ext-id "abc-123"]
         (parser/parse "[[id:abc-123]]" :start :link-ext-id)))
  (is (= [:link-ext-file "./file.org" [:link-file-loc-customid "custom-id"]]
         (parser/parse "./file.org::#custom-id" :start :link-ext-file))))

(deftest antlr-noparse-block
  (is (= [:noparse-block
          [:noparse-block-begin-line [:block-name-noparse "src"]]
          [:noparse-block-content "content\n"]
          [:block-end-line [:block-name "src"]]]
         (parser/parse "#+BEGIN_src\ncontent\n #+END_src" :start :noparse-block)))
  (is (= [:noparse-block
          [:noparse-block-begin-line [:block-name-noparse "example"] [:block-parameters "params! "]]
          [:noparse-block-content ""]
          [:block-end-line [:block-name "example"]]]
         (parser/parse "#+BEGIN_example params! \n#+end_example" :start :noparse-block))))

(deftest antlr-time-and-clock-starts
  (is (= [:ts-time "08:00pm"]
         (parser/parse "08:00pm" :start :ts-time)))
  (is (= [:timestamp-inactive-range
          [:ts-inner-span
           [:ts-inner-w-time [:ts-date "2021-05-22"] [:ts-day "Sat"] [:ts-time "23:26"]]
           [:ts-time "23:46"]
           [:ts-modifiers]]]
         (parser/parse "[2021-05-22 Sat 23:26-23:46]" :start :timestamp-inactive-range)))
  (is (= [:clock
          [:timestamp-inactive-range
           [:ts-inner-w-time [:ts-date "2021-05-22"] [:ts-day "Sat"] [:ts-time "23:26"]]
           [:ts-inner-w-time [:ts-date "2021-05-22"] [:ts-day "Sat"] [:ts-time "23:46"]]]
          [:clock-duration [:clock-dur-hh "0"] [:clock-dur-mm "20"]]]
         (parser/parse "CLOCK: [2021-05-22 Sat 23:26]--[2021-05-22 Sat 23:46] =>  0:20" :start :clock))))

(deftest antlr-planning
  (is (= [:planning
          [:planning-info
           [:planning-keyword [:planning-kw-scheduled]]
           [:timestamp [:timestamp-inactive [:ts-inner [:ts-inner-w-time [:ts-date "2021-05-22"] [:ts-day "Sat"] [:ts-time "23:26"]] [:ts-modifiers]]]]]
          [:planning-info
           [:planning-keyword [:planning-kw-deadline]]
           [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2021-05-22"] [:ts-day "Sat"]] [:ts-modifiers]]]]]]
         (parser/parse "SCHEDULED: [2021-05-22 Sat 23:26]  DEADLINE: <2021-05-22 Sat>" :start :planning))))

(deftest antlr-timestamp-and-inline-primitives
  (is (= [:timestamp [:timestamp-diary "(( <(sexp)().))"]]
         (parser/parse "<%%(( <(sexp)().))>" :start :timestamp)))
  (is (= [:timestamp [:timestamp-active [:ts-inner [:ts-inner-wo-time [:ts-date "2020-01-18"] [:ts-day "Sat"]] [:ts-modifiers]]]]
         (parser/parse "<2020-01-18 Sat>" :start :timestamp)))
  (is (= [:text-entity [:entity-name "Alpha"] [:entity-braces]]
         (parser/parse "\\Alpha{}" :start :text-entity)))
  (is (= [:text-target [:text-target-name "t"]]
         (parser/parse "<<t>>" :start :text-target)))
  (is (= [:text-sub [:text-subsup-curly ".,-123abc!"]]
         (parser/parse "_{.,-123abc!}" :start :text-sub)))
  (is (= [:text-macro [:macro-name "my_macro5"] [:macro-args "arg"]]
         (parser/parse "{{{my_macro5(arg)}}}" :start :text-macro))))

(deftest antlr-list-and-table
  (is (= [:list-item-line
          [:indent ""]
          [:list-item-bullet "-"]
          [:list-item-checkbox [:list-item-checkbox-state "X"]]
          [:list-item-tag "a tag"]
          [:text [:text-normal "a simple list item"]]]
         (parser/parse "- [X] a tag :: a simple list item" :start :list-item-line)))
  (is (= [:table
          [:table-tableel
          [:table-tableel-sep "+---+"]
          [:table-tableel-line "| x |"]
          [:table-tableel-sep "+---+"]]]
         (parser/parse "+---+\n| x |\n+---+\n" :start :table))))

(deftest antlr-link-and-text-style-starts
  (is (= [:link-format [:link [:link-ext [:link-ext-other [:link-url-scheme "https"] [:link-url-rest "//example.com"]]]]]
         (parser/parse "[[https://example.com]]" :start :link-format)))
  (is (= [:text-link [:text-link-angle [:link-url-scheme "http"] [:text-link-angle-path "//example.com/foo?bar=baz&baz=bar"]]]
         (parser/parse "<http://example.com/foo?bar=baz&baz=bar>" :start :text-link)))
  (is (= [[:text-sty-bold "bold text"]]
         (parser/parse "*bold text*" :start :text-styled))))
