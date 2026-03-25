(ns org-parser.antlr.postprocess
  (:require [clojure.string :as str]))

(defn drawer-from-s [s-ast]
  (let [lines (rest s-ast)
        begin (first lines)
        end (last lines)
        middle (butlast (rest lines))]
    (if (and (>= (count lines) 2)
             (= :drawer-begin-line (first begin))
             (= :drawer-end-line (first end))
             (every? #(= :content-line (first %)) middle))
      (into [:drawer begin] middle)
      {:failure? true
       :backend :antlr
       :reason :drawer-shape-mismatch})))

(defn dynamic-block-from-s [s-ast]
  (let [lines (rest s-ast)
        begin (first lines)
        end (last lines)
        middle (butlast (rest lines))]
    (if (and (>= (count lines) 2)
             (= :dynamic-block-begin-line (first begin))
             (= :dynamic-block-end-line (first end))
             (every? #(= :content-line (first %)) middle))
      (into [:dynamic-block begin] middle)
      {:failure? true
       :backend :antlr
       :reason :dynamic-block-shape-mismatch})))

(defn block-from-s [s-ast]
  (let [lines (rest s-ast)
        begin (first lines)
        end (last lines)
        middle (butlast (rest lines))]
    (if (and (>= (count lines) 2)
             (= :block-begin-line (first begin))
             (= :block-end-line (first end)))
      [:block (into [:greater-block begin] (concat middle [end]))]
      {:failure? true
       :backend :antlr
       :reason :block-shape-mismatch})))

(defn- node-raw [node raw]
  (let [[start end] (:span (meta node))]
    (subs raw start end)))

(defn attach-headline-planning [s-ast raw]
  (let [lines (vec (rest s-ast))]
    (loop [idx 0
           acc []]
      (if (>= idx (count lines))
        (with-meta (into [:S] acc) (meta s-ast))
        (let [line (nth lines idx)
              next-line (when (< (inc idx) (count lines)) (nth lines (inc idx)))]
          (if (and (= :headline (first line))
                   next-line
                   (= :planning (first next-line)))
            (recur (+ idx 2) (conj acc (conj line next-line)))
            (recur (inc idx) (conj acc line))))))))

(defn- table-candidate-line? [line raw]
  (and (= :content-line (first line))
       (let [r (node-raw line raw)
             t (str/trim r)]
         (or (re-matches #"\|.*\|" t)
             (re-matches #"\+[-+]+\+" t)
             (re-matches #"#\+TBLFM: .*" t)))))

(defn- trimmed-line [line raw]
  (some-> (node-raw line raw) str/trim))

(defn- table-line-kind [raw-line]
  (cond
    (re-matches #"\+[-+]+\+" raw-line) :tableel-sep
    (re-matches #"\|.*\|" raw-line) :bar-line
    (re-matches #"#\+TBLFM: .*" raw-line) :formula
    :else nil))

(defn- table-org-row [raw-line]
  (let [body (subs raw-line 1 (dec (count raw-line)))
        cells (str/split body #"\|" -1)]
    [:table-row (into [:table-row-cells] (map (fn [cell] [:table-cell cell]) cells))]))

(defn- table-from-lines [group raw]
  (let [trimmed (mapv #(trimmed-line % raw) group)
        start (first (:span (meta (first group))))
        end (second (:span (meta (last group))))]
    (when (every? table-line-kind trimmed)
      (let [table (if (= :tableel-sep (table-line-kind (first trimmed)))
                    [:table
                     (into [:table-tableel]
                           (map (fn [line]
                                  (case (table-line-kind line)
                                    :tableel-sep [:table-tableel-sep line]
                                    :bar-line [:table-tableel-line line]))
                                trimmed))]
                    [:table
                     (into [:table-org]
                           (map (fn [line]
                                  (case (table-line-kind line)
                                    :formula [:table-formula (second (re-matches #"#\+TBLFM: (.*)" line))]
                                    :bar-line (if (re-matches #"\|[+-]+\|" line)
                                                [:table-row [:table-row-sep line]]
                                                (table-org-row line))))
                                trimmed))])]
        (with-meta table {:span [start end]})))))

(defn collapse-table-lines [s-ast raw]
  (let [lines (vec (rest s-ast))]
    (loop [idx 0
           acc []]
      (if (>= idx (count lines))
        (with-meta (into [:S] acc) (meta s-ast))
        (let [line (nth lines idx)]
          (if (table-candidate-line? line raw)
            (let [end-idx (loop [j idx]
                            (if (and (< j (count lines))
                                     (table-candidate-line? (nth lines j) raw))
                              (recur (inc j))
                              j))
                  group (subvec lines idx end-idx)
                  table (table-from-lines group raw)]
              (if table
                (recur end-idx (conj acc table))
                (recur (inc idx) (conj acc line))))
            (recur (inc idx) (conj acc line))))))))

(defn strip-nested-s-lines [s-ast]
  (if (= :S (first s-ast))
    (with-meta (into [:S]
                     (remove #(and (sequential? %)
                                   (= :S (first %)))
                             (rest s-ast)))
               (meta s-ast))
    s-ast))
