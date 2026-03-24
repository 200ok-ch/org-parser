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

(defn attach-headline-planning [s-ast parse-direct raw]
  (let [lines (vec (rest s-ast))]
    (loop [idx 0
           acc []]
      (if (>= idx (count lines))
        (with-meta (into [:S] acc) (meta s-ast))
        (let [line (nth lines idx)
              next-line (when (< (inc idx) (count lines)) (nth lines (inc idx)))]
          (if (and (= :headline (first line))
                   next-line
                   (= :content-line (first next-line)))
            (let [next-raw (node-raw next-line raw)
                  planning (parse-direct next-raw :planning)]
              (if (and planning (not (:failure? planning)))
                (recur (+ idx 2) (conj acc (conj line planning)))
                (recur (inc idx) (conj acc line))))
            (recur (inc idx) (conj acc line))))))))

(defn- table-candidate-line? [line raw]
  (and (= :content-line (first line))
       (let [r (node-raw line raw)
             t (str/trim r)]
         (or (re-matches #"\|.*\|" t)
             (re-matches #"\+[-+]+\+" t)
             (re-matches #"#\+TBLFM: .*" t)))))

(defn collapse-table-lines [s-ast parse-direct raw]
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
                  group-raw (str/join "\n" (map #(node-raw % raw) group))
                  table (parse-direct group-raw :table)]
              (if (and table (not (:failure? table)))
                (let [start (first (:span (meta (first group))))
                      end (second (:span (meta (last group))))
                      table* (with-meta table {:span [start end]})]
                  (recur end-idx (conj acc table*)))
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
