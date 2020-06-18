(ns sudoku.core
  (:require [hashp.core])) ; Temporary

(def diag (atom nil))

(defn row-ok?
  "Return true if the row has no violations."
  [p row-num]
  (->> (nth p row-num)
       (remove #(= % 0))
       (apply distinct?)))

(defn rows-ok?
  "Return true if all the rows have no violations."
  [p]
  (every? #(row-ok? p %) (range 0 8)))


(defn col-ok?
  "Return true if the column has no violations."
  [p col-num]
  (->> (map #(nth % col-num) p)
       (remove #(= % 0))
       (apply distinct?)))

(defn cols-ok?
  "Return true if all the columns have no violations."
  [p]
  (every? #(col-ok? p %) (range 0 8)))

(def bindex
  {1 [0 0],
   2 [0 3],
   3 [0 6],
   4 [3 0],
   5 [3 3],
   6 [3 6],
   7 [6 0],
   8 [6 3]
   9 [6 6]})

(defn get-block
  "Return a vector representing the argument block."
  [puz n]
  (let [[row col] (get bindex n)]
    (-> []
        (into (let [r (nth puz row)]       [(nth r col) (nth r (+ col 1)) (nth r (+ col 2))]))
        (into (let [r (nth puz (+ 1 row))] [(nth r col) (nth r (+ col 1)) (nth r (+ col 2))]))
        (into (let [r (nth puz (+ 2 row))] [(nth r col) (nth r (+ col 1)) (nth r (+ col 2))])))))

(defn block-ok?
  "Return true if the block of 9 numbers have no violations."
  [puz blk-num]
   (as-> (get-block puz blk-num) ?b
     (remove #(= % 0) ?b)
     (or (empty? ?b)
         (apply distinct? ?b))))

(defn blocks-ok?
  "Return true if all the blocks have no violations."
  [puz]
  (every? #(block-ok? puz %) (range 1 10)))

(defn puzzle-ok?
  "Return true if the puzzle has no violations."
  [puz]
  (and (rows-ok? puz)
       (cols-ok? puz)
       (blocks-ok? puz)))

(defn row-col2block
  "Return the block number corresponding to the argument."
  [row col]
  (+ 1 (* (quot row 3) 3) (quot col 3)))

(defn not-ok
  "Return the row and column which is not ok."
  [puz]
  (reduce (fn [bad [r c]]
            (if (and (contains? bad :r) (contains? bad :c))
              bad
              (let [bnum (row-col2block r c)]
                (cond-> bad
                  (not (row-ok? puz r)) (assoc :r r)
                  (not (col-ok? puz r)) (assoc :c c)
                  (not (block-ok? puz bnum)) (-> (assoc :r r) (assoc :c c))))))
          {}
          (for [row (range 9) col (range 9)] (vector row col))))

(defn possible?
  "Return true if val is possible in the argument position."
  [puz row col val]
  (and (->> (nth puz row)
            (not-any? #(= % val)))
       (->> (map #(nth % col) puz)
            (not-any? #(= % val)))
       (->> (get-block puz (row-col2block row col))
            (not-any? #(= % val)))))

(defn possible-seq
  "Return a sequence of what values are possible at the argument position."
  [puz row col]
  (reduce (fn [res val]
            (if (possible? puz row col val)
              (conj res val)
              res))
          '()
          (range 1 10)))

(defn given?
  "Return the value or false if it is zero (unspecified)."
  [puz row col]
  (let [val (nth (nth puz row) col)]
    (if (zero? val) false val)))

(defn make-cell-state
  "Make a map describing the state of the argument position."
  [puz row col]
  {:r row :c col :possible (possible-seq puz row col)})

(defn make-problem
  "Create a problem map from a puzzle, setting :steps and initialize :state to ()."
  [puz]
  (-> 
   (reduce (fn [res [r c]]
                (if (given? puz r c)
                  res
                  (update res :steps conj (make-cell-state puz r c))))
              {:puzzle puz :state '() :steps []}
              (for [row (range 9) col (range 9)] (vector row col)))
   (update :steps (fn [s] (sort-by #(-> % :possible count) s)))))

(defn recompute-steps
  "Puzzle has been changed; recalculate :steps, preserve :state."
  [prob]
  (as-> prob ?p
   (dissoc ?p :steps)
   (reduce (fn [prob [r c]]
             (if (given? (:puzzle prob) r c)
               prob
               (update prob :steps conj (make-cell-state (:puzzle prob) r c))))
           ?p
           (for [row (range 9) col (range 9)] (vector row col)))
   (update ?p :steps (fn [s] (sort-by #(-> % :possible count) s)))))

(defn update-cell
  "Return a problem with the puzzle updated."
  [puz r c val]
  (assoc-in puz [r c] val))

;;; POD Should I do a recompute-steps before this. (Processing time goes up). 
(defn solve-deductive-loop
  "Update problem with all deductively solvable steps."
  [prob]
  (loop [p prob]
    (if (or (-> p :steps empty?)
            (> (-> p :steps first :possible count) 1)) p
        (let [{:keys [r c possible]} (-> prob :steps first)]
          (-> p
              (update :puzzle #(update-cell % r c (first possible)))
              (update :state conj {:r r :c c :val (first possible)})
              recompute-steps)))))

(defn bad-choice?
  "Return true if the the puzzle cannot be solved as is."
  [prob]
  (some empty? (->> prob :steps (map :possible))))

(defn backtrack
  "Backtrack out to the last unused choice and use it."
  [prob]
  (loop [p prob]
    (cond (-> p :steps empty?) nil, ; No backtracking possible
          (-> p :state first :choice not-empty)   ; A choice point
          (let [{:keys [r c choice]} (-> p :state first)]
            (-> p
                (update :puzzle #(update-cell % r c (first choice))) ; Update puzzle at choice...
                (update :state                 ; :state is a list!
                        (fn [s] (conj (rest s) ; ...prune the choice made from :choice
                                      {:r r :c c :choice (rest choice)})))
                recompute-steps))
          :else                                      ; No choice here.
          (let [{:keys [r c]} (-> p :state first)]   ; Just back out of puzzle
            (recur (-> p                             ; and continue. (No need to recompute-steps yet.)
                       (update :puzzle #(update-cell % r c 0))
                       (update :state pop)))))))

(defn solve
  "Solve the Sudoku puzzle, if possible."
  [prob]
  (loop [p (-> prob solve-deductive-loop)
         cnt 0] ; Just for debugging
    (cond (-> p :steps empty?) (:puzzle p), ; Solved!
          (not (puzzle-ok? (:puzzle p))) (-> p (assoc :status :BUG!!!) (assoc :bad (not-ok (:puzzle p))))
          (not p)   :unsolvable   ; No steps left, unsolvable.
          :else (recur (if (bad-choice? p)    ; Backtrack
                         (-> p backtrack solve-deductive-loop)
                         (let [{:keys [r c possible]} (-> p :steps first)
                               others (rest possible)]
                           (-> p ; Go forward from a choice point. (pop :steps, push on :state, update cell).
                               (update :puzzle #(update-cell % r c (first possible)))
                               (update :state conj {:r r :c c :val (first possible) :choice others})
                               recompute-steps
                               solve-deductive-loop)))
                       (inc cnt)))))
                
;;;============================================= Testing Stuff =======================================
(def example-bad
  [[0 1 5 3 6 0 0 0 0]
   [3 0 0 2 0 7 4 6 5]
   [0 0 0 0 0 0 2 1 0]
   [1 9 0 0 2 0 3 7 6]
   [8 3 0 9 1 0 5 0 4]
   [2 0 6 0 0 4 0 0 0]
   [0 0 3 0 5 1 9 0 7]
   [0 4 2 6 7 3 0 0 1]
   [0 0 0 0 9 8 6 0 2]])

(def example
  [[3 0 1 6 0 0 7 0 0]
   [4 2 0 0 0 0 1 0 0]
   [0 6 0 0 5 2 0 9 0]
   [0 8 3 5 1 0 0 4 0]
   [0 0 0 0 0 0 3 6 0]
   [5 0 0 4 9 0 0 0 0]
   [0 0 0 0 2 0 0 0 0]
   [7 4 0 0 0 5 0 0 9]
   [0 0 0 0 0 7 0 0 0]])

(def moderate ; puzzle 120. 
  [[0 8 0 0 0 0 0 0 0]
   [0 4 7 8 0 9 0 0 1]
   [0 0 1 4 5 0 0 2 0]
   [8 1 6 7 0 0 5 0 0]
   [9 0 0 0 0 1 0 0 0]
   [0 0 0 5 6 0 0 0 0]
   [0 0 0 0 0 8 0 5 3]
   [0 0 0 0 0 0 0 8 0]
   [0 0 0 3 1 0 0 4 6]])

(def hard
  [[0 1 5 0 0 0 0 0 0]
   [0 0 0 0 0 0 4 0 5]
   [0 0 0 0 0 0 2 0 0]
   [1 0 0 0 0 0 0 0 0]
   [8 0 0 0 0 0 5 0 4]
   [2 0 0 0 0 4 0 0 0]
   [0 0 0 0 5 0 0 0 7]
   [0 4 2 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 2]])

(def prob (make-problem example))
