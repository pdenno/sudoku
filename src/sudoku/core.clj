(ns sudoku.core)

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
  [p n]
  (let [[row col] (get bindex n)]
    (-> []
        (into (let [r (nth p row)]       [(nth r col) (nth r (+ col 1)) (nth r (+ col 2))]))
        (into (let [r (nth p (+ 1 row))] [(nth r col) (nth r (+ col 1)) (nth r (+ col 2))]))
        (into (let [r (nth p (+ 2 row))] [(nth r col) (nth r (+ col 1)) (nth r (+ col 2))])))))

(defn block-ok?
  "Return true if the block of 9 numbers have no violations."
  [p blk-num]
   (->> (get-block p blk-num)
        (remove #(= % 0))
        (apply distinct?)))

(defn blocks-ok?
  "Return true if all the blocks have no violations."
  [p]
  (every? #(block-ok? p %) (range 1 10)))

(defn puzzle-ok?
  "Return true if the puzzle has no violations."
  [p]
  (and (rows-ok? p)
       (cols-ok? p)
       (blocks-ok? p)))

(defn row-col2block
  "Return the block number corresponding to the argument."
  [row col]
  (+ 1 (* (quot row 3) 3) (quot col 3)))

(defn possible?
  "Return a vector of all the possible values for this position."
  [p row col val]
  (and (->> (nth p row)
            (not-any? #(= % val)))
       (->> (map #(nth % col) p)
            (not-any? #(= % val)))
       (->> (get-block p (row-col2block row col))
            (not-any? #(= % val)))))

(defn possible-vec
  "Return a vector of what values are possible at the argument position."
  [puz row col]
  (reduce (fn [res val]
            (if (possible? puz row col val)
              (conj res val)
              res))
          []
          (range 1 10)))

(defn given?
  [p row col]
  (let [val (nth (nth p row) col)]
    (if (zero? val) false val)))

(defn make-state
  "Make a map describing the state of the argument position."
  [puz row col]
  {:r row :c col :possible (possible-vec puz row col)})

(defn make-problem
  "Create a map that saves the state of puzzle for backtracking."
  [puz]
  (-> 
   (reduce (fn [res [r c]]
                (if (given? puz r c)
                  res
                  (update res :steps conj (make-state puz r c))))
              {:puzzle puz :state [] :steps []}
              (for [row (range 9) col (range 9)] (vector row col)))
      (update :steps (fn [s] (sort-by #(-> % :possible count) s)))))


(defn update-puzzle
  "Returns a prob with the puzzle updated"
  [prob r c val]
  (assoc-in prob [:puzzle r c] val))

;;; This needs to be updated to save the state. 
(defn solve-deductive
  "Update problem with with all deductively solvable steps."
  [prob]
  (reduce (fn [res s]
            (if (== 1 (-> s :possible count)) ; Update the puzzle with this value.
              (assoc res :puzzle
                     (-> (update-puzzle res (:r s) (:c s) (-> s :possible first))
                         :puzzle))
              res)) ; Discard choice-point states; they need to be recalculated. 
          {:puzzle (:puzzle prob) :steps []}
          (:steps prob)))

(defn solve-deductive-loop
  "Runs through a loop of solve-deductive until there are none left or the problem is solved."
  [prob]
  (loop [p prob]
    (if (not-any? #(= 1 (count %)) (map :possible (:steps p)))
      p
      (recur (-> p solve-deductive :puzzle make-problem)))))

(defn bad-choice?
  "Return true if the the puzzle cannot be solved as is."
  [prob]
  (some empty? (->> prob :steps (map :possible))))

(defn backtrack
  "Backtrack out to the last unused choice and use it."
  [prob]
  (loop [p prob]
    (cond (-> p :steps empty?)
          nil, ; No backtracking possible
          (-> p :state first :choice not-empty)         ; non-empty choice point
          (-> p
              (update-puzzle                            ; ...update puzzle...
               (-> p :state first :row)
               (-> p :state first :col)
               (-> p :state first :choice first))
              (update-in [:state 0] #(-> % rest vec))),  ;...update state and return.
          :else ; No choice here, just clear the puzzle state and continue. 
          (recur (-> p
                     (update-puzzle                            ; ...update puzzle...
                      (-> p :state first :row)
                      (-> p :state first :col)
                      0)
                     (update p :state (-> rest vec)))))))

(defn solve
  [prob]
  (loop [p (solve-deductive-loop prob)]
    (if (-> p :steps empty?)
      p, ; solved!
      (recur (if (bad-choice? p) ; Backtrack
               (-> p backtrack solve-deductive-loop)
               (as-> p ?p           ; Go forward from a choice point.
                 (let [row (-> p :steps first :row)
                       col (-> p :steps first :col)
                       choice  (-> p :steps first :possible)
                       others? (pop choice)]
                   (update-puzzle ?p row col (first choice))
                   (update ?p :state conj {:row row :col col :val val :choice choice})
                   (if others?
                     (update ?p :state conj {:row row :col col :possible others?})
                     (update ?p :state pop)))))))))
  
                
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

(def moderate
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
