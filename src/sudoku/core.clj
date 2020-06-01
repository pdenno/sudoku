(ns sudoku.core
  (:gen-class))

(def example
  [[0 1 5 3 6 0 0 0 0]
   [3 0 0 2 0 7 4 6 5]
   [0 0 0 0 0 0 2 1 0]
   [1 9 0 0 2 0 3 7 6]
   [8 3 0 9 1 0 5 0 4]
   [2 0 6 0 0 4 0 0 0]
   [0 0 3 0 5 1 9 0 7]
   [0 4 2 6 7 3 0 0 1]
   [0 0 0 0 9 8 6 0 2]])

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
  (every? #(block-ok? p %) (range 0 8)))


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

(defn free-score
  "Scores a cell in the puzzle according to how easy it is to solve."
  [p row col]
  (+
   (-> p
      (nth row)
      (->>
       (filter #(= % 0))
       count))
   (->> p
        (map #(nth % col))
        (filter #(= % 0))
        count)
   (->> (get-block p (row-col2block row col))
        (filter #(= % 0))
        count)))

(defn possible?
  "Return a vector of all the possible values for this position."
  [p row col val]
  (and (->> (nth p row)
            (not-any? #(= % val)))
       (->> (map #(nth % col) p)
            (not-any? #(= % val)))
       (->> (get-block p (row-col2block row col))
            (not-any? #(= % val)))))

(defn given?
  [p row col]
  (let [val (nth (nth p row) col)]
    (if (zero? val)
      false
      val)))

#_(defn make-state
  "Make a map describing the state of the argument position."
  [p row col]
  (as-> {:x row :y col} ?s
    (if-let [give (given? p row col)] 
      (assoc ?s :given give)
      ?s)
    (assoc ?s :possible
           (if (:given ?s)
             []
             (reduce (fn [res val]
                       (if (possible? p row col val)
                         (conj res val)
                         res))
                     []
                     (range 1 10))))))

(defn make-state
  "Make a map describing the state of the argument position."
  [p row col]
  (let [give (given? p row col)]
    (cond-> {:x row :y col}
      give (assoc :given give)
      (not give) (assoc :possible (reduce (fn [res val]
                                            (if (possible? p row col val)
                                              (conj res val)
                                              res))
                                          []
                                          (range 1 10))))))


(defn make-problem
  "Create a map that saves the state of puzzle for backtracking."
  [p]
  (as-> {:puzzle p} ?s
    (assoc ?s :state (for [row (range 9)
                           col (range 9)]
                       (make-state p row col)))
    (update ?s :state #(reduce (fn [res s]
                                 (if (contains? s :possible)
                                   (conj res s)
                                   res))
                               []
                               %))))
                
#_(defn make-problem
  "Create a map that saves the state of puzzle for backtracking."
  [p]
  {:puzzle p
   :state (make-state p)})


(Defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
