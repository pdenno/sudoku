(ns sudoku.core-test
  (:require [clojure.test :refer :all]
            [sudoku.core   :as core]))

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

(deftest a-test
  (testing "Deductive solution"
    (is (=
         [[3 9 1 6 8 4 7 5 2]
          [4 2 5 7 3 9 1 8 6]
          [8 6 7 1 5 2 4 9 3]
          [2 8 3 5 1 6 9 4 7]
          [9 1 4 2 7 8 3 6 5]
          [5 7 6 4 9 3 8 2 1]
          [6 3 9 8 2 1 5 7 4]
          [7 4 8 3 6 5 2 1 9]
          [1 5 2 9 4 7 6 3 8]],
         (:puzzle (core/solve-deductive-loop (core/make-problem example)))))
    (is (= {:puzzle
            [[4 1 5 3 6 9 7 8 8]
             [3 8 9 2 8 7 4 6 5]
             [7 6 9 5 4 0 2 1 3]
             [1 9 4 8 2 5 3 7 6]
             [8 3 7 9 1 6 5 2 4]
             [2 5 6 7 3 4 1 9 9]
             [6 0 3 4 5 1 9 0 7]
             [9 4 2 6 7 3 8 5 1]
             [5 7 1 4 9 8 6 3 2]],
            :steps
            [{:r 2, :c 5, :possible []}
             {:r 6, :c 1, :possible []}
             {:r 6, :c 7, :possible []}]}
           (core/solve-deductive-loop (core/make-problem example-bad))))))

