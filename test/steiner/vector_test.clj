(ns steiner.vector-test
  (:require [expectations :refer :all]
            [steiner.vector :refer :all]))


(defn almost-equal [a b]
  (< (Math/abs (- a b)) 1e-6))

; tests for equilateral triangle
(let [A [0 0]
      B [1 0]
      C [1/2 (Math/sqrt 3/4)]
      sqrt3 (/ 1 (Math/sqrt 3))]
  (expect (almost-equal (/ Math/PI 3) (angle A B C)))
  (expect (almost-equal (/ Math/PI 3) (angle A C B)))
  (expect (almost-equal (/ Math/PI 3) (angle C B A)))
  (expect (almost-equal (/ Math/PI 3) (angle C A B)))
  (expect Math/PI (reduce + (angles [A B C]))))
