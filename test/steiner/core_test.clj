(ns steiner.core-test
  (:require [expectations :refer :all]
            [steiner.core :refer :all]
            [steiner.vector :refer :all]))


(defn almost-equal [a b]
  (< (Math/abs (- a b)) 1e-6))

; tests for equilateral triangle
(let [A [0 0]
      B [1 0]
      C [1/2 (Math/sqrt 3/4)]
      S (steiner-point [A B C])
      sqrt3 (/ 1 (Math/sqrt 3))]
  (expect (almost-equal (/ Math/PI 3) (angle A B C)))
  (expect (almost-equal (/ Math/PI 3) (angle A C B)))
  (expect (almost-equal (/ Math/PI 3) (angle C B A)))
  (expect (almost-equal (/ Math/PI 3) (angle C A B)))
  (expect (almost-equal (* Math/PI 2/3) (angle S A B)))
  (expect (almost-equal (* Math/PI 2/3) (angle S A C)))
  (expect (almost-equal (* Math/PI 2/3) (angle S B C)))
  (expect Math/PI (reduce + (angles [A B C])))
  (expect (almost-equal sqrt3 (distance A S)))
  (expect (almost-equal sqrt3 (distance B S)))
  (expect (almost-equal sqrt3 (distance C S)))
  (expect (almost-equal 0 (distance S (steiner-point [B A C]))))
  (expect (almost-equal 0 (distance S (steiner-point [C A B])))))


; test when one angle is greater than 120'
(let [A [0 0]
      B [1 0]
      C [1/2 0.1]
      S (steiner-point [A B C])]
  (expect C S))
