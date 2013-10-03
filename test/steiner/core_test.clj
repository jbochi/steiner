(ns steiner.core-test
  (:require [expectations :refer :all]
            [steiner.core :refer :all]))


; tests for equilateral triangle
(let [A [0 0]
      B [1 0]
      C [1/2 (Math/sqrt 3/4)]
      S (steiner-point [A B C])
      sqrt3 (/ 1 (Math/sqrt 3))]
  (expect (< (- (/ Math/PI 3) (angle A B C) 1e-6)))
  (expect (< (- (/ Math/PI 3) (angle A C B) 1e-6)))
  (expect (< (- (/ Math/PI 3) (angle C B A) 1e-6)))
  (expect (< (- (/ Math/PI 3) (angle C A B) 1e-6)))
  (expect (< (- sqrt3 (distance A S)) 1e-6))
  (expect (< (- sqrt3 (distance B S)) 1e-6))
  (expect (< (- sqrt3 (distance C S)) 1e-6))
  (expect (< (distance S (steiner-point [B A C])) 1e-6))
  (expect (< (distance S (steiner-point [C A B])) 1e-6)))


; test when one angle is greater than 120'
(let [A [0 0]
      B [1 0]
      C [1/2 0.1]
      S (steiner-point [A B C])]
  (expect C S))
