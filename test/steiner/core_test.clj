(ns steiner.core-test
  (:require [expectations :refer :all]
            [steiner.core :refer :all]))


(let [A [0 0]
      B [1 0]
      C [1/2 (Math/sqrt 3/4)]
      S (steiner-point A B C)
      sqrt3 (/ 1 (Math/sqrt 3))]
  (expect (< (- sqrt3 (distance A S)) 1e-6))
  (expect (< (- sqrt3 (distance B S)) 1e-6))
  (expect (< (- sqrt3 (distance C S)) 1e-6))
  (expect S (steiner-point B A C))
  (expect (< (distance S (steiner-point B A C)) 1e-6))
  (expect (< (distance S (steiner-point C A B)) 1e-6)))
