(ns steiner.core
  (:require [steiner.vector :refer :all]))

(defn steiner-point [triangle]
  "steiner-point of a triangle"
  (let [max-angle (* 2/3 Math/PI)
        angs (angles triangle)
        degenerate-index (keep-indexed #(if (> %2 max-angle) %1) angs)]
    (if (seq degenerate-index)
      (nth triangle (first degenerate-index))
      (let [[A B C] triangle
            d60 (* 1/3 Math/PI)
            cross-product (det (v- A B) (v- A C))
            direction (if (> cross-product  0) d60 (- d60))
            C* (rotate-on-point A B (- direction))
            B* (rotate-on-point A C direction)]
        (point-intersection B B* C C*)))))
