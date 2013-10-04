(ns steiner.core
  (:require [steiner.vector :refer :all]))

(def d60 (* 1/3 Math/PI))

(defn steiner-point [triangle]
  "steiner-point of a triangle"
  (let [max-angle (* 2/3 Math/PI)
        angs (angles triangle)
        degenerate-index (keep-indexed #(if (> %2 max-angle) %1) angs)]
    (if (seq degenerate-index)
      (nth triangle (first degenerate-index))
      (let [[A B C] triangle
            cross-product (det (v- A B) (v- A C))
            direction (if (> cross-product  0) d60 (- d60))
            C* (rotate-on-point A B (- direction))
            B* (rotate-on-point A C direction)]
        (point-intersection B B* C C*)))))

(defn steiner-tree [points]
  (let [[A B C D] points
        B* (rotate-on-point A B (- d60))
        S1 (steiner-point [B* C D])
        S2 (steiner-point [A B S1])]
  (reduce + [(distance A S2)
             (distance B S2)
             (distance S2 S1)
             (distance S1 C)
             (distance S1 D)])))
