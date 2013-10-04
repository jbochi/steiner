(ns steiner.vector)

(defn v+ [v1 v2]
  "sum vectors"
  (map + v1 v2))

(defn v- [v1 v2]
  "subtract vectors"
  (map - v1 v2))

(defn size [v]
  "size of vector"
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn normalize [v]
  "normalize vector"
  (let [s (size v)]
    (map #(/ % s) v)))

(defn distance [p1 p2]
  "distance between points"
  (size (v- p1 p2)))

(defn det [p1 p2]
  "determinant of [x1 y1] [x2 y2]"
  (let [[x1 y1] p1
        [x2 y2] p2]
  (- (* x1 y2) (* y1 x2))))

(defn angle [a b c]
  "angle between points B and C around A"
  (let [v1 (v- b a)
        v2 (v- c a)
        n1 (normalize v1)
        n2 (normalize v2)
        [x1 y1] n1
        [x2 y2] n2
        dot-product (+ (* x1 x2) (* y1 y2))]
    (Math/acos dot-product)))

(defn angles [triangle]
  "all angles in a triangle"
  (let [[A B C] triangle]
    (map #(apply angle %) [[A B C] [B A C] [C A B]])))

(defn rotate [p theta]
  "rotate point by theta"
  (let [[x y] p
        sin (Math/sin theta)
        cos (Math/cos theta)]
  [(- (* x cos) (* y sin))
   (+ (* x sin) (* y cos))]))

(defn rotate-on-point [p1 p2 theta]
  "rotate point p2 around point p1 by theta"
  (v+ p1 (rotate (v- p2 p1) theta)))

; http://en.wikipedia.org/wiki/Line-line_intersection
(defn point-intersection [p1 p2 p3 p4]
  "calculate intersection of lines between points p1-p2 and p3-p4"
  (let [[x1 y1] p1
        [x2 y2] p2
        [x3 y3] p3
        [x4 y4] p4
        dp1p2 (det p1 p2)
        dp3p4 (det p3 p4)
        dx1x2 (- x1 x2)
        dx3x4 (- x3 x4)
        dy1y2 (- y1 y2)
        dy3y4 (- y3 y4)
        numx (det [dp1p2 dx1x2] [dp3p4 dx3x4])
        numy (det [dp1p2 dy1y2] [dp3p4 dy3y4])
        dem (det [dx1x2 dy1y2] [dx3x4 dy3y4])]
    [(/ numx dem) (/ numy dem)]))

