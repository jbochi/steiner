(ns steiner.core)

(defn v+ [a b]
  "sum vectors"
  (map + a b))

(defn v- [a b]
  "subtract vectors"
  (map - a b))

(defn size [v]
  "size of vector"
  (Math/sqrt (reduce + (map #(* % %) v))))

(defn distance [v1 v2]
  "distance between points"
  (size (v- v1 v2)))

(defn rotate [v theta]
  "rotate vector by theta"
  (let [x (first v)
        y (last v)
        sin (Math/sin theta)
        cos (Math/cos theta)]
  [(- (* x cos) (* y sin))
   (+ (* x sin) (* y cos))]))


(defn rotate-on-point [v1 v2 theta]
  "rotate v2-v1 on point v1 by theta"
  (v+ v1 (rotate (v- v2 v1) theta)))

(defn det [v1 v2]
  "determinant of [x1 y1] [x2 y2]"
  (let [x1 (first v1)
        y1 (last v1)
        x2 (first v2)
        y2 (last v2)]
  (- (* x1 y2) (* y1 x2))))

; http://en.wikipedia.org/wiki/Line-line_intersection
(defn intersection [p1 p2 p3 p4]
  "calculate intersection of lines between points p1-p2 and p3-p4"
  (let [x1 (first p1)
        x2 (first p2)
        x3 (first p3)
        x4 (first p4)
        y1 (last p1)
        y2 (last p2)
        y3 (last p3)
        y4 (last p4)
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


(defn steiner-point [A B C]
  "steiner-point of a triangle"
  (let [d60 (* 1/3 Math/PI)
        C* (rotate-on-point A B (- d60))
        B* (rotate-on-point A C d60)]
  (intersection B B* C C*)))
