(defn make-grid
  "Create a grid with dimensions [w h]."
  ([w h]
    (make-grid w h 0))
  ([w h initial-value]
    (vec (map vec (repeat h (repeat w initial-value))))))

(defn print-grid [g]
  (doseq [row g] (println row)))

(defn get-grid [g x y]
  (get-in g [y x]))

(defn set-grid [g x y v]
  (assoc-in g [y x] v))

(defn neighbours
  "Returns a sub-grid containing immediate neighbours of [x y]."
  [g x y]
  (let [x- (max 0 (dec x))
        x+ (min (count (first g)) (+ x 2))
        y- (max 0 (dec y))
        y+ (min (count g) (+ y 2))]
    (vec (map #(subvec % x- x+)
               (subvec g y- y+)))))

(defn in-polygon
  "Returns true if [x y] is within polygon, false otherwise. Point is within a
  polygon if when projected onto the y-axis, it's x value is below odd number
  of polygon edges; works for convex and concave polygons.
  @see http://alienryderflex.com/polygon/"
  [poly x y]
  (odd?
    (let [edges (partition 2 1 (conj poly (first poly)))]
      (apply + (map (fn [[[x1 y1] [x2 y2]]]
        (if (and (not= (> y1 y) (> y2 y))
                 (< x (+ (/ (* (- x2 x1) (- y y1)) (- y2 y1)) x1)))
          1 0)) edges)))))
