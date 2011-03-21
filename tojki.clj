(defn make-grid
  "Create a grid with dimensions [w h]."
  ([w h]
    (make-grid w h nil))
  ([w h initial-value]
    (vec (map vec (repeat h (repeat w initial-value))))))

(defn print-grid [g]
  (doseq [row g] (println row)))

(defn get-grid
  ([g x y] (get-in g [y x]))
  ([g [x y]] (get-in g [y x])))

(defn set-grid
  ([g x y v] (assoc-in g [y x] v))
  ([g [x y] v] (assoc-in g [y x] v)))

(defn neighbours
  "Returns a subgrid containing immediate neighbours of [x y]."
  [g x y]
  (let [x- (- x 1)
        x+ (+ x 2)
        y- (- y 1)
        y+ (+ y 2)]
    (vec (map #(subvec % x- x+)
               (subvec g y- y+)))))

(defn rotate [n v]
  (let [shift (mod n (count v))]
    (vec (concat (subvec v shift) (subvec v 0 shift)))))

(defn ; boundary
  #^{:test (fn []
      (let [convex [[0 0] [0 1] [1 2] [2 1] [2 0]]
            concave [[0 0] [0 2] [1 1] [2 2] [3 1] [3 0]]]
        (assert (= (boundary #{[0 0] [0 1] [1 0] [1 1] [1 2] [2 0] [2 1]}) convex))
  ))}
  boundary
  "Returns a polygon representing the boundary of the given point set."
  [points]
  ; Start with the left-most point in the set, 12 o'clock direction.
  (loop [result [(apply min-key first points)]
         clockwise [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]]]
    (let [neighbour (fn [dir] (vec (map + (clockwise dir) (peek result))))
          neighbour? (fn [dir] (when (comp points neighbour) dir))]
      (println result clockwise)
      ; Find the first neighbour, clockwise, that belongs to the set.
      ; This is the next point in the boundary.
      (if-let [dir (first (keep neighbour? (range 8)))]
        (do (println "dir" dir)
        (recur
          (conj result (neighbour dir))
          ; Continue search from the neighbour immediately following the
          ; direction we came from.
          (vec (rotate (+ 5 dir) clockwise)))
        )
        result))))

(defn ; in-polygon?
  #^{:test (fn []
      (let [convex [[0 0] [0 1] [1 2] [2 1] [2 0]]
            concave [[0 0] [0 2] [1 1] [2 2] [3 1] [3 0]]]
        (assert (in-polygon? convex 1 1))
        (assert (in-polygon? concave 2 1))
        (assert (not (in-polygon? convex 2 2)))
        (assert (not (in-polygon? concave 1 2)))
  ))}
  in-polygon?
  "Returns true if [x y] lies on the interior of the polygon, false if it is on
  the exterior. Result undefined for points on the polygon boundary. Polygon is
  defined as an open vector of [x y] vertices, may be convex or concave."
  ; Point is on the interior of the polygon if when projected onto the y-axis,
  ; it's x value is below an odd number of polygon edges.
  ; @see http://alienryderflex.com/polygon/
  [poly x y]
  (odd?
    (let [edges (partition 2 1 (conj poly (first poly)))]
      (reduce + (map (fn [[[x1 y1] [x2 y2]]]
        (if (and (not= (> y1 y) (> y2 y))
                 (< x (+ (/ (* (- x2 x1) (- y y1)) (- y2 y1)) x1)))
          1 0)) edges)))))

