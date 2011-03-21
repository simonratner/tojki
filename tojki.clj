(defn make-grid
  "Create a grid with dimensions [w h]."
  ([w h]
    (make-grid w h nil))
  ([w h initial-value]
    (vec (map vec (repeat h (repeat w initial-value))))))

(defn print-grid
  ([g] (doseq [row g] (println row))))

(defn get-grid
  ([g x y] (get-in g [y x])))

(defn set-grid
  ([g x y v] (assoc-in g [y x] v)))

(defn- ; rotate
  #^{:test (fn []
      (assert (= [1 2 3] (rotate 0 [1 2 3])))
      (assert (= [2 3 1] (rotate 1 [1 2 3])))
      (assert (= [2 3 1] (rotate 4 [1 2 3])))
      (assert (= [3 1 2] (rotate -1 [1 2 3])))
      (assert (= [3 1 2] (rotate -4 [1 2 3])))
    )}
  rotate
  "Rotate vector v left by n. Negative values rotate right."
  [n v]
  (let [shift (mod n (count v))]
    (into (subvec v shift) (subvec v 0 shift))))

(defn ; boundary
  #^{:test (fn []
      (let [convex #{[0 0] [0 1] [1 0] [1 1] [1 2] [2 0] [2 1]}
            convex* [[0 1] [0 0] [1 0] [2 0] [2 1] [1 2] [0 1]]
            concave #{[0 0] [0 1] [0 2] [1 0] [1 1] [2 0] [2 1] [2 2] [3 0] [3 1]}
            concave* [[0 2] [0 1] [0 0] [1 0] [2 0] [3 0] [3 1] [2 2] [1 1] [0 2]]
            intersect #{[0 0] [0 2] [1 0] [1 1] [1 2] [2 0] [2 2]}
            intersect* [[0 2] [1 1] [0 0] [1 0] [2 0] [1 1] [2 2] [1 2] [0 2]]]
        (assert (= (boundary #{}) []))
        (assert (= (boundary #{[0 0]}) [[0 0]]))
        (assert (= (boundary convex) convex*))
        (assert (= (boundary concave) concave*))
        (assert (= (boundary intersect) intersect*))
      ))}
  boundary
  "Returns a polygon representing the boundary of the given point set."
  [points]
  (into [] (when (seq points)
    (loop [result [(apply min-key first points)]  ; left-most point
           clockwise [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]]]
      (let [prev (peek result)
            ; Returns a neighbour from the set in the direction dir, or nil.
            neighbour (fn [dir] (points (vec (map + (clockwise dir) prev))))
            ; Find the first neighbour, clockwise, that belongs to the set.
            ; This is the next point in the boundary.
            [p dir] (first (keep #(if-let [p (neighbour %)] [p %]) (range 8)))]
        ; Terminate once we loop around to the start.
        (if-not (and (= prev (first result))
                     (= p (second result)))
          (recur
            (conj result p)
            ; Continue search from the neighbour immediately following the
            ; direction we came from.
            (rotate (+ 5 dir) clockwise))
          result))))))

(defn ; in-polygon?
  #^{:test (fn []
      (let [convex [[0 1] [0 0] [1 0] [2 0] [2 1] [1 2] [0 1]]
            concave [[0 2] [0 1] [0 0] [1 0] [2 0] [3 0] [3 1] [2 2] [1 1] [0 2]]]
        (assert (in-polygon? convex 1 1))
        (assert (in-polygon? concave 2 1))
        (assert (not (in-polygon? [] 0 0)))
        (assert (not (in-polygon? convex 2 2)))
        (assert (not (in-polygon? concave 1 2)))
  ))}
  in-polygon?
  "Returns true if [x y] lies on the interior of the polygon, false if it is on
  the exterior. Result undefined for points on the polygon boundary. Polygon is
  defined as a closed vector of [x y] vertices, may be convex or concave."
  [poly x y]
  ; Point is on the interior of the polygon if when projected onto the y-axis,
  ; it's x value is below an odd number of polygon edges.
  ; @see http://alienryderflex.com/polygon/
  (odd?
    (let [edges (partition 2 1 poly)]
      (reduce + (map (fn [[[x1 y1] [x2 y2]]]
        (if (and (not= (> y1 y) (> y2 y))
                 (< x (+ (/ (* (- x2 x1) (- y y1)) (- y2 y1)) x1)))
          1 0)) edges)))))

