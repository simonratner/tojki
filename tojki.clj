(defn make-grid
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

(defn neighbours [g x y]
  (let [x- (max 0 (dec x))
        x+ (min (count (first g)) (+ x 2))
        y- (max 0 (dec y))
        y+ (min (count g) (+ y 2))]
    (vec (map #(subvec % x- x+)
               (subvec g y- y+)))))
