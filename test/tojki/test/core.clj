(ns tojki.test.core
  (:use [tojki.core])
  (:use [clojure.test]))

(deftest test-rotate
  (testing "Rotate"
    (testing "no-op"
      (is (= [1 2 3] (rotate 0 [1 2 3])))
    )
    (testing "left"
      (is (= [2 3 1] (rotate 1 [1 2 3])))
      (is (= [2 3 1] (rotate 4 [1 2 3])))
    )
    (testing "right"
      (is (= [3 1 2] (rotate -1 [1 2 3])))
      (is (= [3 1 2] (rotate -4 [1 2 3])))
    )
  ))

(deftest test-boundary
  (let [convex #{[0 0] [0 1] [1 0] [1 1] [1 2] [2 0] [2 1]}
        convex* [[0 1] [0 0] [1 0] [2 0] [2 1] [1 2] [0 1]]
        concave #{[0 0] [0 1] [0 2] [1 0] [1 1] [2 0] [2 1] [2 2] [3 0] [3 1]}
        concave* [[0 2] [0 1] [0 0] [1 0] [2 0] [3 0] [3 1] [2 2] [1 1] [0 2]]
        intersecting #{[0 0] [0 2] [1 0] [1 1] [1 2] [2 0] [2 2]}
        intersecting* [[0 2] [1 1] [0 0] [1 0] [2 0] [1 1] [2 2] [1 2] [0 2]]]
    (testing "Boundary of"
      (testing "empty set"
        (is (= (boundary #{}) [])))
      (testing "point"
        (is (= (boundary #{[0 0]}) [[0 0]])))
      (testing "convex polygon"
        (is (= (boundary convex) convex*)))
      (testing "concave polygon"
        (is (= (boundary concave) concave*)))
      (testing "intersecting polygon"
        (is (= (boundary intersecting) intersecting*)))
    )
  ))

(deftest test-in-polygon?
  (let [convex [[0 1] [0 0] [1 0] [2 0] [2 1] [1 2] [0 1]]
        concave [[0 2] [0 1] [0 0] [1 0] [2 0] [3 0] [3 1] [2 2] [1 1] [0 2]]]
    (testing "Inside"
      (testing "convex polygon?"
        (is (in-polygon? convex 1 1)))
      (testing "concave polygon?"
        (is (in-polygon? concave 2 1)))
    )
    (testing "Outside"
      (testing "empty polygon?"
        (is (not (in-polygon? [] 0 0))))
      (testing "convex polygon?"
        (is (not (in-polygon? convex 2 2))))
      (testing "concave polygon?"
        (is (not (in-polygon? concave 1 2))))
    )
  ))
