(ns maruks.data.pairing-heap-test
  (:refer-clojure :exclude [merge])
  (:require [clojure.test :refer :all]
            [maruks.data.pairing-heap :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest max-heap-test

  (def h (max-heap 7 3 9))
  (def h2 (max-heap 3 9 7))

  (testing "seqable"
    (is (= '(9 7 3) (seq h))))

  (testing "stack functions"
    (is (= 9 (peek h)))
    (is (= (list 7 3) (seq (pop h))))
    (is (= 10 (peek (conj h 10))))
    (is (= 3 (count h)))
    (is (not (empty? h)))
    (is (= h (list 9 7 3)))
    (is (= h h2)))

  (testing "object functions"
    (is (not (nil? (.toString h))))))

(deftest custom-cmp-heap-test

  (def h (heap #(<= (count %1) (count %2)) "sd" "a" "fgh"))
  (def h2 (heap #(<= (count %1) (count %2)) "a" "fgh" "sd"))

  (testing "seqable"
    (is (= '("a" "sd" "fgh") (seq h))))

  (testing "stack functions"
    (is (= "a" (peek h)))
    (is (= (list "sd" "fgh") (seq (pop h))))
    (is (= "a" (peek (conj h "ok"))))
    (is (= 3 (count h)))
    (is (not (empty? h)))
    (is (= h (list "a" "sd" "fgh")))
    (is (= h h2)))

  (testing "object functions"
    (is (not (nil? (.toString h))))))

(def min-heap-sort-property (prop/for-all
                             [l (gen/not-empty  (gen/list gen/int))]
                             (= (seq (apply min-heap l)) (sort l))))

(def max-heap-sort-property (prop/for-all
                             [l (gen/not-empty (gen/list gen/int))]
                             (= (seq (apply max-heap l)) (reverse (sort l)))))

(defspec min-heap-sort-property-test min-heap-sort-property)

(defspec max-heap-sort-property-test max-heap-sort-property)
