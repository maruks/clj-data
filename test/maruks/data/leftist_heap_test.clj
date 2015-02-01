(ns maruks.data.leftist-heap-test
  (:require [clojure.test :refer :all]
            [maruks.data.leftist-heap :refer :all]
            [criterium.core :refer [bench]]))

(deftest min-heap-test

  (def h (min-heap 7 3 9))
  (def h2 (min-heap 9 3 7))

  (testing "seqable"
    (is (= '(3 7 9) (seq h))))
  
  (testing "stack functions"
    (is (= 3 (peek h)))
    (is (= (list 7 9) (seq (pop h))))
    (is (= 1 (peek (conj h 1))))
    (is (= 3 (count h)))
    (is (not (empty? h)))
    (is (= h (list 3 7 9)))
    (is (= h h2)))

  (testing "object functions"
    (is (not (nil? (.toString h))))
    (is (= (.equals h h2)))
    (is (= (.hashCode h) (.hashCode h2)))))

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
    (is (not (nil? (.toString h))))
    (is (= (.equals h h2)))
    (is (= (.hashCode h) (.hashCode h2)))))

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
    (is (not (nil? (.toString h))))
    (is (= (.equals h h2)))
    (is (= (.hashCode h) (.hashCode h2)))))

(defn- perf-test-conj []
  (bench (peek (reduce conj (min-heap) (range 10000000 20000000 100)))))

(defn- perf-test-heap []
  (bench (peek (apply min-heap (range 10000000 20000000 100)))))
