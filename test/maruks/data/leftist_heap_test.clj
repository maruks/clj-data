(ns maruks.data.leftist-heap-test
  (:require [clojure.test :refer :all]
            [maruks.data.leftist-heap :refer :all]))

(deftest min-heap-test

  (def h (min-heap 7 3 9))
  (def h2 (min-heap 7 3 9))

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
  (def h2 (max-heap 7 3 9))

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
