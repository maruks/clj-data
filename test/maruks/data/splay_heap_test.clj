(ns maruks.data.splay-heap-test
  (:refer-clojure :exclude [partition merge])
  (:require [clojure.test :refer :all]
            [maruks.data.splay-heap :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [criterium.core :refer [bench]]))

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

(defn- every-node? [pred node]
  (when (pred node)
    (and (pred (.left node))
         (pred (.right node)))))

(defn- rnd-op [h]
  (case (rand-int 3)
    0 (pop h)
    (conj h (rand-int 1000000))))

(defn- rnd-heaps [h n]
  (take n (iterate rnd-op h) ))

(defn elem-pred [cmpfn]
  (fn [n]
    (or (nil? n)
        (and
         (or (nil? (.left n)) (cmpfn (.. n left elem) (.elem n)))
         (or (nil? (.right n)) (cmpfn (.elem n) (.. n right elem)))))))

(def num-of-heaps 10)

(deftest min-heap-properties
  (testing "elem(left) <= elem(parent) <= elem(right)"
    (is (every? (elem-pred <=) (map #(.root %) (rnd-heaps (min-heap) num-of-heaps))))))

(def min-heap-sort-property (prop/for-all
                             [l (gen/not-empty  (gen/list gen/int))]
                             (= (seq (apply min-heap l)) (sort l))))

(def max-heap-sort-property (prop/for-all
                             [l (gen/not-empty (gen/list gen/int))]
                             (= (seq (apply max-heap l)) (reverse (sort l)))))

(defspec min-heap-sort-property-test min-heap-sort-property)

(defspec max-heap-sort-property-test max-heap-sort-property)

(defn- perf-test-conj []
  (bench (peek (reduce conj (min-heap) (range 10000000 20000000 100)))))

(defn- perf-test-heap []
  (bench (peek (apply min-heap (range 10000000 20000000 100)))))
