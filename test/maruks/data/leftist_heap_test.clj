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

(defn- dist-to-rightmost-leaf [n]
  (if n (inc (dist-to-rightmost-leaf (.right n))) 0))

(defn- rank-pred-siblings [n]
  (or (nil? n)
      (>= (rank (.left n))
          (rank (.right n)))))

(defn elem-pred [cmpfn]
  (fn [n]
    (or (nil? n)
        (and
         (or (nil? (.left n)) (cmpfn (.elem n) (.. n left elem)))
         (or (nil? (.right n)) (cmpfn (.elem n) (.. n right elem)))))))

(defn- rank-pred-paren-children [n]
  (or (nil? n)
      (= (rank n) (inc (min (rank (.left n)) (rank (.right n)))))))

(defn- rank-pred-dist [n]
  (or (nil? n)
      (= (.rank n) (dist-to-rightmost-leaf n))))

(def num-of-heaps 10000)

(deftest min-heap-properties
  (testing "rank(l) >= rank(r)"
    (is (every? rank-pred-siblings (map #(.root %) (rnd-heaps (min-heap) num-of-heaps)))))

  (testing "rank (parent) = 1 + min(rank(left),rank(right))"
    (is (every? rank-pred-paren-children (map #(.root %) (rnd-heaps (min-heap) num-of-heaps)))))

  (testing "rank (node) is rightmost distance to leaf node"
    (is (every? rank-pred-dist (map #(.root %) (rnd-heaps (min-heap) num-of-heaps)))))

  (testing "elem(parent) <= elem(child)"    
    (is (every? (elem-pred <=) (map #(.root %) (rnd-heaps (min-heap) num-of-heaps))))))

(deftest max-heap-properties
  (testing "rank(l) >= rank(r)"
    (is (every? rank-pred-siblings (map #(.root %) (rnd-heaps (max-heap) num-of-heaps)))))

  (testing "rank (parent) = 1 + min(rank(left),rank(right))"
    (is (every? rank-pred-paren-children (map #(.root %) (rnd-heaps (max-heap) num-of-heaps)))))  

  (testing "rank (node) is rightmost distance to leaf node"
    (is (every? rank-pred-dist (map #(.root %) (rnd-heaps (max-heap) num-of-heaps)))))  
  
  (testing "elem(parent) >= elem(child)"    
    (is (every? (elem-pred >=) (map #(.root %) (rnd-heaps (max-heap) num-of-heaps))))))

(defn- perf-test-conj []
  (bench (peek (reduce conj (min-heap) (range 10000000 20000000 100)))))

(defn- perf-test-heap []
  (bench (peek (apply min-heap (range 10000000 20000000 100)))))
