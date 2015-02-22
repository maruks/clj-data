(ns maruks.data.red-black-tree-test
  (:require [clojure.test :refer :all]
            [maruks.data.red-black-tree :refer :all]
            [criterium.core :refer [bench]]))


(deftest red-black-tree-test

  (def e (empty-tree))
  (def a (reduce conj e '(1 2 3)))
  (def b (reduce conj a '(3 2 4)))
  
  (testing "set functions"
    (is (empty? e))
    (is (not (empty? a)))
    (is (not (empty? b)))

    (is (zero? (count e)))
    (is (= 3 (count a)))
    (is (= 4 (count b)))

    (is (not (contains? e 1)))
    (is (contains? a 2))
    (is (not (contains? a 4)))    
    (is (contains? b 4))    

    (is (= 2 (get a 2)))
    (is (= 3 (get b 3)))
    (is (nil? (get a 4)))
    
    (is (b 4))
    (is (not (b 5))))

  (testing "seq returns sorted sequence of elements"
    (is (= '(1 2 3) (seq a)))
    (is (= '(1 2 3 4) (seq b)))
    (is (nil? (seq e)))
    (is (= '(1 2 3 4 5 6 7 8 9) (seq (reduce conj (empty-tree) '(5 2 7 9 8 4 3 1 6))))))

  (testing "insert ignores duplicate elements"
    (is (= '(5) (seq (reduce conj (empty-tree) '(5 5 5)))))))

(defn- every-node? [pred node]
  (when (pred node)
    (and (pred (.left node))
         (pred (.right node)))))

(defn- bst-pred [n]
  (or (nil? n)
      (and (or (nil? (.left n)) (> (.elem n) (.. n left elem)))
           (or (nil? (.right n)) (< (.elem n) (.. n right elem))))))

(defn- red-child-pred [n]
  (or (nil? n)
      (= :black (.color n))     
      (and (or (nil? (.left n)) (= :black (.. n left color)))
           (or (nil? (.right n)) (= :black (.. n right color))))))

(defn- count-black-nodes [n]
  (if n
    (let [t (mapcat count-black-nodes (list (.left n) (.right n)))]
      (if (= :black (.color n))
        (map inc t)
        t))
    '(0)))

(defn- black-nodes-pred [n]
  (let [t (count-black-nodes n)
        b (first t)]
    (every? #(= b %) t)))

(def set-size 100)

(defn- rnd-set [n]
  (reduce conj (empty-tree) (repeatedly n #(rand-int 10000))))

(defn- rnd-sets [n]
  (repeatedly n #(rnd-set set-size)))

(deftest red-black-tree-properties

  (testing "left < parent < right"
    (is (every? #(every-node? bst-pred (.root %)) (rnd-sets 100))))
  
  (testing "no red node has red child node"
    (is (every? #(every-node? red-child-pred (.root %)) (rnd-sets 100))))

  (testing "every path from the root to empty node has the same number of black nodes"
    (is (every? #(every-node? black-nodes-pred (.root %)) (rnd-sets 100)))))
