(ns maruks.data.red-black-tree-test
  (:require [clojure.test :refer :all]
            [maruks.data.red-black-tree :refer :all]
            [criterium.core :refer [bench]]))

(deftest red-black-tree-test
  
  (let [e (empty-tree)
        a (reduce conj e '(1 2 3))
        b (reduce conj a '(3 2 4))]
    
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
      (is (= '(5) (seq (reduce conj (empty-tree) '(5 5 5))))))))

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

(defn- red-children-pred [n]
  (or (nil? n)
      (= :black (.color n))     
      (or (and (nil? (.left n)) (nil? (.right n)))
          (and (not (nil? (.left n))) (not (nil? (.right n)))))))

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

(defn- red-black-color-pred [n]
  (or (nil? n)
      (= :black (.color n))
      (= :red (.color n))))

(def set-size 200)

(defn- rnd-set [n]
  (reduce conj (empty-tree) (repeatedly n #(rand-int 10000))))

(defn- rnd-sets [n]
  (repeatedly n #(rnd-set (+ set-size (rand-int 10)))))

(deftest red-black-tree-properties

  (testing "root is black"
    (is (every? #(= :black (.. % root color)) (rnd-sets 100))))  
  
  (testing "left child < parent < right child"
    (is (every? #(every-node? bst-pred (.root %)) (rnd-sets 100))))
  
  (testing "no red node has red child node"
    (is (every? #(every-node? red-child-pred (.root %)) (rnd-sets 100))))

  (testing "node is either black or red"
      (is (every? #(every-node? red-black-color-pred (.root %)) (rnd-sets 100))))
    
  (testing "red node has either zero or two children"
    (is (every? #(every-node? red-children-pred (.root %)) (rnd-sets 100))))  
  
  (testing "every path from the root to empty node has the same number of black nodes"
    (is (every? #(every-node? black-nodes-pred (.root %)) (rnd-sets 100)))))

(deftest custom-cmp-tree
  (let [t (red-black-tree #(compare (count %1) (count %2)) '("qw" "dfgh" "a" "abc"))]
    (testing "seq returns sorted sequence of elements"
      (is (= '("a" "qw" "abc" "dfgh") (seq t))))))

(defn- is-red-black-tree? [n]
  (and
   (bst-pred n)
   (red-child-pred n)
   (red-children-pred n)
   (black-nodes-pred n)
   (red-black-color-pred n)))

(defn is-leaf? [n]
  (and n (nil? (.left n)) (nil? (.right n))))

(deftest element-removal
  (testing "removing RED LEAF node"
    (let [t (red-black-tree '(2 1 4 3))
          d (disj t 1)
          n (find-node (.root t) 1 compare)]
      (is (and (red? n) (is-leaf? n)))
      (is (= '(2 3 4) (seq d)))
      (is (= '(1 2 3 4) (seq t)))
      (is (is-red-black-tree? (.root d)))
      (is (is-red-black-tree? (.root t)))))
  (testing "removing BLACK node with RED child"
    (let [t (red-black-tree '(2 1 4 3))
          d (disj t 2)
          n (find-node (.root t) 2 compare)
          c (.left n)]
      (is (and (red? c) (is-leaf? c)))
      (is (and (black? n) (not (is-leaf? n))))
      (is (= '(1 3 4) (seq d)))
      (is (= '(1 2 3 4) (seq t)))
      (is (is-red-black-tree? (.root d)))
      (is (is-red-black-tree? (.root t))))))
