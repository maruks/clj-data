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
    
    (is (= '(1 2 3) (seq a)))
    (is (= '(1 2 3 4) (seq b)))
    (is (nil? (seq e)))

    (is (b 4))
    (is (not (b 5))))


  (testing "no red node has red child node")


  (testing "every path from the root to empty node has the same number of black nodes")
  
  )
