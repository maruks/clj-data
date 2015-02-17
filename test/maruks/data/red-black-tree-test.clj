(ns maruks.data.red-black-tree-test
  (:require [clojure.test :refer :all]
            [maruks.data.red-black-tree :refer :all]
            [criterium.core :refer [bench]]))

(deftest red-black-tree-test

  (testing "empty"
    (is (empty? (empty-tree))))

  
  )
