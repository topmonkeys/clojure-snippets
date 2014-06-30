(ns clojure-snippets.core-test
  (:require [clojure.test :refer :all]
            [clojure-snippets.core :refer :all]))

(deftest addition
  (testing "Addition"
    (testing "with positive integers"
      (is (= 4 (+ 2 7)))
      (is (= 7 (+ 3 4))))
    (testing "with negative integers"
      (is (= -4 (+ -2 -2)))
      (is (= -1 (+ 3 -4))))))

(deftest subtraction
  (is (= 1 (- 4 3)))
  (is (= 3 (- 7 4))))

(deftest arithmetic
  (addition)
  (subtraction))

