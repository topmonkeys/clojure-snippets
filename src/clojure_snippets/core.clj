(ns clojure-snippets.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(ns clojure-snippets.core.core
  (:require [clojure.java.javadoc]
            [clojure.string :as s]))

(defn numbers-down-from
  "Returns a vector of numbers from the number provided until zero."
  [n]
  (loop [v [], n n]
    (if (pos? n)
      (recur (conj v n) (dec n))
      v)))

(defn sum-down-from
  "Returns the sum of numbers from the number provided until zero."
  [n]
  (loop [sum 0, n n]
    (if (pos? n)
      (recur (+ sum n) (dec n))
      sum)))

(defn sqrt
  "Returns the square root of a number."
  [n]
  (java.lang.Math/sqrt n))

(defn print-a-sequence
  "Prints a sequence."
  [s]
  (when (seq s)
    (println (first s))
    (recur (rest s))))

(defn find-java-methods
  "Returns a list of methods that match the pattern."
  [class-name method-name-regex]
  (for [method (seq (.getMethods class-name))
        :let [method-name (.getName method)]
        :when (re-find method-name-regex method-name)]
    method-name))
    

(defn bit-modified-pixels
  "Returns a set of co-ordinates with their co-ordinates bit modified with the function passed."
  [max-x max-y bit-fn]
  (for [x (range max-x) y (range max-y)] [x y (bit-fn x y)]))
