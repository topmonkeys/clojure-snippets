(ns clojure-snippets.99-lisp-problems)

;;; 99 LISP Problems
;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninet
;;; y-Nine_Lisp_Problems.html

;; Working with lists

; P01 (*) Find the last box of a list.
; Example:
; * (my-last '(a b c d))
; (D)
(defn last-item 
  "Returns the last item from the list."
  [l]
  (when (seq l)
    (if-not (seq (rest l))
      l
      (recur (rest l)))))
    
; P02 (*) Find the last but one box of a list.
; Example:
; * (my-but-last '(a b c d))
; (C D)
(defn last-two-items
  "Returns the last two items from the list."
  [l]
  (when (and (seq l) (seq (rest l)))
    (if-not (seq (rest (rest l)))
      l
      (recur (rest l)))))

; P03 (*) Find the K'th element of a list.
; The first element in the list is number 1.
; Example:
; * (element-at '(a b c d e) 3)
; C
(defn item-at
  "Returns the nth item from the list."
  [n l]
  (loop [i 0 s l]
    (when (seq s)
      (if-not (< i (dec n))
        (first s)
        (recur (inc i) (rest s))))))
               
; P04 (*) Find the number of elements of a list.
(defn number-of-items
  "Returns the number of items in a list."
  [l]
  (loop [s l c 0]
    (if-not (seq s)
      c
      (recur (rest s) (inc c)))))

; P05 (*) Reverse a list.    
(defn reverse-list
  "Returns the reverse of a list."
  [l]
  (loop [s l result '()]
    (if-not (seq s)
      result
      (recur (rest s) (conj result (first s))))))

; P06 (*) Find out whether a list is a palindrome.
; A palindrome can be read forward or backward; e.g. (x a m a x).
(defn is-palindrome
  "Checks if a list is a palindrome."
  [l]
  (= l (reverse-list l)))

; P07 (**) Flatten a nested list structure.
; Transform a list, possibly holding lists as elements into a `flat' list by 
; replacing each list with its elements (re  cursively).
; Example:
; * (my-flatten '(a (b (c d) e)))
; (A B C D E)
; Hint: Use the predefined functions list and append.
(defn flatten-list
  "Flattens a nested list."
  ([l] (flatten-list l []))
  ([l result]
     (if-not (seq l)
       result
       (if-not (seq? (first l))
         (flatten-list (rest l) (conj result (first l)))
         (flatten-list (rest l) (flatten-list (first l) result))))))

; P08 (**) Eliminate consecutive duplicates of list elements.
; If a list contains repeated elements they should be replaced with a single 
; copy of the element. The order of the elements should not be changed.
; Example:
; * (compress '(a a a a b c c a a d e e e e))
; (A B C A D E)
(defn remove-consecutive-items
  "Removes consecutive items of an item from the beginning of the list"
  [i l]
  (loop [item i s l]
    (if-not (seq s)
      s
      (if-not (= item (first s))
        s
        (recur item (rest s))))))

(defn remove-consecutive-duplicates
  "Removes consecutive duplicates of a list without changing the order."
  ([l] (remove-consecutive-duplicates l []))
  ([l result]
     (if-not (seq l)
       result
       (let [s (remove-consecutive-items (first l) l)]
         (remove-consecutive-duplicates (seq s) (conj result (first l)))))))
    
; P09 (**) Pack consecutive duplicates of list elements into sublists.
; If a list contains repeated elements they should be placed in separate 
; sublists.
; Example:
; * (pack '(a a a a b c c a a d e e e e))
; ((A A A A) (B) (C C) (A A) (D) (E E E E))
(defn pack-consecutive-items
  "Packs one or more consecutive items at the beginning of the list."
  [i l]
  (loop [item i s l result []]
    (if-not (and (seq s) (= item (first s)))
      result
      (recur item (rest s) (conj result (first s))))))

(defn pack-consecutive-duplicates
  "Packs consecutive duplicates of a list."
  ([l] (pack-consecutive-duplicates l []))
  ([l result]
     (if-not (seq l)
       result
       (let [s (pack-consecutive-items (first l) l)
             t (remove-consecutive-items (first l) l)]
         (pack-consecutive-duplicates t (conj result s))))))

; P10 (*) Run-length encoding of a list.
; Use the result of problem P09 to implement the so-called run-length encoding 
; data compression method. Consecutive duplicates of elements are encoded as 
; lists (N E) where N is the number of duplicates of the element E.
; Example:
; * (encode '(a a a a b c c a a d e e e e))
; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(defn run-length-encode
  "Returns the run-length encoded list."
  [l]
  (if-not (seq l)
    l
    (let [s (pack-consecutive-duplicates l)]
      (loop [t s result []]
        (if-not (seq t)
          result
          (recur (rest t) 
                 (conj result (list (number-of-items (first t)) 
                                    (first (first t))))))))))

;  P11 (*) Modified run-length encoding.
; Modify the result of problem P10 in such a way that if an element has no 
; duplicates it is simply copied into the result list. Only elements with 
; duplicates are transferred as (N E) lists.
; Example:
; * (encode-modified '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E)) 
(defn run-length-encode-modified
  "Returns the run-length encoded list."
  [l]
  (if-not (seq l)
    l
    (let [s (pack-consecutive-duplicates l)]
      (loop [t s result []]
        (if-not (seq t)
          result
          (recur (rest t) 
                 (if (= (number-of-items (first t)) 1)
                   (conj result (first (first t)))
                   (conj result (list (number-of-items (first t)) 
                                      (first (first t)))))))))))

; P12 (**) Decode a run-length encoded list.
; Given a run-length code list generated as specified in problem P11. Construct 
; its uncompressed version.
(defn run-length-decode
  "Decodes the run length encoded list."
  [l]
  (if-not (seq l)
    l
    (loop [s l result []]
      (if-not (seq s)
        result
        (if-not (seq? (first s))
          (recur (rest s) (conj result (first s)))
          (recur (rest s) (into result 
                                (repeat (first (first s)) 
                                        (second (first s))))))))))

; P13 (**) Run-length encoding of a list (direct solution).
; Implement the so-called run-length encoding data compression method directly. 
; I.e. don't explicitly create the sublists containing the duplicates, as in 
; problem P09, but only count them. As in problem P11, simplify the result list 
; by replacing the singleton lists (1 X) by X.
; Example:
; * (encode-direct '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))
(defn number-of-duplicates
  "Returns the number of consecutive duplicates at the beginning of a list."
  [l i]
  (loop [s l count 0]
    (if-not (seq s)
      count
      (if-not (= i (first s))
        count
        (recur (rest s) (inc count))))))

(defn run-length-encode-simple
  "Returns the run-length encoded list."
  ([l] (run-length-encode-simple l []))
  ([l result]
     (if-not (seq l)
       result
       (let [n (number-of-duplicates l (first l))
             s (drop n l)]
         (run-length-encode-simple s 
                                   (if (= n 1)
                                     (conj result (first l))
                                     (conj result (list n (first l)))))))))

; P14 (*) Duplicate the elements of a list.
; Example:
; * (dupli '(a b c c d))
; (A A B B C C C C D D)
(defn create-duplicates
  "Creates a duplicate of every element."
  [l]
  (loop [s l result []]
    (if-not (seq s)
      result
      (recur (rest s) (conj result (first s) (first s))))))

; P15 (**) Replicate the elements of a list a given number of times.
; Example:
; * (repli '(a b c) 3)
; (A A A B B B C C C)
(defn create-repeated-elements
  "Creates repeated elements of every element in a list."
  [l n]
  (loop [s l result []]
    (if-not (seq s)
      result
      (recur (rest s) (into result (repeat n (first s)))))))

; P16 (**) Drop every N'th element from a list.
; Example:
; * (drop '(a b c d e f g h i k) 3)
; (A B D E G H K)
(defn drop-every-nth-element
  "Drops every nth element from the list."
  [l n]
  (loop [s l result [] index 0]
    (if-not (seq s)
      result
      (recur (rest s)
             (if-not (and (not= index 0) (= (mod index n) (dec n)))
               (conj result (first s))
               result)
             (inc index)))))
               
; P17 (*) Split a list into two parts; the length of the first part is given.
; Do not use any predefined predicates.
; Example:
; * (split '(a b c d e f g h i k) 3)
; ( (A B C) (D E F G H I K))
(defn split-into-two
  "Splits a list into two parts. The first part's length is provided."
  ([l n]
     (loop [s l index 0 first-part [] second-part []]
       (if-not (seq s)
         (list first-part second-part)
         (if (<= index (dec n))
           (recur (rest s) (inc index) (conj first-part (first s)) second-part)
           (recur (rest s) 
                  (inc index) 
                  first-part 
                  (conj second-part (first s))))))))

; P18 (**) Extract a slice from a list.
; Given two indices, I and K, the slice is the list containing the elements 
; between the I'th and K'th element of the original list (both limits included).
; Start counting the elements with 1.
; Example:
; * (slice '(a b c d e f g h i k) 3 7)
; (C D E F G)
(defn slice-a-list
  "Returns a slice of a list based on indices provided."
  [l i k]
  (loop [s l result [] index 0]
    (if-not (seq s)
      result
      (if-not (and (>= index (dec i)) (< index k))
        (recur (rest s) result (inc index))
        (recur (rest s) (conj result (first s)) (inc index)))))) 

; P19 (**) Rotate a list N places to the left.
; Examples:
; * (rotate '(a b c d e f g h) 3)
; (D E F G H A B C)
; * (rotate '(a b c d e f g h) -2)
; (G H A B C D E F)
(defn rotate-to-left
  "Rotates a list n places to the left."
  [l n]
  (let [a (if (< n 0) (- (number-of-items l) (Math/abs n)) n)]
    (loop [s l index 0 first-part [] second-part []]
      (if-not (seq s)
        (into [] (concat second-part first-part))
        (if (<= index (dec a))
          (recur (rest s) 
                 (inc index) 
                 (conj first-part (first s)) 
                 second-part)
          (recur (rest s) 
                 (inc index) 
                 first-part 
                 (conj second-part (first s))))))))

; P20 (*) Remove the K'th element from a list.
; Example:
; * (remove-at '(a b c d) 2)
; (A C D)
(defn remove-nth-element
  "Removes the nth element from a list."
  [l n]
  (loop [s l result [] index 0]
    (if-not (seq s)
      result
      (recur (rest s)
             (if-not (= index (dec n))
               (conj result (first s))
               result)
             (inc index)))))

; P21 (*) Insert an element at a given position into a list.
; Example:
; * (insert-at 'alfa '(a b c d) 2)
; (A ALFA B C D)
(defn insert-at-position
  "Inserts an element at a given position of the list."
  [n l item]
  (loop [s l result [] index 0]
    (if-not (seq s)
      result
      (recur (rest s)
             (if (= index (dec n))
               (conj result item (first s))
               (conj result (first s)))
             (inc index)))))

; P22 (*) Create a list containing all integers within a given range.
; If first argument is smaller than second, produce a list in decreasing order.
; Example:
; * (range 4 9)
; (4 5 6 7 8 9)
(defn range-of-numbers
  "Returns a range of numbers within the range limits provided."
  [m n]
  (loop [index m result []]
    (if (= index n)
      (conj result index)
      (recur (if (< m n) (inc index) (dec index))
             (conj result index)))))

; P23 (**) Extract a given number of randomly selected elements from a list.
; The selected items shall be returned in a list.
; Example:
; * (rnd-select '(a b c d e f g h) 3)
; (E D A)
(defn random-select
  "Returns a list of randomly selected elements from a list."
  [l n]
  (loop [s l result [] index 0]
    (if-not (< index n)
      result
      (let [count (number-of-items s)
            i (rand-int count)
            r (item-at (inc i) s)]
        (recur (remove-nth-element s (inc i)) 
               (conj result r)
               (inc index))))))

; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
; The selected numbers shall be returned in a list.
; Example:
; * (lotto-select 6 49)
; (23 1 17 33 21 37)
; Hint: Combine the solutions of problems P22 and P23.
(defn lotto-draw
  "Returns a list of random numbers from a range of numbers."
  [n m]
  (random-select (range-of-numbers 1 m) n))

; P25 (*) Generate a random permutation of the elements of a list.
; Example:
; * (rnd-permu '(a b c d e f))
; (B A D C E F)
; Hint: Use the solution of problem P23.
(defn random-permutation
  "Returns a random permutation of a list."
  [l]
  (random-select l (number-of-items l)))
