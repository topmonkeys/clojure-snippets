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
(defn nth-item
  "Returns the nth item from the list."
  [n l]
  (loop [i 0 s l]
    (when (seq s)
      (if (= i n)
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

  
