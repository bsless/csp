(ns csp.core
  (:require
   [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! poll!]]))

;;; Coroutines

;;; Problem: Write a process X to copy characters output by
;;; process west to process east.

;; X :: *[c:character; west?c -> east!c]

(defn copy
  [west east]
  (go-loop []
    (let [v (<! west)]
      (if (and v (>! east v))
        (recur)
        (close! east)))))

;;; Problem: Adapt the previous program to replace every
;; pair of consecutive asterisks "**" by an upward arrow
;; "^". Assume that the final character input is not an
;; asterisk.

;; X :: *[c:character; west?c -->
;;        [  c != asterisk --> east!c
;;         ☐ c  = asterisk --> west?c ;
;;           [  c != asterisk --> east!asterisk; east!c
;;            ☐ c  = asterisk --> east!upward arrow ]
;;        ]
;;       ]


(defn squash0
  [west east]
  (go-loop []
    (if (let [c (<! west)]
          (and
           c
           (if (not= c \*)
             (>! east c)
             (let [c (<! west)]
               (and
                c
                (if (not= c \*)
                  (do (>! east \*) (>! east c))
                  (>! east \^)))))))
      (recur)
      (close! east))))

(defn squash
  [west east]
  (go-loop []
    (if (when-let [c (<! west)]
          (if (not= c \*)
            (>! east c)
            (when-let [c (<! west)]
              (if (not= c \*)
                (do (>! east \*) (>! east c))
                (>! east \^)))))
      (recur)
      (close! east))))


;;; Problem: to read cards from a cardfile and output to
;; process X the stream of characters they contain. An extra
;; space should be inserted at the end of each card.

;; *[cardimage:(l..80)character; cardfile?cardimage
;;   i:integer; i := 1;
;;   *[i <= 80 -> X!cardimage(i); i -> i + 1]
;;     X!space
;;     ]


(defn disassemble
  [cardfile out]
  (go
    (loop []
      (when-let [arr (<! cardfile)]
        (loop [i 0]
          (when
              (and
               (<= i 80)
               (>! out (nth arr i)))
            (recur (inc i))))
        (recur)))
    (>! out \space)
    (close! out)))


;;; Problem: To read a stream of characters from process X
;;; and print them in lines of 125 characters on a lineprinter.
;;; The last line should be completed with spaces if necessary.

(defn assemble
  [cs printer]
  (go
    (let [[i arr]
          (loop [i 0
                 arr []]
            (if (= i 125)
              (do (>! printer arr) (recur 0 []))
              (let [c (<! cs)]
                (if c
                  (recur (inc i) (conj arr c))
                  [i arr]))))]
      (if (not= i 125)
        (loop [i i
               arr arr]
          (if (= i 125)
            (do (>! printer arr) (close! cs))
            (recur (inc i) (conj arr \space))))))))

;;; Problem: Read a sequence of cards of 80 characters each,
;; and print the characters on a linepfinter at 125 characters
;; per line. Every card should be followed by an extra
;; space, and the last line should be completed with spaces
;; if necessary.

(defn reformat
  [west east]
  (let [in (chan)
        out (chan)]
    [(disassemble west in) (copy in out) (assemble out west)]))


;;; Problem: Adapt the above program to replace every pair
;;; of consecutive asterisks by an upward arrow

(defn reformat2
  [west east]
  (let [in (chan)
        out (chan)]
    [(disassemble west in) (squash in out) (assemble out west)]))


;;; Subroutines and data representation

;; 4.1 Function: Division With Remainder Problem: Construct a process to
;; represent a functiontype subroutine, which accepts a positive dividend
;; and divisor, and returns their integer quotient and remainder.
;; Efficiency is of no concern.

;; Solution:
;; [DIV :: *[ x,y:integer; X?( x,y) -->
;;           quot,rem:integer; quot = 0; rem ~ x;
;;           * [rem >= y -> rem := rem - y; quot -> quot + 1];
;;           X!(quot,rem)
;;           ]
;; ||X::USER
;; ]


(defn div
  [xy]
  (let [qr (chan)]
    (go
      (loop []
        (let [[-x -y] (<! xy)]
          (if (and x y)
            (loop [-r -x -q 0]
              (if (>= -r -y)
                (recur (- -r -y) (inc -q))
                (>! qr [-q -r])))
            (close! qr)))))
    qr))


(comment
  (a/<!!
   (let [xy (chan)
         qr (div xy)]
     (go
       (>! xy [10 3])
       (<! qr)))))

;;; Problem: Compute a factorial by the recursive method, to a given
;;; limit

;; [fac( i: 1..limit)::
;;  * [n:integer;fac(i - 1)?n ->
;;     [n = 0 -> fac(i - 1)!1
;;      || n > 0 -> fac(i + 1)!n- 1;
;;         r:integer;fac(i + 1)?r;fac(i - i)!(n * r)
;;      ]
;;    ]
;;      || fac(0)::USER
;; ]

(defn factorial
  [limit]
  (let [chans (vec (repeatedly (inc limit) chan))
        fac (fn [n] (chans n))]
    (loop [i 1]
      (if (> i limit)
        (chans 0)
        (do
          (go-loop  []
            (let [n (<! (fac (dec i)))]
              (if (or (zero? n) (== 1 n))
                (>! (fac (dec i)) 1)
                (do
                  (>! (fac i) (dec n))
                  (>! (fac (dec i)) (* n (<! (fac i)))))))
            (recur))
          (recur (inc i)))))))

(comment
  (def -fac (factorial 10))
  (a/<!! (go (>! -fac 6) (<! -fac))))

;;; 4.3 Data Representation: Small Set of Integers
;;; Problem: To represent a set of not more than 100 integers as a
;;; process, S, which accepts two kinds of instruction from its calling
;;; process X: (1) S!insert(n), insert the integer n in the set, and (2)
;;; S!has(n); ... ; S?b, b is set true if n is in the set, and false
;;; otherwise. The initial value of the set is empty.

;;; S::
;;; content:(0..99)integer; size:integer; size .--- 0;
;;; * [n:integer;X?has(n) -> SEARCH;X!(i < size)
;;;    || n:integer; X?insert(n) -> SEARCH;
;;;    [i < size -> skip
;;;     || i = size; size < 100 -> content (size) := n; size := size + l
;;;     ]
;;;    ]
;;; where SEARCH is an abbreviation for:
;;;    i:integer; i := 0;
;;;    * [i < size; content(i) != n 0 -> i := i + 1]

(defn search
  [arr size n]
  (let [k (alength arr)]
    (loop [i 0]
      (when-not (== i k)
        (if (or (= (aget ^objects arr i) n) (= i size))
          i
          (recur (inc i)))))))

(defn linear-set
  [limit]
  (let [insert (a/chan)
        has (a/chan)
        at (a/chan)
        arr (object-array limit)]
    (go
      (loop [size 0]
        (a/alt!
          has ([n] (search arr size n) (recur size))
          at ([i] (aget ^objects arr i) (recur size))
          insert
          ([v insert]
           (let [i (search arr size v)]
             (if i
               (do
                 (aset ^objects arr i v)
                 (recur (inc size)))
               (recur size)))))))))
