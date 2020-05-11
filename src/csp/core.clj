(ns csp.core
  (:require
   [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! poll!]]))

;;; Coroutines

;;; Problem: Write a process X to copy characters output by
;;; process west to process east.

(defn dirty-copy
  [east west]
  (go
    (loop []
      (>! west (<! east))
      (recur))))

(defn copy
  [east west]
  (go-loop []
    (let [v (<! east)]
      (if (and v (>! west v))
        (recur)
        (close! west)))))

;;; Problem: Adapt the previous program to replace every
;; pair of consecutive asterisks "**" by an upward arrow
;; "^". Assume that the final character input is not an
;; asterisk.

(defn squash
  [east west]
  (go-loop []
    (let [v (<! east)]
      (if v
        (do
          (if (= v "*")
            (let [v (<! east)]
              (if (= v "*")
                (>! west "^")
                (do
                  (>! west "*")
                  (>! west v))))
            (>! west v))
          (recur))
        (close! west)))))


;;; Problem: to read cards from a cardfile and output to
;; process X the stream of characters they contain. An extra
;; space should be inserted at the end of each card.

(defn disassemble
  [cardfile out]
  (go
    (loop []
      (let [arr (<! cardfile)]
        (when arr
          (doseq [c arr]
            (>! out c))
          (recur))))
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
