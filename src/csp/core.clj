(ns csp.core
  (:require
   [clojure.core.async :as a :refer [<! >! chan go go-loop close!]]))

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

;; lineimage:( 1.. 125)character;
;; i:integer; i := 1;
;; * [c:character; X?c
;;    lineimage(i) := c;
;;    [  i <= 124 --> i := i + I
;;     ☐ i = 125 --> lineprinter!lineimage; i := 1
;;    ]
;;   ]; fill buffer
;;   [  i = 1 -> skip
;;    ☐ i > 1 -> *[i _< 125 ~ lineimage(i) := space; i := i + 1];
;;      lineprinter!lineimage ;;; pad buffer
;;   ]


;;; Intuitive implementation

(defn -assemble
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
      (when (not= i 125)
        (loop [i i
               arr arr]
          (if (= i 125)
            (do (>! printer arr) (close! cs))
            (recur (inc i) (conj arr \space))))))))

;;; More 1-1 implementation without extra constructs

(defn assemble
  [cs printer]
  (go
    (let [[i arr]
          (loop [i 1
                 arr []]
            (if-let [c (<! cs)]
              (let [arr (conj arr c)]
                (cond
                  (= i 125) (do (>! printer arr) (recur 0 []))
                  (<= i 124) (recur (inc i) arr)))
              [i arr]))]
      (cond
        (= i 1) true
        (> i 1)
        (loop [i i
               arr arr]
          (if (<= i 125)
            (recur (inc i) (conj arr \space))
            (do (>! printer arr) (close! cs))))))))


(defmacro alt
  [& bodies]
  `(go
     (let [ps# (a/merge ~(mapv (fn [body] `(go ~@body)) bodies))]
       (loop []
         (let [v# (<! ps#)]
           (if (nil? v#)
             v#
             (or v# (recur))))))))

;;; Fully equivalent implementation

(defn assemble
  [cs printer]
  (go
    (let [[i arr]
          (loop [i 1
                 arr []]
            (when-let [c (<! cs)]
              (let [[i arr]
                    (alt
                     [(when (<= i 124)
                        [(inc i) (conj arr c)])]
                     [(when (= i 125)
                        (>! printer arr)
                        [1 []])])]
                (recur i arr))))]
      (alt
       [(when (= i 1) true)]
       [(when (> i 1)
          (loop [i i
                 arr arr]
            (if (= i 125)
              (do (>! printer arr) (close! cs))
              (recur (inc i) (conj arr \space)))))]))))

;;; Problem: Read a sequence of cards of 80 characters each,
;; and print the characters on a linepfinter at 125 characters
;; per line. Every card should be followed by an extra
;; space, and the last line should be completed with spaces
;; if necessary.

(defn reformat
  [west east]
  (let [in (chan)
        out (chan)]
    [(disassemble west in) (copy in out) (assemble out east)]))


;;; Problem: Adapt the above program to replace every pair
;;; of consecutive asterisks by an upward arrow

(defn reformat2
  [west east]
  (let [in (chan)
        out (chan)]
    [(disassemble west in) (squash in out) (assemble out east)]))


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
  []
  (let [qr (chan)
        xy (chan)]
    (go
      (loop []
        (if-let [[x y] (<! xy)]
          (let [[q r]
                (loop [r x q 0]
                  (if (>= r y)
                    (recur (- r y) (inc q))
                    [q r]))]
            (>! qr [q r]))
          (close! qr))))
    {:in xy :out qr}))


;;; Problem: Compute a factorial by the recursive method, to a given
;;; limit

;; [fac( i: 1..limit)::
;;  * [n:integer;fac(i - 1)?n ->
;;     [n = 0 -> fac(i - 1)!1
;;      ☐ n > 0 -> fac(i + 1)!n- 1;
;;         r:integer;fac(i + 1)?r;fac(i - i)!(n * r)
;;      ]
;;    ]
;;      || fac(0)::USER
;; ]

;;; NOTE: Is i+1 a mistake?

(defn factorial
  [limit]
  (let [fac (vec (repeatedly (inc limit) chan))]
    (loop [i 1]
      (if (> i limit)
        (fac 0)
        (do
          (go-loop  []
            (let [n (<! (fac (dec i)))]
              (cond
                (or (zero? n) (== 1 n)) (>! (fac (dec i)) 1)
                (> n 1) (do (>! (fac i) (dec n))
                            (let [r (<! (fac i))]
                              (>! (fac (dec i)) (* n r))))))
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
;;;    ☐  n:integer; X?insert(n) -> SEARCH;
;;;    [i < size -> skip
;;;     ☐  i = size; size < 100 -> content (size) := n; size := size + l
;;;     ]
;;;    ]
;;; where SEARCH is an abbreviation for:
;;;    i:integer; i := 0;
;;;    * [i < size; content(i) != n 0 -> i := i + 1]

(defn search
  [arr size n]
  (loop [i 0]
    (cond
      (== i size) i
      (= (aget ^objects arr i) n) i
      :else (recur (inc i)))))

(defn linear-set
  [limit]
  (let [insert (a/chan)
        has (a/chan)
        at (a/chan)
        arr (object-array limit)]
    (go-loop [size 0]
      (a/alt!
        has ([n] (>! has (let [i (search arr size n)] (and (< i size) i))) (recur size))
        at ([i] (>! at (aget ^objects arr i)) (recur size))
        insert
        ([v insert]
         (let [i (search arr size v)]
           (cond
             (< i size) (recur size)
             (and (== i size) (< size limit))
             (do (aset ^objects arr i v) (recur (inc size))))))))
    {:insert insert :has has :at at}))

;;; Recursive data representation

;;; S(i: I.. 100)::
;;; * [n:integer; S (i - 1)?has(n) -> S(0)!false
;;;    ☐ n:integer; S (i - 1)?insert(n) ->
;;;    * [m:integer; S(i - 1)?has(m) ->
;;;       [  m <= n -> S(0)!(m = n)
;;;        ☐ m >  n -> S(i + 1)!has(m)
;;;       ]
;;;       ☐ m:integer; S(i - 1)?insert(m) ->
;;;         [ m < n -> S(i + 1)!insert(n); n := m
;;;          ☐ m = n -> skip
;;;          ☐ m > n -> S(i + 1)!insert(m)
;;;          ]
;;;     ]
;;;    ]

(defn rec-set
  [limit]
  (let [has (vec (repeatedly (+ 1 limit) chan))
        insert (vec (repeatedly (+ 1 limit) chan))
        s0 (chan)]
    (loop [i 0]
      (when (< i limit)
        (let [i+1 (inc i)
              -has (has i)
              +has (has i+1)
              -insert (insert i)
              +insert (insert i+1)]
          (go
            (let [n
                  (loop []
                    (assert (some? -has) (str "has" i "is nil!"))
                    (assert (some? -insert) (str "insert" i "is nil!"))
                    (a/alt!
                      -has ([v] (>! s0 false) (recur))
                      -insert ([v] v)))]
              (loop [n n]
                (assert (some? +has) (str "has" i+1 "is nil!"))
                (assert (some? +insert) (str "insert" i+1 "is nil!"))
                (a/alt!
                  -has
                  ([m]
                   (cond
                     (<= m n) (>! s0 (= m n))
                     (> m n)  (>! +has m))
                   (recur n))
                  -insert
                  ([m]
                   (cond
                     (< m n) (do (>! +insert n) (recur m))
                     (> m n) (do (>! +insert m) (recur n))
                     (= m n) (recur n)))))))
          (recur (inc i)))))
    {:has (has 1) :insert (insert 1) :s s0}))


;;; 5.1 Bounded Buffer
;;; Problem: Construct a buffering process X to smooth
;;; variations in the speed of output of portions by a pro-
;;; ducer process and input by a consumer process. The
;;; consumer contains pairs of commands X!more( );
;;; X?p, and the producer contains commands of the form
;;; X!p. The buffer should contain up to ten portions.
;;; Solution:
;;; X::
;;; buffer:(0..9) portion;
;;; in,out:integer; in := 0; out := 0;
;;; comment 0 <= out <= in <= out + 10;
;;; *[  in < out + 10; producer?buffer(in mod 10) -> in := in + 1
;;;   ☐ out < in; consumer?more( ) --> consumer!buffer(out mod 10);
;;;    out := out + 1
;;;    ]


(defn buffer
  [limit producer consumer]
  (let [buf (object-array limit)]
    (go-loop [in 0 out 0]
      (a/alt!
        producer
        ([v]
         (if (< in (+ out limit))
           (do
             (aset ^objects buf (mod in limit) v)
             (recur (inc in) out))
           (recur in out)))
        [[consumer (or (aget ^objects buf (mod out limit)) false)]]
        ([_]
         (if (< out in)
           (recur in (inc out))
           (recur in out)))))))

(comment
  (def producer (a/chan))
  (def consumer (a/chan))
  (def buf (buffer 3 producer consumer))
  (a/offer! producer 2)
  (a/poll! consumer)
  (a/close! producer)
  (a/close! consumer))

(defn pbuf
  [limit producer consumer]
  (let [chans (concat [producer] (repeatedly limit a/chan) [consumer])
        pairs (map vector chans (drop 1 chans))]
    (doseq [[f t] pairs]
      (a/pipe f t))))


(comment
  (def producer (a/chan))
  (def consumer (a/chan))
  (def buf (pbuf 3 producer consumer))
  (a/offer! producer 1)
  (a/offer! producer 2)
  (a/offer! producer 3)
  (a/offer! producer 4)
  (a/poll! consumer))
