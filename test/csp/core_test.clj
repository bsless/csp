(ns csp.core-test
  (:require
   [clojure.test :as t]
   [csp.core :as sut]
   [clojure.core.async :as a :refer [<! >! chan go go-loop close! put! poll!]]))

(t/deftest squash0
  (t/is (= "abc*def^g^*i"
           (clojure.string/join
            (a/<!!
             (let [s "abc*def**g***i"
                   to (chan)]
               (sut/squash0 (a/to-chan s) to)
               (a/into [] (a/take (count s) to))))))))

(t/deftest squash
  (t/is (= "abc*def^g^*i"
           (clojure.string/join
            (a/<!!
             (let [s "abc*def**g***i"
                   to (chan)]
               (sut/squash (a/to-chan s) to)
               (a/into [] (a/take (count s) to))))))))

(t/deftest div
  (t/is (= [3 1]
           (a/<!!
            (let [p (sut/div)]
              (go
                (>! (p :in) [10 3])
                (<! (p :out))))))))

(t/deftest search
  (t/is (= 1
           (sut/search (object-array [1 2 3]) 3 2)))
  (t/is (= 3
           (sut/search (object-array [1 2 3]) 3 4))))

(t/deftest linear-set
  (let [s (sut/linear-set 100)]
    (a/<!! (go (>! (s :insert) 10)
               (>! (s :insert) 20)
               (>! (s :insert) 30)))
    (t/testing "s has a value which was inserted"
      (t/is
       (some?
        (a/<!!
         (go
           (a/>! (s :has) 20)
           (a/<! (s :has)))))))
    (t/testing "s does not have a value which was not inserted"
      (t/is
       (false?
        (a/<!!
         (go
           (a/>! (s :has) 40)
           (a/<! (s :has)))))))))

(t/deftest rec-set
  (let [s (sut/rec-set 10)]
    (a/<!! (go (>! (s :insert) 1)
               (>! (s :insert) 2)
               (>! (s :insert) 3)))
    (t/is (true? (a/<!! (go (>! (s :has) 2)
                            (<! (s :s))))))
    (t/is (true? (a/<!! (go (>! (s :has) 3)
                            (<! (s :s))))))
    (t/is (false? (a/<!! (go (>! (s :has) 4)
                             (<! (s :s))))))))
