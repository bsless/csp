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
