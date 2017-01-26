(ns lang.core-test
  (:require [clojure.test :refer :all]
            [lang.core :refer :all]))

(deftest inputstream-test
  (testing "test inputstream_next"
    (is (= nil (inputstream_next {:pos 0 :input "" :line 0 :col 0})))))
