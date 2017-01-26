(ns lang.core-test
  (:require [clojure.test :refer :all]
            [lang.core :refer :all]))

(deftest inputstream-test
  (testing "test inputstream_next"
    (is (= [nil {:pos 0 :input "" :line 0 :col 0}] (inputstream_next {:pos 0 :input "" :line 0 :col 0})))
    (is (= [\a {:pos 1 :input "a" :line 0 :col 1}] (inputstream_next {:pos 0 :input "a" :line 0 :col 0})))
    (is (= [\newline {:pos 1 :input "\n" :line 1 :col 0}] (inputstream_next {:pos 0 :input "\n" :line 0 :col 0}))))
  (testing "test inputstream_peek"
    (is (= nil (inputstream_peek {:pos 0 :input "" :line 0 :col 0})))
    (is (= \a (inputstream_peek {:pos 0 :input "a" :line 0 :col 0}))))
  (testing "test inputstream_eof"
    (is (= true (inputstream_eof {:pos 0 :input "" :line 0 :col 0})))
    (is (= false (inputstream_eof {:pos 0 :input "a" :line 0 :col 0})))
    (is (= true (inputstream_eof {:pos 2 :input "a" :line 0 :col 2}))))
  (testing "test inputstream_croak"
    (is (thrown-with-msg? Exception #"Unexpected character at position 5 \(3:4\)" (inputstream_croak "Unexpected character" {:pos 5 :input "dontcare" :line 3 :col 4} )))))


(deftest tokenstream-test
  (testing "test is_keyword"
    (is (= false (is_keyword nil)))
    (is (= true (is_keyword "if")))
    (is (= true (is_keyword "then")))
    (is (= true (is_keyword "else")))
    (is (= true (is_keyword "lambda")))
    (is (= true (is_keyword "λ")))
    (is (= true (is_keyword "true")))
    (is (= true (is_keyword "false"))))
   (testing "test is_digit"
    (is (= false (is_digit nil)))
    (is (= true (is_digit "1")))
    (is (= false (is_digit "w")))
    (is (= true (is_digit \1))))
   (testing "test is_id_start"
    (is (= false (is_id_start nil)))
    (is (= true (is_id_start "a")))
    (is (= false (is_id_start "1")))))
 

