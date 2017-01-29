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
    (is (= true (inputstream_eof nil)))
    (is (= false (inputstream_eof {:pos 0 :input "a" :line 0 :col 0})))
    (is (= true (inputstream_eof {:pos 2 :input "a" :line 0 :col 2}))))
  (testing "test inputstream_croak"
    (is (thrown-with-msg? Exception #"Unexpected character at position 5 \(3:4\)" (inputstream_croak "Unexpected character" {:pos 5 :input "dontcare" :line 3 :col 4} )))))


(deftest tokenstream-test
  (testing "test is_keyword"
    (is (= false (is_keyword nil)))
    (is (= false (is_keyword "while")))
    (is (= false (is_keyword "")))
    (is (= false (is_keyword " ")))
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
    (is (= false (is_id_start "1")))
    (is (= true (is_id_start "A")))
    (is (= true (is_id_start \a))))
   (testing "test is_id"
    (is (= false (is_id nil)))
    (is (= false (is_id " ")))
    (is (= true (is_id "_")))
    (is (= true (is_id "0")))
    (is (= true (is_id "1")))
    (is (= true (is_id "2")))
    (is (= true (is_id "3")))
    (is (= true (is_id "4")))
    (is (= true (is_id "5")))
    (is (= true (is_id "6")))
    (is (= true (is_id "7")))
    (is (= true (is_id "8")))
    (is (= true (is_id "9")))
    (is (= true (is_id "?")))
    (is (= true (is_id "!")))
    (is (= true (is_id "-")))
    (is (= true (is_id "<")))
    (is (= true (is_id ">")))
    (is (= true (is_id "=")))
    (is (= true (is_id \=))))
  (testing "test is_op_char"
    (is (= false (is_op_char nil)))
    (is (= false (is_op_char "_")))
    (is (= true (is_op_char "+")))
    (is (= true (is_op_char "-")))
    (is (= true (is_op_char "*")))
    (is (= true (is_op_char "/")))
    (is (= true (is_op_char "%")))
    (is (= true (is_op_char "=")))
    (is (= true (is_op_char "&")))
    (is (= true (is_op_char "|")))
    (is (= true (is_op_char "<")))
    (is (= true (is_op_char ">")))
    (is (= true (is_op_char "!")))
    (is (= true (is_op_char \!))))
   (testing "test is_punc"
    (is (= false (is_punc nil)))
    (is (= false (is_punc "x")))
    (is (= true (is_punc ",")))
    (is (= true (is_punc ";")))
    (is (= true (is_punc "(")))
    (is (= true (is_punc ")")))
    (is (= true (is_punc "{")))
    (is (= true (is_punc "}")))
    (is (= true (is_punc "[")))
    (is (= true (is_punc "]")))
    (is (= true (is_punc \]))))
   (testing "test is_whitespace"
    (is (= false (is_whitespace nil)))
    (is (= false (is_whitespace "x")))
    (is (= true (is_whitespace " ")))
    (is (= true (is_whitespace "\t")))
    (is (= true (is_whitespace "\n")))
    (is (= true (is_whitespace \newline))))
   (testing "test read_while"
    (is (= ["abcd" {:pos 4 :input "abcd" :line 0 :col 4}] (read_while (fn [_] true) {:pos 0 :input "abcd" :line 0 :col 0})))
    (is (= ["" {:pos 0 :input "abcd" :line 0 :col 0}] (read_while (fn [_] false) {:pos 0 :input "abcd" :line 0 :col 0}))))
   (testing "test read_number"
    (is (= [{:value 2 :type "num"} {:pos 1 :input "2" :line 0 :col 1}] (read_number {:pos 0 :input "2" :line 0 :col 0})))
    (is (= [{:value 3 :type "num"} {:pos 1 :input "3" :line 0 :col 1}] (read_number {:pos 0 :input "3" :line 0 :col 0})))
    (is (= [{:value 3.1 :type "num"} {:pos 3 :input "3.1" :line 0 :col 3}] (read_number {:pos 0 :input "3.1" :line 0 :col 0}))))
   (testing "test read_number"
    (is (= [{:value "myvar" :type "var"} {:pos 5 :input "myvar" :line 0 :col 5}] (read_ident {:pos 0 :input "myvar" :line 0 :col 0})))
    (is (= [{:value "then" :type "kw"} {:pos 4 :input "then" :line 0 :col 4}] (read_ident {:pos 0 :input "then" :line 0 :col 0})))
    (is (= [{} {:pos 0 :input " " :line 0 :col 0}] (read_ident {:pos 0 :input " " :line 0 :col 0}))))
   (testing "test read_escaped"
    (is (= ["before" {:pos 7 :input "before'after" :line 0 :col 7}] (read_escaped {:pos 0 :input "before'after" :line 0 :col 0} \')))
    (is (= ["" {:pos 1 :input "'after" :line 0 :col 1}] (read_escaped {:pos 0 :input "'after" :line 0 :col 0} \')))
    (is (= ["something" {:pos 9 :input "something" :line 0 :col 9}] (read_escaped {:pos 0 :input "something" :line 0 :col 0} \')))
    (is (= ["somet'hing" {:pos 11 :input "somet\\'hing" :line 0 :col 11}] (read_escaped {:pos 0 :input "somet\\'hing" :line 0 :col 0} \'))))
   (testing "test read_string"
    (is (= [{:type "str" :value "before"} {:pos 7 :input "before\"after" :line 0 :col 7}] (read_string {:pos 0 :input "before\"after" :line 0 :col 0})))
    (is (= [{:type "str" :value ""} {:pos 1 :input "\"after" :line 0 :col 1}] (read_string {:pos 0 :input "\"after" :line 0 :col 0})))
    (is (= [{:type "str" :value "something"} {:pos 9 :input "something" :line 0 :col 9}] (read_string {:pos 0 :input "something" :line 0 :col 0}))))
   (testing "test skip_comment"
    (is (= [\newline {:pos 6 :input "line1\nline2" :line 1 :col 0}] (skip_comment {:pos 0 :input "line1\nline2" :line 0 :col 0})))
    (is (= [nil {:pos 5 :input "line1" :line 0 :col 5}] (skip_comment {:pos 0 :input "line1" :line 0 :col 0}))))
  (testing "test read_next"
    (is (= nil (read_next {:pos 0 :input "" :line 0 :col 0})))
    (is (= nil (read_next {:pos 0 :input "# abc\n" :line 0 :col 0})))
    (is (= [{:type "str" :value "something"} {:pos 11 :input "\"something\"" :line 0 :col 11}] (read_next {:pos 0 :input "\"something\"" :line 0 :col 0})))
    (is (= [{:type "num" :value 23} {:pos 2 :input "23" :line 0 :col 2}] (read_next {:pos 0 :input "23" :line 0 :col 0})))
    (is (= [{:type "var" :value "alma"} {:pos 4 :input "alma" :line 0 :col 4}] (read_next {:pos 0 :input "alma" :line 0 :col 0})))
    (is (= [{:type "kw" :value "if"} {:pos 2 :input "if" :line 0 :col 2}] (read_next {:pos 0 :input "if" :line 0 :col 0})))
    (is (= [{:type "punc" :value \,} {:pos 1 :input "," :line 0 :col 1}] (read_next {:pos 0 :input "," :line 0 :col 0})))
    (is (= [{:type "op" :value "+"} {:pos 1 :input "+" :line 0 :col 1}] (read_next {:pos 0 :input "+" :line 0 :col 0})))
    (is (thrown-with-msg? Exception #"Can't handle character: \^ at position 0 \(0:0\)" (read_next {:pos 0 :input "^" :line 0 :col 0}))))
  (testing "test tokenstream_peek"
    (is (= nil (tokenstream_peek [{:pos 0 :input "" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_peek [{:pos 0 :input "(+ 1 2)" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_peek [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}]))))
  (testing "test tokenstream_peek"
    (is (= nil (tokenstream_next [{:pos 0 :input "" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_next [{:pos 0 :input "(+ 1 2)" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_next [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}])))))
 
 
