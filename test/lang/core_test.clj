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
  (testing "test tokenstream_is_keyword"
    (is (= false (tokenstream_is_keyword nil)))
    (is (= false (tokenstream_is_keyword "while")))
    (is (= false (tokenstream_is_keyword "")))
    (is (= false (tokenstream_is_keyword " ")))
    (is (= true (tokenstream_is_keyword "if")))
    (is (= true (tokenstream_is_keyword "then")))
    (is (= true (tokenstream_is_keyword "else")))
    (is (= true (tokenstream_is_keyword "lambda")))
    (is (= true (tokenstream_is_keyword "λ")))
    (is (= true (tokenstream_is_keyword "true")))
    (is (= true (tokenstream_is_keyword "false"))))
   (testing "test tokenstream_is_digit"
    (is (= false (tokenstream_is_digit nil)))
    (is (= true (tokenstream_is_digit "1")))
    (is (= false (tokenstream_is_digit "w")))
    (is (= true (tokenstream_is_digit \1))))
   (testing "test tokenstream_is_id_start"
    (is (= false (tokenstream_is_id_start nil)))
    (is (= true (tokenstream_is_id_start "a")))
    (is (= false (tokenstream_is_id_start "1")))
    (is (= true (tokenstream_is_id_start "A")))
    (is (= true (tokenstream_is_id_start \a))))
   (testing "test tokenstream_is_id"
    (is (= false (tokenstream_is_id nil)))
    (is (= false (tokenstream_is_id " ")))
    (is (= true (tokenstream_is_id "_")))
    (is (= true (tokenstream_is_id "0")))
    (is (= true (tokenstream_is_id "1")))
    (is (= true (tokenstream_is_id "2")))
    (is (= true (tokenstream_is_id "3")))
    (is (= true (tokenstream_is_id "4")))
    (is (= true (tokenstream_is_id "5")))
    (is (= true (tokenstream_is_id "6")))
    (is (= true (tokenstream_is_id "7")))
    (is (= true (tokenstream_is_id "8")))
    (is (= true (tokenstream_is_id "9")))
    (is (= true (tokenstream_is_id "?")))
    (is (= true (tokenstream_is_id "!")))
    (is (= true (tokenstream_is_id "-")))
    (is (= true (tokenstream_is_id "<")))
    (is (= true (tokenstream_is_id ">")))
    (is (= true (tokenstream_is_id "=")))
    (is (= true (tokenstream_is_id \=))))
  (testing "test tokenstream_is_op_char"
    (is (= false (tokenstream_is_op_char nil)))
    (is (= false (tokenstream_is_op_char "_")))
    (is (= true (tokenstream_is_op_char "+")))
    (is (= true (tokenstream_is_op_char "-")))
    (is (= true (tokenstream_is_op_char "*")))
    (is (= true (tokenstream_is_op_char "/")))
    (is (= true (tokenstream_is_op_char "%")))
    (is (= true (tokenstream_is_op_char "=")))
    (is (= true (tokenstream_is_op_char "&")))
    (is (= true (tokenstream_is_op_char "|")))
    (is (= true (tokenstream_is_op_char "<")))
    (is (= true (tokenstream_is_op_char ">")))
    (is (= true (tokenstream_is_op_char "!")))
    (is (= true (tokenstream_is_op_char \!))))
   (testing "test tokenstream_is_punc"
    (is (= false (tokenstream_is_punc nil)))
    (is (= false (tokenstream_is_punc "x")))
    (is (= true (tokenstream_is_punc ",")))
    (is (= true (tokenstream_is_punc ";")))
    (is (= true (tokenstream_is_punc "(")))
    (is (= true (tokenstream_is_punc ")")))
    (is (= true (tokenstream_is_punc "{")))
    (is (= true (tokenstream_is_punc "}")))
    (is (= true (tokenstream_is_punc "[")))
    (is (= true (tokenstream_is_punc "]")))
    (is (= true (tokenstream_is_punc \]))))
   (testing "test tokenstream_is_whitespace"
    (is (= false (tokenstream_is_whitespace nil)))
    (is (= false (tokenstream_is_whitespace "x")))
    (is (= true (tokenstream_is_whitespace " ")))
    (is (= true (tokenstream_is_whitespace "\t")))
    (is (= true (tokenstream_is_whitespace "\n")))
    (is (= true (tokenstream_is_whitespace \newline))))
   (testing "test tokenstream_read_while"
    (is (= ["abcd" {:pos 4 :input "abcd" :line 0 :col 4}] (tokenstream_read_while (fn [_] true) {:pos 0 :input "abcd" :line 0 :col 0})))
    (is (= ["" {:pos 0 :input "abcd" :line 0 :col 0}] (tokenstream_read_while (fn [_] false) {:pos 0 :input "abcd" :line 0 :col 0}))))
   (testing "test tokenstream_read_number"
    (is (= [{:value 2 :type "num"} {:pos 1 :input "2" :line 0 :col 1}] (tokenstream_read_number {:pos 0 :input "2" :line 0 :col 0})))
    (is (= [{:value 3 :type "num"} {:pos 1 :input "3" :line 0 :col 1}] (tokenstream_read_number {:pos 0 :input "3" :line 0 :col 0})))
    (is (= [{:value 3.1 :type "num"} {:pos 3 :input "3.1" :line 0 :col 3}] (tokenstream_read_number {:pos 0 :input "3.1" :line 0 :col 0}))))
   (testing "test tokenstream_read_number"
    (is (= [{:value "myvar" :type "var"} {:pos 5 :input "myvar" :line 0 :col 5}] (tokenstream_read_ident {:pos 0 :input "myvar" :line 0 :col 0})))
    (is (= [{:value "then" :type "kw"} {:pos 4 :input "then" :line 0 :col 4}] (tokenstream_read_ident {:pos 0 :input "then" :line 0 :col 0})))
    (is (= [{} {:pos 0 :input " " :line 0 :col 0}] (tokenstream_read_ident {:pos 0 :input " " :line 0 :col 0}))))
   (testing "test tokenstream_read_escaped"
    (is (= ["before" {:pos 7 :input "before'after" :line 0 :col 7}] (tokenstream_read_escaped {:pos 0 :input "before'after" :line 0 :col 0} \')))
    (is (= ["" {:pos 1 :input "'after" :line 0 :col 1}] (tokenstream_read_escaped {:pos 0 :input "'after" :line 0 :col 0} \')))
    (is (= ["something" {:pos 9 :input "something" :line 0 :col 9}] (tokenstream_read_escaped {:pos 0 :input "something" :line 0 :col 0} \')))
    (is (= ["somet'hing" {:pos 11 :input "somet\\'hing" :line 0 :col 11}] (tokenstream_read_escaped {:pos 0 :input "somet\\'hing" :line 0 :col 0} \'))))
   (testing "test tokenstream_read_string"
    (is (= [{:type "str" :value "before"} {:pos 7 :input "before\"after" :line 0 :col 7}] (tokenstream_read_string {:pos 0 :input "before\"after" :line 0 :col 0})))
    (is (= [{:type "str" :value ""} {:pos 1 :input "\"after" :line 0 :col 1}] (tokenstream_read_string {:pos 0 :input "\"after" :line 0 :col 0})))
    (is (= [{:type "str" :value "something"} {:pos 9 :input "something" :line 0 :col 9}] (tokenstream_read_string {:pos 0 :input "something" :line 0 :col 0}))))
   (testing "test tokenstream_skip_comment"
    (is (= [\newline {:pos 6 :input "line1\nline2" :line 1 :col 0}] (tokenstream_skip_comment {:pos 0 :input "line1\nline2" :line 0 :col 0})))
    (is (= [nil {:pos 5 :input "line1" :line 0 :col 5}] (tokenstream_skip_comment {:pos 0 :input "line1" :line 0 :col 0}))))
  (testing "test tokenstream_read_next"
    (is (= nil (tokenstream_read_next {:pos 0 :input "" :line 0 :col 0})))
    (is (= nil (tokenstream_read_next {:pos 0 :input "# abc\n" :line 0 :col 0})))
    (is (= [{:type "str" :value "something"} {:pos 11 :input "\"something\"" :line 0 :col 11}] (tokenstream_read_next {:pos 0 :input "\"something\"" :line 0 :col 0})))
    (is (= [{:type "num" :value 23} {:pos 2 :input "23" :line 0 :col 2}] (tokenstream_read_next {:pos 0 :input "23" :line 0 :col 0})))
    (is (= [{:type "var" :value "alma"} {:pos 4 :input "alma" :line 0 :col 4}] (tokenstream_read_next {:pos 0 :input "alma" :line 0 :col 0})))
    (is (= [{:type "kw" :value "if"} {:pos 2 :input "if" :line 0 :col 2}] (tokenstream_read_next {:pos 0 :input "if" :line 0 :col 0})))
    (is (= [{:type "punc" :value \,} {:pos 1 :input "," :line 0 :col 1}] (tokenstream_read_next {:pos 0 :input "," :line 0 :col 0})))
    (is (= [{:type "op" :value "+"} {:pos 1 :input "+" :line 0 :col 1}] (tokenstream_read_next {:pos 0 :input "+" :line 0 :col 0})))
    (is (thrown-with-msg? Exception #"Can't handle character: \^ at position 0 \(0:0\)" (tokenstream_read_next {:pos 0 :input "^" :line 0 :col 0}))))
  (testing "test tokenstream_peek"
    (is (= nil (tokenstream_peek [{:pos 0 :input "" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_peek [{:pos 0 :input "(+ 1 2)" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_peek [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}]))))
  (testing "test tokenstream_peek"
    (is (= nil (tokenstream_next [{:pos 0 :input "" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_next [{:pos 0 :input "(+ 1 2)" :line 0 :col 0}])))
    (is (= [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}] (tokenstream_next [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}]))))
   (testing "test tokenstream_eof"
    (is (= true (tokenstream_eof [{:pos 0 :input "" :line 0 :col 0}])))
    (is (= false (tokenstream_eof [{:pos 0 :input "(+ 1 2)" :line 0 :col 0}])))
    (is (= false (tokenstream_eof [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}])))))
 
 (deftest parse-test
  (testing "test parse_is_punc"
    (is (= nil (parse_is_punc nil nil)))
    (is (= {:type "punc" :value \(} (parse_is_punc \( [{:type "punc" :value \(} {:pos 1 :input "(+ 1 2)" :line 0 :col 1}])))
    (is (= false (parse_is_punc \( [{:type "op" :value "+="} {:pos 3 :input "a+=5" :line 0 :col 3}])))
    (is (= false (parse_is_punc \( [{:type "punc" :value \.} {:pos 1 :input "." :line 0 :col 1}])))
    (is (= {:type "punc" :value \.} (parse_is_punc nil [{:type "punc" :value \.} {:pos 1 :input "." :line 0 :col 1}]))))
  (testing "test parse_is_kw"
    (is (= nil (parse_is_kw nil nil)))
    (is (= {:type "kw" :value "if"} (parse_is_kw "if" [{:type "kw" :value "if"} {:pos 2 :input "if" :line 0 :col 2}])))
    (is (= false (parse_is_kw "if" [{:type "op" :value "+="} {:pos 3 :input "a+=5" :line 0 :col 3}])))
    (is (= false (parse_is_kw "if" [{:type "kw" :value "then"} {:pos 4 :input "then" :line 0 :col 4}])))
    (is (= {:type "kw" :value "if"} (parse_is_kw nil [{:type "kw" :value "if"} {:pos 2 :input "if" :line 0 :col 2}]))))
  (testing "test parse_is_op"
    (is (= nil (parse_is_op nil nil)))
    (is (= {:type "op" :value "+="} (parse_is_op "+=" [{:type "op" :value "+="} {:pos 2 :input "+=" :line 0 :col 2}])))
    (is (= false (parse_is_op "+=" [{:type "kw" :value "if"} {:pos 2 :input "if" :line 0 :col 2}])))
    (is (= false (parse_is_op "+=" [{:type "op" :value "*"} {:pos 1 :input "*" :line 0 :col 1}])))
    (is (= {:type "op" :value "+="} (parse_is_op nil [{:type "op" :value "+="} {:pos 2 :input "+=" :line 0 :col 2}]))))
  (testing "test parse_skip_punc"
    (is (= nil (parse_skip_punc nil nil)))
    (is (= [{:type "punc" :value \,} {:pos 1 :input "," :line 0 :col 1}] (parse_skip_punc \, [{:pos 0 :input "," :line 0 :col 0}])))
    (is (thrown-with-msg? Exception #"Expecting punctuation: \"q\"" (parse_skip_punc \q, [{:pos 1 :input "q + 1" :line 0 :col 1}]))))
  (testing "test parse_skip_kw"
    (is (= nil (parse_skip_kw nil nil)))
    (is (= [{:type "kw" :value "else"} {:pos 4 :input "else" :line 0 :col 4}] (parse_skip_kw "else" [{:pos 0 :input "else" :line 0 :col 0}])))
    (is (thrown-with-msg? Exception #"Expecting keyword: \"hello\"" (parse_skip_kw "hello", [{:pos 0 :input "hello world" :line 0 :col 0}]))))
  (testing "test parse_skip_op"
    (is (= nil (parse_skip_op nil nil)))
    (is (= [{:type "num" :value 5} {:pos 3 :input "+ 5" :line 0 :col 3}] (parse_skip_op "+" [{:pos 1 :input "+ 5" :line 0 :col 1}])))
    (is (thrown-with-msg? Exception #"Expecting operator: \"hello\"" (parse_skip_op "hello", [{:pos 5 :input "hello world" :line 0 :col 5}])))))

