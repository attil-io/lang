(ns lang.token_stream-test
  (:require [clojure.test :refer :all]
            [lang.token_stream :refer :all]))



(deftest tokenstream-test
  (testing "test tokenstream_is_keyword"
    (let [tokenstream_is_keyword #'lang.token_stream/tokenstream_is_keyword]
    (is (= false (tokenstream_is_keyword nil)))
    (is (= false (tokenstream_is_keyword "while")))
    (is (= false (tokenstream_is_keyword "")))
    (is (= false (tokenstream_is_keyword " ")))
    (is (= true (tokenstream_is_keyword "let")))
    (is (= true (tokenstream_is_keyword "if")))
    (is (= true (tokenstream_is_keyword "then")))
    (is (= true (tokenstream_is_keyword "else")))
    (is (= true (tokenstream_is_keyword "lambda")))
    (is (= true (tokenstream_is_keyword "λ")))
    (is (= true (tokenstream_is_keyword "true")))
    (is (= true (tokenstream_is_keyword "false")))))
  (testing "test tokenstream_is_digit"
    (let [tokenstream_is_digit #'lang.token_stream/tokenstream_is_digit]
    (is (= false (tokenstream_is_digit nil)))
    (is (= true (tokenstream_is_digit "1")))
    (is (= false (tokenstream_is_digit "w")))
    (is (= true (tokenstream_is_digit \1)))))
  (testing "test tokenstream_is_id_start"
    (let [tokenstream_is_id_start #'lang.token_stream/tokenstream_is_id_start]
    (is (= false (tokenstream_is_id_start nil)))
    (is (= true (tokenstream_is_id_start "a")))
    (is (= false (tokenstream_is_id_start "1")))
    (is (= true (tokenstream_is_id_start "A")))
    (is (= true (tokenstream_is_id_start \a)))))
  (testing "test tokenstream_is_id"
    (let [tokenstream_is_id #'lang.token_stream/tokenstream_is_id]
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
    (is (= true (tokenstream_is_id \=)))))
  (testing "test tokenstream_is_op_char"
    (let [tokenstream_is_op_char #'lang.token_stream/tokenstream_is_op_char]
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
    (is (= true (tokenstream_is_op_char \!)))))
  (testing "test tokenstream_is_punc"
    (let [tokenstream_is_punc #'lang.token_stream/tokenstream_is_punc]
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
    (is (= true (tokenstream_is_punc \])))))
  (testing "test tokenstream_is_whitespace"
    (let [tokenstream_is_whitespace #'lang.token_stream/tokenstream_is_whitespace]
    (is (= false (tokenstream_is_whitespace nil)))
    (is (= false (tokenstream_is_whitespace "x")))
    (is (= true (tokenstream_is_whitespace " ")))
    (is (= true (tokenstream_is_whitespace "\t")))
    (is (= true (tokenstream_is_whitespace "\n")))
    (is (= true (tokenstream_is_whitespace \newline)))))
  (testing "test tokenstream_read_while"
    (let [tokenstream_read_while #'lang.token_stream/tokenstream_read_while]
    (is (= ["abcd" {:pos 4 :input "abcd" :line 0 :col 4}] (tokenstream_read_while (fn [_] true) {:pos 0 :input "abcd" :line 0 :col 0})))
    (is (= ["" {:pos 0 :input "abcd" :line 0 :col 0}] (tokenstream_read_while (fn [_] false) {:pos 0 :input "abcd" :line 0 :col 0})))))
  (testing "test tokenstream_read_number"
    (let [tokenstream_read_number #'lang.token_stream/tokenstream_read_number]
    (is (= [{:value 2 :type "num"} {:pos 1 :input "2" :line 0 :col 1}] (tokenstream_read_number {:pos 0 :input "2" :line 0 :col 0})))
    (is (= [{:value 3 :type "num"} {:pos 1 :input "3" :line 0 :col 1}] (tokenstream_read_number {:pos 0 :input "3" :line 0 :col 0})))
    (is (= [{:value 3.1 :type "num"} {:pos 3 :input "3.1" :line 0 :col 3}] (tokenstream_read_number {:pos 0 :input "3.1" :line 0 :col 0})))))
  (testing "test tokenstream_read_ident"
    (let [tokenstream_read_ident #'lang.token_stream/tokenstream_read_ident]
    (is (= [{:value "myvar" :type "var"} {:pos 5 :input "myvar" :line 0 :col 5}] (tokenstream_read_ident {:pos 0 :input "myvar" :line 0 :col 0})))
    (is (= [{:value "then" :type "kw"} {:pos 4 :input "then" :line 0 :col 4}] (tokenstream_read_ident {:pos 0 :input "then" :line 0 :col 0})))
    (is (= [{} {:pos 0 :input " " :line 0 :col 0}] (tokenstream_read_ident {:pos 0 :input " " :line 0 :col 0})))))
  (testing "test tokenstream_read_escaped"
    (let [tokenstream_read_escaped #'lang.token_stream/tokenstream_read_escaped]
    (is (= ["before" {:pos 7 :input "before'after" :line 0 :col 7}] (tokenstream_read_escaped {:pos 0 :input "before'after" :line 0 :col 0} \')))
    (is (= ["" {:pos 1 :input "'after" :line 0 :col 1}] (tokenstream_read_escaped {:pos 0 :input "'after" :line 0 :col 0} \')))
    (is (= ["something" {:pos 9 :input "something" :line 0 :col 9}] (tokenstream_read_escaped {:pos 0 :input "something" :line 0 :col 0} \')))
    (is (= ["somet'hing" {:pos 11 :input "somet\\'hing" :line 0 :col 11}] (tokenstream_read_escaped {:pos 0 :input "somet\\'hing" :line 0 :col 0} \')))))
  (testing "test tokenstream_read_string"
    (let [tokenstream_read_string #'lang.token_stream/tokenstream_read_string]
    (is (= [{:type "str" :value "before"} {:pos 7 :input "before\"after" :line 0 :col 7}] (tokenstream_read_string {:pos 0 :input "before\"after" :line 0 :col 0})))
    (is (= [{:type "str" :value ""} {:pos 1 :input "\"after" :line 0 :col 1}] (tokenstream_read_string {:pos 0 :input "\"after" :line 0 :col 0})))
    (is (= [{:type "str" :value "something"} {:pos 9 :input "something" :line 0 :col 9}] (tokenstream_read_string {:pos 0 :input "something" :line 0 :col 0})))))
  (testing "test tokenstream_skip_comment"
    (let [tokenstream_skip_comment #'lang.token_stream/tokenstream_skip_comment]
    (is (= [\newline {:pos 6 :input "line1\nline2" :line 1 :col 0}] (tokenstream_skip_comment {:pos 0 :input "line1\nline2" :line 0 :col 0})))
    (is (= [nil {:pos 5 :input "line1" :line 0 :col 5}] (tokenstream_skip_comment {:pos 0 :input "line1" :line 0 :col 0})))))
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
    (is (= nil (tokenstream_peek {:pos 0 :input "" :line 0 :col 0})))
    (is (= {:type "punc" :value \(} (tokenstream_peek {:pos 0 :input "(+ 1 2)" :line 0 :col 0})))
    (is (= {:type "op" :value "+"} (tokenstream_peek  {:pos 1 :input "(+ 1 2)" :line 0 :col 1})))
    (is (= {:type "num" :value 1} (tokenstream_peek  {:pos 2 :input "(+ 1 2)" :line 0 :col 2}))))
  (testing "test tokenstream_next"
    (is (= nil (tokenstream_next [{:pos 0 :input "" :line 0 :col 0}])))
    (is (= {:pos 1 :input "(+ 1 2)" :line 0 :col 1} (tokenstream_next {:pos 0 :input "(+ 1 2)" :line 0 :col 0})))
    (is (= {:pos 2 :input "(+ 1 2)" :line 0 :col 2} (tokenstream_next {:pos 1 :input "(+ 1 2)" :line 0 :col 1}))))
  (testing "test tokenstream_eof"
    (is (= true (tokenstream_eof {:pos 0 :input "" :line 0 :col 0})))
    (is (= false (tokenstream_eof {:pos 0 :input "(+ 1 2)" :line 0 :col 0})))
    (is (= false (tokenstream_eof {:pos 1 :input "(+ 1 2)" :line 0 :col 1})))))

