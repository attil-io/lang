(ns lang.evaluate-test
  (:require [clojure.test :refer :all]
            [lang.evaluate :refer :all]))

(defn myprint [& args] ["bla" {:vars {:print myprint} :parent nil}])
 
 (deftest evaluate-test
  (testing "test evaluate"
    (is (= [5 {:vars {} :parent nil}] (evaluate {:type "num" :value 5} {:vars {} :parent nil})))
    (is (= ["hello" {:vars {} :parent nil}] (evaluate {:type "str" :value "hello"} {:vars {} :parent nil})))
    (is (= [true {:vars {} :parent nil}] (evaluate {:type "bool" :value true} {:vars {} :parent nil})))
    (is (= [42 {:vars {:a 42} :parent nil}] (evaluate {:type "var" :value "a"} {:vars {:a 42} :parent nil})))
    (is (= [42 {:vars {:a 42} :parent nil}] (evaluate {:type "assign" :left {:type "var" :value "a"} :right {:type "num" :value 42}} {:vars {:a 0} :parent nil})))
    (is (thrown-with-msg? Exception #"Cannot assign to" (evaluate {:type "assign" :left {:type "num" :value 6} :right {:type "num" :value 42}} {:vars {:a 0} :parent nil})))
    (is (= [42 {:vars {} :parent nil}] (evaluate {:type "if" :cond {:type "bool" :value true} :then {:type "num" :value 42}} {:vars {} :parent nil})))
    (is (= [44 {:vars {} :parent nil}] (evaluate {:type "if" :cond {:type "bool" :value false} :then {:type "num" :value 42} :else {:type "num" :value 44}} {:vars {} :parent nil})))
    (is (= [false {:vars {} :parent nil}] (evaluate {:type "if" :cond {:type "bool" :value false} :then {:type "num" :value 42}} {:vars {} :parent nil})))
    (is (= [47 {:vars {} :parent nil}] (evaluate {:type "binary" :operator "+" :left {:type "num" :value 5} :right {:type "num" :value 42}} {:vars {} :parent nil})))
    (is (= [47 {:vars {} :parent nil}] (((evaluate {:type "lambda" :name "bla" :vars ["a"] :body {:type "var" :value "a"}} {:vars {} :parent nil}) 0) {:vars {} :parent nil} 47)))
    (is (= [false {:vars {} :parent nil}] (evaluate {:type "prog" :prog [{:type "bool" :value false}]} {:vars {} :parent nil})))
    (is (= [47 {:vars {} :parent nil}] (evaluate {:type "prog" :prog [{:type "num" :value 47}]} {:vars {} :parent nil})))
    (is (= [48 {:vars {} :parent nil}] (evaluate {:type "prog" :prog [{:type "num" :value 47}{:type "num" :value 48}]} {:vars {} :parent nil})))
    (is (= [10 {:vars {:a 5 :b 10} :parent nil}] (evaluate {:type "prog" :prog [{:type "assign" :operator "=" :left {:type "var" :value "a"} :right {:type "num" :value 5}}{:type "assign" :operator "=" :left {:type "var" :value "b"} :right {:type "num" :value 10}}]} {:vars {:a 0 :b 0} :parent nil})))
    (is (= [10 {:vars {} :parent nil}] (evaluate {:type "call" :func {:type "lambda" :name "a" :vars ["c"] :body {:type "var" :value "c"}} :args [{:type "num" :value 10}]} {:vars {} :parent nil})))
    (is (= ["bla" {:vars {:print myprint} :parent nil}] (evaluate {:type "call", :func {:value "print", :type "var"}, :args [{:type "str", :value "hello, world"}]} {:vars {:print myprint} :parent nil})))
    (is (thrown-with-msg? Exception #"I don't know how to evaluate" (evaluate {:type "unknown"} {:vars {} :parent nil}))))
  (testing "test evaluate_apply_op"
    (let [evaluate_apply_op #'lang.evaluate/evaluate_apply_op]
    (is (= 5 (evaluate_apply_op "+" 2 3)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "+" "alma" 3)))
    (is (= 6 (evaluate_apply_op "*" 2 3)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "*" "alma" 3)))
    (is (= -1 (evaluate_apply_op "-" 2 3)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "-" "alma" 3)))
    (is (= 2/3 (evaluate_apply_op "/" 2 3)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "/" "alma" 3)))
    (is (thrown-with-msg? Exception #"Divide by zero" (evaluate_apply_op "/" 2 0)))
    (is (= 2 (evaluate_apply_op "%" 2 3)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "%" "alma" 3)))
    (is (thrown-with-msg? Exception #"Divide by zero" (evaluate_apply_op "%" 2 0)))
    (is (= false (evaluate_apply_op "&&" true false)))
    (is (= true (evaluate_apply_op "||" false true )))
    (is (= true (evaluate_apply_op "<" 1 2)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "<" "alma" 3)))
    (is (= false (evaluate_apply_op ">" 1 2)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op ">" "alma" 3)))
    (is (= true (evaluate_apply_op "<=" 1 2)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op "<=" "alma" 3)))
    (is (= false (evaluate_apply_op ">=" 1 2)))
    (is (thrown-with-msg? Exception #"Expected number but got" (evaluate_apply_op ">=" "alma" 3)))
    (is (= false (evaluate_apply_op "==" 1 2)))
    (is (= true (evaluate_apply_op "!=" 1 2)))
    (is (thrown-with-msg? Exception #"Can't apply operator" (evaluate_apply_op "<>" 1 2)))))
  (testing "test evaluate_make_lambda"
    (let [evaluate_make_lambda #'lang.evaluate/evaluate_make_lambda]
    (is (= [5 {:vars {} :parent nil}] ((evaluate_make_lambda {:type "lambda" :name "bla" :vars [] :body {:type "num" :value 5}}) {:vars {} :parent nil})))
    (is (= [false {:vars {} :parent nil}] ((evaluate_make_lambda  {:type "lambda" :name "bla" :vars ["a"] :body {:type "var" :value "a"}}) {:vars {} :parent nil})))
    (is (= [42 {:vars {} :parent nil}] ((evaluate_make_lambda  {:type "lambda" :name "bla" :vars ["a"] :body {:type "var" :value "a"}}) {:vars {} :parent nil} 42))))))

