(ns lang.tree-processor.dumper-test
  (:require [clojure.test :refer :all]
            [lang.tree-processor.dumper :refer :all]))

 (deftest walk-tree-test
  (testing "test walk-tree"
    (is (nil? (walk-tree nil)))
    (is (= "{}" (walk-tree {:type "prog" :prog nil})))
    (is (= "{}" (walk-tree {:type "prog" :prog {}})))
    (is (= "{x=false}" (walk-tree {:type "prog" :prog [{:type "assign" :operator "=" :left {:type "var" :value "x"} :right {:type "bool" :value false}}]})))
    (is (= "42" (walk-tree {:type "num" :value 42})))
    (is (= "blah" (walk-tree {:type "str" :value "blah"})))
    (is (= "true" (walk-tree {:type "bool" :value true})))
    (is (= "false" (walk-tree {:type "bool" :value false})))
    (is (= "foo" (walk-tree {:type "var" :value "foo"})))
    (is (= "42+66" (walk-tree {:type "binary" :operator "+" :left {:type "num" :value 42} :right {:type "num" :value 66}})))
    (is (= "a=2" (walk-tree {:type "assign" :operator "=" :left {:type "var" :value "a"} :right {:type "num" :value 2}})))
    (is (= "function foo(x){return 42}" (walk-tree {:type "lambda" :name "foo" :vars ["x"] :body {:type "num" :value 42}})))
    (is (= "function foo(){return 42}" (walk-tree {:type "lambda" :name "foo" :vars [] :body {:type "num" :value 42}})))
    (is (= "function foo(x,y){return 42}" (walk-tree {:type "lambda" :name "foo" :vars ["x" "y"] :body {:type "num" :value 42}})))
    (is (= "let (x=42){x}" (walk-tree {:type "let" :vars [{:name "x" :def {:type "num" :value 42}}] :body {:type "var" :value "x"}})))
    (is (= "let (){42}" (walk-tree {:type "let" :vars [] :body {:type "num" :value 42}})))
    (is (= "let (x=42,y=666){x}" (walk-tree {:type "let" :vars [{:name "x" :def {:type "num" :value 42}} {:name "y" :def {:type "num" :value 666}}] :body {:type "var" :value "x"}})))
    (is (= "if (x==42){x}{true}" (walk-tree {:type "if" :cond {:type "binary" :operator "==" :left {:type "var" :value "x"} :right {:type "num" :value 42}} :then {:type "var" :value "x"} :else {:type "bool" :value true}})))
    (is (= "if (x==42){x}{false}" (walk-tree {:type "if" :cond {:type "binary" :operator "==" :left {:type "var" :value "x"} :right {:type "num" :value 42}} :then {:type "var" :value "x"}})))
    (is (= "foo(10)" (walk-tree {:type "call" :func {:type "var" :value "foo"} :args [{:type "num" :value 10}]})))))

 (deftest to-clj-test
  (testing "test to-clj"
    (is (= nil (to-clj nil)))
    (is (= "(foo 10)" (to-clj {:type "call" :func {:type "var" :value "foo"} :args [{:type "num" :value 10}]})))))
 
