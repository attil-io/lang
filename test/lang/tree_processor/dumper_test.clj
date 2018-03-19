(ns lang.tree-processor.dumper-test
  (:require [clojure.test :refer :all]
            [lang.tree-processor.dumper :refer :all]))

 (deftest dump-tree-test
  (testing "test dump-tree"
    (is (nil? (dump-tree nil)))
    (is (= "{}" (dump-tree {:type "prog" :prog nil})))
    (is (= "{}" (dump-tree {:type "prog" :prog {}})))
    (is (= "42" (dump-tree {:type "num" :value 42})))
    (is (= "blah" (dump-tree {:type "str" :value "blah"})))
    (is (= "true" (dump-tree {:type "bool" :value true})))
    (is (= "false" (dump-tree {:type "bool" :value false})))
    (is (= "foo" (dump-tree {:type "var" :value "foo"})))
    (is (= "42+66" (dump-tree {:type "binary" :operator "+" :left {:type "num" :value 42} :right {:type "num" :value 66}})))
    (is (= "a=2" (dump-tree {:type "assign" :operator "=" :left {:type "var" :value "a"} :right {:type "num" :value 2}})))
    (is (= "function foo(x){return 42}" (dump-tree {:type "lambda" :name "foo" :vars ["x"] :body {:type "num" :value 42}})))
    (is (= "function foo(){return 42}" (dump-tree {:type "lambda" :name "foo" :vars [] :body {:type "num" :value 42}})))
    (is (= "function foo(x,y){return 42}" (dump-tree {:type "lambda" :name "foo" :vars ["x" "y"] :body {:type "num" :value 42}})))
    (is (= "let (x=42){x}" (dump-tree {:type "let" :vars [{:name "x" :def {:type "num" :value 42}}] :body {:type "var" :value "x"}})))
    (is (= "let (){42}" (dump-tree {:type "let" :vars [] :body {:type "num" :value 42}})))
    (is (= "let (x=42,y=666){x}" (dump-tree {:type "let" :vars [{:name "x" :def {:type "num" :value 42}} {:name "y" :def {:type "num" :value 666}}] :body {:type "var" :value "x"}})))
 
    ))

