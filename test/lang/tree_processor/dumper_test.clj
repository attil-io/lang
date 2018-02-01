(ns lang.tree-processor.dumper-test
  (:require [clojure.test :refer :all]
            [lang.tree-processor.dumper :refer :all]))

 (deftest dump-tree-test
  (testing "test dump-tree"
    (is (nil? (dump-tree nil)))))

