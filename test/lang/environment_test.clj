(ns lang.environment-test
  (:require [clojure.test :refer :all]
            [lang.environment :refer :all]))

 (deftest environment-test
  (testing "test environment_create"
    (is (= {:vars {} :parent nil} (environment_create nil)))
    (is (= {:vars {} :parent {:vars nil :parent nil}} (environment_create {:vars nil :parent nil})))
    (is (= {:vars {} :parent {:vars {:var1 "val1"} :parent nil}} (environment_create {:vars {:var1 "val1"} :parent nil}))))
  (testing "test environment_lookup"
    (is (= nil (environment_lookup "hello" {:vars {} :parent nil})))
    (is (= {:vars {:hello 42} :parent nil} (environment_lookup "hello" {:vars {:hello 42} :parent nil})))
    (is (= {:vars {:hello 42} :parent nil} (environment_lookup "hello" {:vars {} :parent {:vars {:hello 42} :parent nil}}))))
  (testing "test environment_get"
    (is (thrown-with-msg? Exception #"Undefined variable" (environment_get "hello" {:vars {} :parent nil})))
    (is (= 42 (environment_get "hello" {:vars {:hello 42} :parent nil})))
    (is (= 42 (environment_get "hello" {:vars {} :parent {:vars {:hello 42} :parent nil}}))))
  (testing "test environment_get_global"
    (is (= 45 (environment_get_global "hello" {:vars {} :parent {:vars {:hello 45} :parent nil}})))
    (is (= 42 (environment_get_global "hello" {:vars {:hello 5} :parent {:vars {:hello 42} :parent nil}}))))
  (testing "test environment_set"
    (is (= {:vars {:hello 42} :parent nil} (environment_set "hello" 42 {:vars {:hello 5} :parent nil})))
    (is (= {:vars {} :parent {:vars {:hello 42} :parent nil}} (environment_set "hello" 42 {:vars {} :parent {:vars {:hello 5} :parent nil}})))
    (is (thrown-with-msg? Exception #"Undefined variable" (environment_set "hello" 42 {:vars {} :parent nil})))
    (is (= {:vars {:hello 42} :parent {:vars {} :parent nil}} (environment_set "hello" 42 {:vars {} :parent {:vars {} :parent nil}}))))
   (testing "test environment_set_global"
    (is (= {:vars {:hello 42} :parent nil} (environment_set_global "hello" 42 {:vars {:hello 5} :parent nil})))
    (is (= {:vars {} :parent {:vars {:hello 42} :parent nil}} (environment_set_global "hello" 42 {:vars {} :parent {:vars {:hello 5} :parent nil}}))))
  (testing "test environment_def"
    (is (= {:vars {:hello 42} :parent nil} (environment_def "hello" 42 {:vars {} :parent nil})))
    (is (= {:vars {:hello 5} :parent nil} (environment_def "hello" 5 {:vars {:hello 42} :parent nil})))))

