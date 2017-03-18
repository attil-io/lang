(ns lang.interpret-test
  (:require [clojure.test :refer :all]
            [lang.interpret :refer :all]
            [lang.util :refer :all]))

 
(deftest interpret-test
  (testing "test interpret"
    (is (= "hello, world" (first (interpret "print(\"hello, world\")"))))
    (is (= "5" (first (interpret "sum = lambda(x, y) x + y; print(sum(2, 3));"))))
    (is (= "11" (first (interpret "plus = λ(a, b) a + b; cons = λ(a, b) λ(f) f(a, b); consfun = cons(5, 6); print(consfun(plus));"))))
    (is (= ["1" ", " "2" ", " "3" ", " "4" ", " "5" ", " "6" ", " "7" ", " "8" ", " "9" ", " "10" "\n"] (second (interpret (long-str 
               "print_range = lambda(a, b) if a <= b {            "
               "                        print(a);                 "
               "                        if a + 1 <= b {           "
               "                          print(\", \");          "
               "                          print_range(a + 1, b);  "
               "                        } else println(\"\");     "
               "                      };                          "
               "print_range(1, 10);                               "
)))))))

