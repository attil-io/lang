(ns lang.tree-processor.dumper
	(:require [lang.token_stream :refer :all])
	(:require [lang.input_stream :refer :all]))

(declare dump-var)

(defn dump-tree [ast] (let [ast_type (:type ast)]
        (case ast_type 
              nil nil
              "prog" "{}"
              "num" (str (:value ast))
              "str" (str (:value ast))
              "bool" (str (:value ast))
              "var" (dump-var (:value ast))
              )))

(defn- dump-var [value] value)

