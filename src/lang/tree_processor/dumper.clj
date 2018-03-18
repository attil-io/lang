(ns lang.tree-processor.dumper
	(:require [lang.token_stream :refer :all])
	(:require [lang.input_stream :refer :all]))

(declare dump-var)
(declare dump-binary)
(declare dump-assign)

(defn dump-tree [ast] (let [ast_type (:type ast)]
        (case ast_type 
              nil nil
              "prog" "{}"
              "num" (str (:value ast))
              "str" (str (:value ast))
              "bool" (str (:value ast))
              "var" (dump-var (:value ast))
              "binary" (dump-binary ast)
              "assign" (dump-assign ast)
              )))

(defn- dump-var [value] value)
(defn- dump-binary [ast] (str (dump-tree (:left ast)) (:operator ast) (dump-tree (:right ast))))
(defn- dump-assign [ast] (dump-binary ast))

