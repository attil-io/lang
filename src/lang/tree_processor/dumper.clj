(ns lang.tree-processor.dumper
        (:require [lang.parser :refer :all])
        (:require [clojure.string :as s :only (join)]))

(declare dump-prog)
(declare dump-var)
(declare dump-binary)
(declare dump-assign)
(declare dump-lambda)
(declare dump-let)
(declare dump-if)
(declare dump-call)
(declare dump-tree)

(def ^{:private true} FALSE { :type "bool" :value false })

(defn dump [code] (dump-tree (parse_parse_toplevel code)))

(defn dump-tree [ast] (let [ast_type (:type ast)]
        (case ast_type 
              nil nil
              "prog" (dump-prog ast)
              "num" (str (:value ast))
              "str" (str (:value ast))
              "bool" (str (:value ast))
              "var" (dump-var (:value ast))
              "binary" (dump-binary ast)
              "assign" (dump-assign ast)
              "lambda" (dump-lambda ast)
              "let" (dump-let ast)
              "if" (dump-if ast)
              "call" (dump-call ast)
              )))

(defn- dump-var [value] (str value))
(defn- dump-binary [ast] (str (dump-tree (:left ast)) (:operator ast) (dump-tree (:right ast))))
(defn- dump-assign [ast] (dump-binary ast))
(defn- dump-lambda [ast] (str "function " (:name ast) "(" (s/join "," (map dump-var (:vars ast))) "){return " (dump-tree (:body ast)) "}"))
(defn- dump-let-var [v] (str (dump-var (:name v)) "=" (dump-tree (:def v))))
(defn- dump-let [ast] (str "let (" (s/join "," (map dump-let-var (:vars ast))) "){" (dump-tree (:body ast)) "}"))
(defn- dump-if [ast] (str "if (" (dump-tree (:cond ast)) "){" (dump-tree (:then ast)) "}{" (dump-tree (or (:else ast) FALSE) ) "}"))
(defn- dump-call [ast] (str (dump-tree (:func ast)) "(" (s/join "," (map dump-tree (:args ast))) ")"  ))
(defn- dump-prog [ast] (str "{" (dump-tree (:prog ast)) "}"))

