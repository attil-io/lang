(ns lang.tree-processor.dumper
        (:require [lang.parser :refer :all])
        (:require [clojure.string :as s :only (join)]))

(def ^{:private true} FALSE { :type "bool" :value false })
(declare walk-tree)

(defprotocol WALKER-CALLBACK
    (on-prog [this ast])
    (on-num [this ast])
    (on-str [this ast])
    (on-bool [this ast])
    (on-var [this ast])
    (on-binary [this ast])
    (on-assign [this ast])
    (on-lambda [this ast])
    (on-let [this ast])
    (on-if [this ast])
    (on-call [this ast]))


(defn- internal-let-var [this v] (str (:name v) "=" (walk-tree (:def v) this)))

(deftype dump-callback [] WALKER-CALLBACK
    (on-prog [this ast] (str "{" (s/join \newline (map #(walk-tree % this) (:prog ast))) "}"))
    (on-num [this ast] (str (:value ast)))
    (on-str [this ast] (str (:value ast)))
    (on-bool [this ast] (str (:value ast)))
    (on-var [this ast] (str (:value ast)))
    (on-binary [this ast] (str (walk-tree (:left ast) this) (:operator ast) (walk-tree (:right ast) this)))
    (on-assign [this ast] (on-binary this ast))
    (on-lambda [this ast] (str "function " (:name ast) "(" (s/join "," (map str (:vars ast))) "){return " (walk-tree (:body ast) this) "}"))
    (on-let [this ast] (str "let (" (s/join "," (map #(internal-let-var this %) (:vars ast))) "){" (walk-tree (:body ast) this) "}"))
    (on-if [this ast] (str "if (" (walk-tree (:cond ast) this) "){" (walk-tree (:then ast) this) "}{" (walk-tree (or (:else ast) FALSE) this) "}"))
    (on-call [this ast] (str (walk-tree (:func ast) this) "(" (s/join "," (map #(walk-tree % this) (:args ast))) ")")))

(defn- literal? [ast]
  (contains? #{"num" "str" "bool"} (:type ast)))

(deftype clojure-callback [] WALKER-CALLBACK
    (on-prog [this ast] (s/join \newline (map #(walk-tree % this) (:prog ast))))
    (on-num [this ast] (str (:value ast)))
    (on-str [this ast] (str \" (:value ast) \"))
    (on-bool [this ast] (str (:value ast)))
    (on-var [this ast] (str (:value ast)))
    (on-binary [this ast] (str \( (:operator ast) " " (walk-tree (:left ast) this) " " (walk-tree (:right ast) this) \) ))
    (on-assign [this ast] (on-binary this ast))
    (on-lambda [this ast] (str "(defn " (:name ast) " [" (s/join " " (map str (:vars ast))) "] " (when-not (literal? (:body ast)) "(") (walk-tree (:body ast) this) (when-not (literal? (:body ast)) ")") ")"))
    (on-let [this ast] (str "(let [" (s/join " " (map #(internal-let-var this %) (:vars ast))) "] (" (walk-tree (:body ast) this) ")"))
    (on-if [this ast] (str "(if (" (walk-tree (:cond ast) this) ") (" (walk-tree (:then ast) this) ")(" (walk-tree (or (:else ast) FALSE) this) ")"))
    (on-call [this ast] (str "(" (walk-tree (:func ast) this) " " (s/join " " (map #(walk-tree % this) (:args ast))) ")")))


(defn walk-tree ([ast cb]
        (case (:type ast)
              nil nil
              "prog" (on-prog cb ast)
              "num" (on-num cb ast)
              "str" (on-str cb ast)
              "bool" (on-bool cb ast)
              "var" (on-var cb ast)
              "binary" (on-binary cb ast)
              "assign" (on-assign cb ast)
              "lambda" (on-lambda cb ast)
              "let" (on-let cb ast)
              "if" (on-if cb ast)
              "call" (on-call cb ast)))
  ([ast] (walk-tree ast (dump-callback.))))

(defn to-clj [ast] (walk-tree ast (clojure-callback.)))

(defn dump [code] (walk-tree (parse_parse_toplevel code) (dump-callback.)))

