(ns lang.evaluate
	(:require [lang.environment :refer :all])
	(:require [lang.util :refer :all]))

(declare evaluate_apply_op)
(declare evaluate_make_lambda)

(defn evaluate [expression environment]
	(case (:type expression)
		"num" [(:value expression) environment]
		"str" [(:value expression) environment]
		"bool" [(:value expression) environment]
		"var" [(environment_get (:value expression) environment) environment]
		"assign" (if (= "var" (:type (:left expression))) 
			(let [[eval_result eval_env] (evaluate (:right expression) environment)]
				[eval_result (environment_set (:value (:left expression)) eval_result eval_env)])
			(throw (Exception. (str "Cannot assign to " (:left expression)))))
		"binary" (let [[eval_left_result eval_left_env] (evaluate (:left expression) environment)

				[eval_right_result eval_right_env] (evaluate (:right expression) eval_left_env)]
[(evaluate_apply_op (:operator expression) eval_left_result eval_right_result) eval_right_env])
		"lambda" [(evaluate_make_lambda expression) environment]
		"if" (let [[cond_result cond_env] (evaluate (:cond expression) environment)]
			(if cond_result
				(evaluate (:then expression) cond_env)
			(if (nil? (:else expression)) [false cond_env] (evaluate (:else expression) cond_env))))
		"prog" (let [prog (:prog expression)
				proglen (count prog)]
			(reduce (fn [[value env] progidx] (evaluate (nth prog progidx) env)) [false environment] (range proglen)))
		"let"  (let [{:keys [vars body]} expression
				env (environment_create environment)
				extended_env (reduce (fn [actenv newvar] (let [[eval_val eval_env] (evaluate (:def newvar) actenv)](environment_def (:name newvar) eval_val eval_env))) env vars)
				[result result_env] (evaluate body extended_env)]
			[result (:parent result_env)])
		"call" (let [[func_val _] (evaluate (:func expression) environment)
				args (:args expression)
				mapped_args_envs (mapwithpartialresults 
						(fn [idx prevs] 
							(let [arg (nth args idx)
								env (if (< 0 (count prevs)) (last (last prevs)) environment)]
							(evaluate arg env)))
						(range (count args)))
				mapped_args (map first mapped_args_envs)
				mapped_env (last (last mapped_args_envs))]
			(apply func_val (cons mapped_env mapped_args)))
		(throw (Exception. (str "I don't know how to evaluate " (:type expression))))))

(defn- evaluate_apply_op [op a b]
	(letfn [(isnum [x] 
			(if (number? x) x (throw (Exception. (str "Expected number but got " x)))))
		(div [x]
			(if (not= 0 (isnum x)) x (throw (Exception. "Divide by zero"))))]
	(case op
		"+" (+ (isnum a) (isnum b))
		"-" (- (isnum a) (isnum b))
		"*" (* (isnum a) (isnum b))
		"/" (/ (isnum a) (div b))
		"%" (mod (isnum a) (div b))
		"&&" (and a b)
		"||" (or a b)
		"<" (< (isnum a) (isnum b))
		">" (> (isnum a) (isnum b))
		"<=" (<= (isnum a) (isnum b))
		">=" (>= (isnum a) (isnum b))
		"==" (== a b)
		"!=" (not= a b)
		(throw (Exception. (str "Can't apply operator " op))))))

(defn- evaluate_make_lambda[exp] 
	(fn [env & args] 
		(let [names (:vars exp)
			scope (environment_create env)
			extended_args (concat args (repeat (- (count names) (count args)) false))
			extended_scope (reduce (fn [current_scope i]
				(environment_def (names i) (nth extended_args i) current_scope))
				scope
				(range (count names)))
			[eval_result eval_scope] (evaluate (:body exp) extended_scope)]
		[eval_result (:parent eval_scope)])))


