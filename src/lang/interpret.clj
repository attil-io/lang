(ns lang.interpret
	(:require [lang.environment :refer :all])
	(:require [lang.parser :refer :all])
	(:require [lang.evaluate :refer :all])
	(:require [lang.util :refer :all]))

(defn- printimpl [env & args]
	(let [print_accum (environment_get_global "print_accum" env)
		print_line (apply str args)
		new_print_accum (conj print_accum print_line)]
	[print_line (environment_set_global "print_accum" new_print_accum env)])) 

(defn- printlnimpl [env & args]
	(apply printimpl (cons env (conj (last args) "\n"))))

(defn- printenvimpl [env & args]
	(loop [vars (:vars env) e env current_printed "" current_env env]
		(if (nil? vars) [current_printed current_env]
		(let [[new_printed new_env] (apply printlnimpl (cons current_env (dumpmap vars)))
			parent (:parent e)]
		(recur (if (nil? parent) nil (:vars parent)) parent (str current_printed new_printed) new_env)))))


(defn- with-empty-closure [fun]
	[fun {:vars {} :parent {}}])

(defn interpret [code] 
	(let [parsed (parse_parse_toplevel code)
		empty_env (environment_create nil) 
		global_env (environment_def "printenv" (with-empty-closure printenvimpl) (environment_def "println" (with-empty-closure printlnimpl) (environment_def "print" (with-empty-closure printimpl) (environment_def "print_accum" [] empty_env))))
		[eval_result eval_env] (evaluate parsed {:vars {} :parent global_env})]
	[eval_result (environment_get "print_accum" eval_env)]))

