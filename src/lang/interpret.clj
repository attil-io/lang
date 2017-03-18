(ns lang.interpret
	(:require [lang.environment :refer :all])
	(:require [lang.parser :refer :all])
	(:require [lang.evaluate :refer :all])
	(:require [lang.util :refer :all]))

(defn- printimpl [env & args]
	(let [print_accum (environment_get "print_accum" env)
		print_line (apply str args)
		new_print_accum (conj print_accum print_line)]
	[print_line (environment_set "print_accum" new_print_accum env)]))

(defn- printlnimpl [env & args]
	(apply printimpl (cons env (conj args "\n"))))

(defn interpret [code] 
	(let [parsed (parse_parse_toplevel code)
		empty_env (environment_create nil) 
		global_env (environment_def "println" printlnimpl (environment_def "print" printimpl (environment_def "print_accum" [] empty_env)))
		[eval_result eval_env] (evaluate parsed {:vars {} :parent global_env})]
	[eval_result (environment_get "print_accum" eval_env)]))

