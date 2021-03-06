(ns lang.environment)

(defn environment_create [parent]
	{:vars {} :parent parent :closure {}})

(defn- environment_lookup_impl [varname scope level]
	(if (nil? scope) nil
	(if (contains? (:vars scope) (keyword varname)) {:scope scope :level level} (recur varname (:parent scope) (inc level)))))

(defn environment_lookup [varname scope]
	(:scope (environment_lookup_impl varname scope 0)))

(defn environment_get [varname scope]
	(if (nil? scope)
		(throw (Exception. (str "Undefined variable " varname)))
		(let [vars (:vars scope)
			closure (:closure scope)
			varname (keyword varname)]
		(if (contains? vars varname)
			(vars varname)
			(if (contains? closure varname)
				(closure varname)
				(recur varname (:parent scope)))))))

(defn environment_get_global [varname scope] 
	(if (nil? (:parent scope)) (environment_get varname scope)
	(environment_get_global varname (:parent scope))))

(defn environment_set [varname value scope]
	(let [{ancestor_scope :scope ancestor_level :level} (environment_lookup_impl varname scope 0)
		varname (keyword varname)]
	(cond 
		(and (nil? ancestor_scope) (nil? (:parent scope)))
			(throw (Exception. (str "Undefined variable " varname)))
		(nil? ancestor_scope)
			(assoc-in scope [:vars varname] value)
		:else
			(assoc-in scope (concat (repeat ancestor_level :parent) [:vars varname]) value))))

(defn- scope_depth [scope level]
	(if (nil? (:parent scope)) level (scope_depth (:parent scope) (inc level))))

(defn environment_set_global [varname value scope] 
	(assoc-in scope (concat (repeat (scope_depth scope 0) :parent) [:vars (keyword varname)]) value))

(defn environment_def [varname value scope]
	(assoc-in scope [:vars (keyword varname)] value))

(defn environment_def_closure [varname value scope]
	(assoc-in scope [:closure (keyword varname)] value))

(defn environment_def_closure_more [varnames values scope]
	(reduce #(environment_def_closure (first %2) (second %2) %) scope (partition 2 (interleave varnames values))))

