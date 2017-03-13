(ns lang.environment)

(defn environment_create [parent]
	{:vars {} :parent parent})

(defn environment_lookup_impl [varname scope level]
	(if (nil? scope) nil
	(if (contains? (:vars scope) (keyword varname)) {:scope scope :level level} (recur varname (:parent scope) (inc level)))))

(defn environment_lookup [varname scope]
	(:scope (environment_lookup_impl varname scope 0)))

(defn environment_get [varname scope]
	(if (nil? scope)
		(throw (Exception. (str "Undefined variable " varname)))
		(let [vars (:vars scope)
			varname (keyword varname)]
		(if (contains? vars varname)
			(vars varname)
			(recur varname (:parent scope))))))

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

(defn environment_def [varname value scope]
	(assoc-in scope [:vars (keyword varname)] value))

