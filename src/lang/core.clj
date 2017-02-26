(ns lang.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def depth (atom 0))

(defn logfun [fun]
       (fn [& args]
           (do
               (swap! depth inc)
               (println (str (apply str (repeat @depth " ")) ">>> (" fun " " args ")"))
               (let [result (apply fun args)]
                    (do 
                         (swap! depth dec)
                         (println (str (apply str (repeat @depth " ")) " <<< " result))
                         result)))))

(def inputstream_result_part first)
(def inputstream_state_part second)
(def parser_tokenizer_token_part first)
(def parser_tokenizer_state_part last)

(declare inputstream_peek)
(declare inputstream_eof)

(defn inputstream_next [inputstream_state]
	(let [{:keys [pos input line col]} inputstream_state 
		ch (inputstream_peek inputstream_state)
		isnewline (= \newline ch)
		nextpos (if (nil? ch) pos (inc pos))
		nextcol (if (nil? ch) col (if isnewline 0 (inc col)))
		nextline (if isnewline (inc line) line)]
		[ch (assoc inputstream_state :pos nextpos :col nextcol :line nextline)]))

(defn inputstream_peek [inputstream_state]
	(let [{:keys [pos input]} inputstream_state 
		ch (if (inputstream_eof inputstream_state) nil (.charAt input pos))]
		ch))

(defn inputstream_eof [inputstream_state]
	(let [{:keys [pos input]} inputstream_state]
		(or (nil? pos) (>= pos (count input)))))

(defn inputstream_croak [msg inputstream_state]
	(let [{:keys [pos line col]} inputstream_state]
		(throw (Exception. (str msg " at position " pos " (" line ":" col ")")))))

(defn tokenstream_is_keyword [x] (contains? #{"let" "if" "then" "else" "lambda" "λ" "true" "false"} x))

(defn tokenstream_is_digit [ch] (and (not (nil? ch)) (not (nil? (re-matches #"[0-9]" (str ch))))))

(defn tokenstream_is_id_start [ch] (and (not (nil? ch)) (not (nil? (re-matches #"(?i)[a-zλ_]" (str ch))))))

(defn tokenstream_is_id [ch] (or (tokenstream_is_id_start ch) (and (not (nil? ch)) (>= (.indexOf "?!-<>=0123456789" (str ch)) 0))))

(defn tokenstream_is_op_char [ch] (and (not (nil? ch)) (>= (.indexOf "+-*/%=&|<>!" (str ch)) 0)))

(defn tokenstream_is_punc [ch] (and (not (nil? ch)) (>= (.indexOf ",;(){}[]" (str ch)) 0)))

(defn tokenstream_is_whitespace [ch] (and (not (nil? ch)) (>= (.indexOf " \t\n" (str ch)) 0)))

(defn tokenstream_read_while [predicate inputstream_state]
      (loop [res "" state inputstream_state]
	    (if (not (and (not (inputstream_eof state)) (predicate (inputstream_peek state))))
		[res state]
		(let [[nextch nextstate] (inputstream_next state)]
		      (recur (str res nextch) nextstate)))))

(defn tokenstream_read_number [inputstream_state] 
	(let [[intpart intpart_state] (tokenstream_read_while #(and (not= \. %) (tokenstream_is_digit %)) inputstream_state)
		maybedot_ch (inputstream_peek intpart_state)
		dot_skip_state (if (= \. maybedot_ch) (inputstream_state_part (inputstream_next intpart_state)) intpart_state)
		[decpart decpart_state] (if (= \. maybedot_ch) (tokenstream_read_while #(and (not= \. %) (tokenstream_is_digit %)) dot_skip_state) nil)
		final_state (if (nil? decpart_state) intpart_state decpart_state)
		num_str (str intpart (if (nil? decpart) "" (str "." decpart)))
		number_numeric (if (nil? decpart_state) (. Integer parseInt num_str) (. Double parseDouble num_str))
	     ]
	     [{:value number_numeric :type "num"}
	       final_state])) 

(defn tokenstream_read_ident [inputstream_state] 
	(let [[id_val id_state] (tokenstream_read_while tokenstream_is_id inputstream_state)
		found_id (< 0 (count id_val))
		id_type (and found_id (if (tokenstream_is_keyword id_val) "kw" "var"))
		ret_val (if found_id {:value id_val :type id_type} {})	
		]
	[ret_val id_state]))

(defn tokenstream_read_escaped [inputstream_state end]
	(loop [resultstr "" state inputstream_state finished false]
		(if (or (inputstream_eof state) finished)
			[resultstr state]
			(let [[[newresult_val newresult_state] escaped]
				(let [[ch_val ch_state :as ch_result] (inputstream_next state)]
					(if (= ch_val \\) [(inputstream_next ch_state) true] [ch_result false]))
				finished (and (not escaped) (= end newresult_val))
				]
				(recur (str resultstr (if finished "" newresult_val)) newresult_state finished)))))


(defn tokenstream_read_string [inputstream_state]
      (let [[read_val read_state] (tokenstream_read_escaped inputstream_state \")]
        [{:type "str" :value read_val} read_state]))

(defn tokenstream_skip_comment [inputstream_state] 
	(inputstream_next (inputstream_state_part (tokenstream_read_while #(not= % \newline) inputstream_state))))

(defn tokenstream_read_next [inputstream_state]
	(let [skip_whitespace_state (inputstream_state_part (tokenstream_read_while tokenstream_is_whitespace inputstream_state))
		nextchar (if (inputstream_eof skip_whitespace_state) nil (inputstream_peek skip_whitespace_state))
		]
		(cond (nil? nextchar) nil
			(= \# nextchar) (tokenstream_read_next (tokenstream_skip_comment skip_whitespace_state))
			(= \" nextchar) (tokenstream_read_string (inputstream_state_part (inputstream_next skip_whitespace_state)))
			(tokenstream_is_digit nextchar) (tokenstream_read_number skip_whitespace_state)
			(tokenstream_is_id_start nextchar) (tokenstream_read_ident skip_whitespace_state)
			(tokenstream_is_punc nextchar)  (let [[nextval nextstate] (inputstream_next skip_whitespace_state)]
				[{:type "punc" :value nextval}  nextstate])
			(tokenstream_is_op_char nextchar) (let [[nextval nextstate] (tokenstream_read_while tokenstream_is_op_char skip_whitespace_state)]
				[{:type "op" :value nextval}  nextstate])
			:else (inputstream_croak (str "Can't handle character: " nextchar) skip_whitespace_state)
)))

(defn tokenstream_peek [inputstream_state]
	(inputstream_result_part (tokenstream_read_next inputstream_state)))

(defn tokenstream_next [inputstream_state]
	(inputstream_state_part (tokenstream_read_next inputstream_state)))

(defn tokenstream_eof [tokenstream_state] (nil? (tokenstream_peek tokenstream_state)))

(declare parse_parse_atom)
(declare parse_parse_expression)
(declare parse_parse_prog)

(defn parse_is_punc [ch tokenstream_state] 
	(let [tok (tokenstream_peek tokenstream_state)]
		(and tok (= "punc" (:type tok)) (or (nil? ch) (= ch (:value tok))) tok)))

(defn parse_is_kw [ch tokenstream_state] 
	(let [tok (tokenstream_peek tokenstream_state)]
		(and tok (= "kw" (:type tok)) (or (nil? ch) (= ch (:value tok))) tok)))

(defn parse_is_op [ch tokenstream_state] 
	(let [tok (tokenstream_peek tokenstream_state)]
		(and tok (= "op" (:type tok)) (or (nil? ch) (= ch (:value tok))) tok)))

(defn parse_skip_punc [ch tokenstream_state] 
	(if (or (nil? ch) (parse_is_punc ch tokenstream_state))
		(tokenstream_next tokenstream_state)
		(inputstream_croak (str "Expecting punctuation: \"" ch "\"") tokenstream_state)))

(defn parse_skip_kw [kw tokenstream_state]
	(if (or (nil? kw) (parse_is_kw kw tokenstream_state))
		(tokenstream_next tokenstream_state)
		(inputstream_croak (str "Expecting keyword: \"" kw "\"") tokenstream_state)))

(defn parse_skip_op [op tokenstream_state]
	(if (or (nil? op) (parse_is_op op tokenstream_state))
		(tokenstream_next tokenstream_state)
		(inputstream_croak (str "Expecting operator: \"" op "\"") tokenstream_state)))

(defn parse_unexpected [tokenstream_state]
	(inputstream_croak (str "Unexpected token: \"" ((tokenstream_peek tokenstream_state) :value) "\"") tokenstream_state))

(def PRECEDENCE {
        "="      1
        "||"     2
        "&&"     3
        "<"      7
        ">"      7
        "<="     7
        ">="     7
        "=="     7
        "!="     7
        "+"     10
        "-"     10
        "*"     20
        "/"     20
        "%"     20
})

(defn parse_maybe_binary [left my_prec tokenstream_state] 
	(if-let [tok (parse_is_op nil tokenstream_state)]
	(let [his_prec (PRECEDENCE (:value tok))]
		(if (> his_prec my_prec)
		(let [[next_token next_state] (tokenstream_read_next tokenstream_state)
			[next_next_token next_next_state] (parse_parse_atom next_state)
			[next_right_token next_right_state] (parse_maybe_binary next_next_token his_prec next_next_state)]
			(parse_maybe_binary
				{
					:type (if (= "=" (:value next_token)) "assign" "binary")
					:operator (:value next_token)
					:left left
					:right next_right_token
				} 
				my_prec
				next_right_state))
		[left tokenstream_state]))
	[left tokenstream_state]))

(defn parse_delimited [start stop separator parser token_stream_state]  
	(loop [state (parse_skip_punc start token_stream_state) accum []]
		(if (or (inputstream_eof state) (parse_is_punc stop state))
			[accum (parse_skip_punc stop state)]
			(let [[parser_res parser_state] (parser state)
				next_state (if (parse_is_punc separator parser_state) (parse_skip_punc separator parser_state) parser_state)]
			(recur next_state (conj accum parser_res))))))

(defn parse_parse_call [func token_stream_state] 
	(let [[argsval argsstate] (parse_delimited \( \) \, parse_parse_expression token_stream_state)]
	[{
		:type "call"
		:func func
		:args argsval
	} argsstate]))

(defn parse_parse_varname [token_stream_state] 
	(let [[next_token next_state] (tokenstream_read_next token_stream_state)]
		(if (not= "var" (:type next_token))
			(inputstream_croak "Expecting variable name" token_stream_state)
			[(:value next_token) next_state])))

(defn parse_parse_vardef [token_stream_state]
	(let [[name_val name_state] (parse_parse_varname token_stream_state)
		[def_val def_state] (if (parse_is_op "=" name_state)
					(parse_parse_expression (tokenstream_next name_state)) 
					[nil name_state])]
		[{:name name_val :def def_val} def_state]))

(def FALSE { :type "bool" :value false })

(defn parse_parse_let [token_stream_state]
	(let [state (parse_skip_kw "let" token_stream_state) token (tokenstream_peek state)]
	(if (= "var" (:type token))
		(let [[call_name call_state] (tokenstream_read_next state) 
			[defs defs_state] (parse_delimited \( \) \, parse_parse_vardef call_state)
			[body_content body_state] (parse_parse_expression defs_state)]
		[{
			:type "call"
			:func {
				:type "lambda"
				:name (:value call_name)
				:vars (map #(:name %) defs)
				:body body_content
			}
			:args (map #(or (:def %) FALSE) defs)
		} body_state])
		(let [[vars vars_state] (parse_delimited \( \) \, parse_parse_vardef state)
			[body_content body_state] (parse_parse_expression vars_state)]
		[{
			:type "let"
			:vars vars
			:body body_content
		} body_state]))))

(defn parse_parse_if [token_stream_state]
	(let [precond_state (parse_skip_kw "if" token_stream_state)
		[cond_val cond_state] (parse_parse_expression precond_state)
		[then_val then_state] (parse_parse_expression (if (parse_is_punc \{ cond_state) cond_state (parse_skip_kw "then" cond_state)))
		[else_val else_state] (if (parse_is_kw "else" then_state)
					(parse_parse_expression (parse_skip_kw "else" then_state))
					[nil then_state])]
	[(conj {
		:type "if"
		:cond cond_val
		:then (if (= [] then_val) FALSE then_val)
	} (when else_val [:else (if (= [] else_val) FALSE else_val)]))
	else_state]))
		
(defn parse_parse_lambda[token_stream_state]
	(let [[nameval namestate] (if (= "var" (:type (tokenstream_peek token_stream_state)))
					(tokenstream_read_next token_stream_state) [nil token_stream_state])
		[varval varstate] (parse_delimited \( \) \, parse_parse_varname namestate)
		[bodyval bodystate] (parse_parse_expression varstate)]
	[{
		:type "lambda"
		:name (:value nameval)
		:vars varval
		:body bodyval
	} bodystate]))

(defn parse_parse_bool [token_stream_state]
	(let [[nextval nextstate] (tokenstream_read_next token_stream_state)]
	[{
		:type "bool"
		:value (= "true" (:value nextval))
	} nextstate]))

(defn parse_maybe_call [expr token_stream_state] 
	(let [[expr new_state] (expr token_stream_state)]
	(if (parse_is_punc \( new_state)
		(parse_parse_call expr new_state)
		[expr new_state])))

(defn parse_parse_atom [tss]
	(parse_maybe_call (fn [token_stream_state]
		(cond 
		(parse_is_punc \( token_stream_state)
			(let [next_state (tokenstream_next token_stream_state)
				[exp_val exp_state] (parse_parse_expression next_state)
				after_punc_state (parse_skip_punc \) exp_state)] 
			[exp_val after_punc_state]) 
		(parse_is_punc \{ token_stream_state) (parse_parse_prog token_stream_state)
		(parse_is_op "!" token_stream_state)
			(let [[expr_result expr_state] (parse_parse_expression (tokenstream_next token_stream_state))]
				[{:type "not" :body expr_result} expr_state])
		(parse_is_kw "let" token_stream_state) (parse_parse_let token_stream_state)
		(parse_is_kw "if" token_stream_state) (parse_parse_if token_stream_state)
		(or (parse_is_kw "true" token_stream_state) (parse_is_kw "false" token_stream_state)) (parse_parse_bool token_stream_state)
		(or (parse_is_kw "lambda" token_stream_state) (parse_is_kw "λ" token_stream_state))
			(parse_parse_lambda (tokenstream_next token_stream_state))
		(let [next_tok (tokenstream_peek token_stream_state)] (contains? #{"var" "num" "str"} (:type next_tok)))
			(tokenstream_read_next token_stream_state)
		:else (parse_unexpected token_stream_state)))
	tss))

(defn parse_parse_expression [token_stream_state] 
	(parse_maybe_call #(
		let [[parse_atom_value parse_atom_state] (parse_parse_atom %1)]
		(parse_maybe_binary parse_atom_value 0 parse_atom_state)) token_stream_state))

(defn parse_parse_prog [token_stream_state]
	(let [[prog_parsed prog_state] (parse_delimited \{ \} \; parse_parse_expression token_stream_state)
		prog (cond 
			(= 0 (count prog_parsed)) FALSE
			(= 1 (count prog_parsed)) (prog_parsed 0)
			:else {:type "prog" :prog prog_parsed})]
	[prog prog_state]))

(defn parse_parse_toplevel [input]
	(loop [token_stream_state {:pos 0 :input input :line 0 :col 0}
		prog []]
	(if (tokenstream_eof token_stream_state) 
		[{:type "prog" :prog prog}]
		(let [[parse_result parse_state] (parse_parse_expression token_stream_state)
			next_state (if (tokenstream_eof parse_state) parse_state (parse_skip_punc \; parse_state))] 
			(recur next_state (conj prog parse_result))))))

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

(defn evaluate [expression environment]
	(case (:type expression)
		"num" (:value expression)
		"str" (:value expression)
		"bool" (:value expression)
		"var" (environment_get (:value expression) environment)
		"assign" (environment_set (:value (:left expression)) (evaluate (:right expression) environment_set) environment)))

