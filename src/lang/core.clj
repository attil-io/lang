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
	(inputstream_croak (str "Unexpected token: \"" ((tokenstream_peek tokenstream_state) :value) "\"") (parser_tokenizer_state_part tokenstream_state)))

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
			[next_next_token next_next_state] (tokenstream_read_next next_state) 
			[next_right_token next_right_state] (parse_maybe_binary next_next_token his_prec next_next_state)]	; FIXME: parse_atom instead of next_next_token
			(parse_maybe_binary
				{
					:type "binary"
					:operator (:value next_token)
					:left left
					:right next_right_token
				} 
				my_prec
				next_right_state))
		[left tokenstream_state]))
	[left tokenstream_state]))

(defn parse_delimited [start stop separator parser token_stream_state]  
	(loop [state (parse_skip_punc start token_stream_state) accum [] isfirst true]
		(if (or (inputstream_eof state) (parse_is_punc stop state))
			[accum (parse_skip_punc stop state)]
			(let [skip_state (if isfirst state (parse_skip_punc separator state))
				[parser_res parser_state] (parser skip_state)]
				(recur parser_state (conj accum parser_res) false)))))

(defn parse_parse_call [func token_stream_state] 
	(let [[argsval argsstate] (parse_delimited \( \) \, tokenstream_read_next token_stream_state)] ; FIXME: parse_expression instead of tokenstream_read_next
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
					(tokenstream_read_next (tokenstream_next name_state))   ; FIXME: parse_expression instead of tokenstream_read_next
					[nil name_state])]
		[{:name name_val :def def_val} def_state]))

(def FALSE { :type "bool" :value false })

(defn parse_parse_let [token_stream_state]
	(let [state (parse_skip_kw "let" token_stream_state) token (tokenstream_peek state)]
	(if (= "var" (:type token))
		(let [[call_name call_state] (tokenstream_read_next state) [defs defsstate] (parse_delimited \( \) \, parse_parse_vardef call_state) bodystate {:pos 20, :input "let a (b = 5) { b; }", :line 0, :col 20}]
		[{
			:type "call"
			:func {
				:type "lambda"
				:name (:value call_name)
				:vars (map #(:name %) defs)
				:body {:type "var", :value "b"}	; FIXME: use parse_expression
			}
			:args (map #(or (:def %) FALSE) defs)
		} bodystate])
		(let [[vars vars_state] (parse_delimited \( \) \, parse_parse_vardef state) bodystate {:pos     18 :input "let (a = 5) { a; }" :line 0 :col 18}]
		[{
			:type "let"
			:vars vars
			:body {:type "var", :value "a"}		; FIXME: use parse_expression
		} bodystate]))))

