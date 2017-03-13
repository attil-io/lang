(ns lang.token_stream
	(:require [lang.input_stream :refer :all]))

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

