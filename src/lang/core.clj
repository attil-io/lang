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

(defn tokenstream_is_keyword [x] (contains? #{"if" "then" "else" "lambda" "λ" "true" "false"} x))

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

(defn tokenstream_peek [tokenstream_state]
	(let [[current_token current_state] (if (< (count tokenstream_state) 2) (vec (cons nil tokenstream_state))tokenstream_state)]
		 (if (nil? current_token) (tokenstream_read_next current_state)  [current_token current_state])))

(def tokenstream_next tokenstream_peek)

(defn tokenstream_eof [tokenstream_state] (nil? (tokenstream_peek tokenstream_state)))


(defn parse_is_punc [ch tokenstream_state] 
	(let [[tok next_state] (tokenstream_peek tokenstream_state)]
		(and tok (= "punc" (:type tok)) (or (nil? ch) (= ch (:value tok))) tok)))

(defn parse_is_kw [ch tokenstream_state] 
	(let [[tok next_state] (tokenstream_peek tokenstream_state)]
		(and tok (= "kw" (:type tok)) (or (nil? ch) (= ch (:value tok))) tok)))

(defn parse_is_op [ch tokenstream_state] 
	(let [[tok next_state] (tokenstream_peek tokenstream_state)]
		(and tok (= "op" (:type tok)) (or (nil? ch) (= ch (:value tok))) tok)))

(defn parse_skip_punc [ch tokenstream_state] 
	(if (or (nil? ch) (parse_is_punc ch tokenstream_state))
		(tokenstream_next tokenstream_state)
		(inputstream_croak (str "Expecting punctuation: \"" ch "\"") (parser_tokenizer_state_part tokenstream_state))))

(defn parse_skip_kw [kw tokenstream_state]
	(if (or (nil? kw) (parse_is_kw kw tokenstream_state))
		(tokenstream_next tokenstream_state)
		(inputstream_croak (str "Expecting keyword: \"" kw "\"") (parser_tokenizer_state_part tokenstream_state))))

(defn parse_skip_op [op tokenstream_state]
	(if (or (nil? op) (parse_is_op op tokenstream_state))
		(tokenstream_next tokenstream_state)
		(inputstream_croak (str "Expecting operator: \"" op "\"") (parser_tokenizer_state_part tokenstream_state))))

(defn parse_unexpected [tokenstream_state]
	(inputstream_croak (str "Unexpected token: \"" ((parser_tokenizer_token_part (tokenstream_peek tokenstream_state)) :value) "\"") (parser_tokenizer_state_part tokenstream_state)))

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
		(let [[next_token next_state] (tokenstream_next tokenstream_state)
			[next_next_token next_next_state] (tokenstream_next [next_state]) 
			[next_right_token next_right_state] (parse_maybe_binary next_next_token his_prec [next_next_state])]
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

(defn parse_delimited [start stop separator parser token_stream_state]  [nil token_stream_state])

