(ns lang.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def resultpart first)
(def statepart second)

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
		dot_skip_state (if (= \. maybedot_ch) (statepart (inputstream_next intpart_state)) intpart_state)
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
	(inputstream_next (statepart (tokenstream_read_while #(not= % \newline) inputstream_state))))

(defn tokenstream_read_next [inputstream_state]
	(let [skip_whitespace_state (statepart (tokenstream_read_while tokenstream_is_whitespace inputstream_state))
		nextchar (if (inputstream_eof skip_whitespace_state) nil (inputstream_peek skip_whitespace_state))
		]
		(cond (nil? nextchar) nil
			(= \# nextchar) (tokenstream_read_next (tokenstream_skip_comment skip_whitespace_state))
			(= \" nextchar) (tokenstream_read_string (statepart (inputstream_next skip_whitespace_state)))
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
		tok))

