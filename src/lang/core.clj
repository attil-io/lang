(ns lang.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
		(>= pos (count input))))

(defn inputstream_croak [msg inputstream_state]
	(let [{:keys [pos line col]} inputstream_state]
		(throw (Exception. (str msg " at position " pos " (" line ":" col ")")))))

(defn is_keyword [x] (contains? #{"if" "then" "else" "lambda" "λ" "true" "false"} x))

(defn is_digit [ch] (and (not (nil? ch)) (not (nil? (re-matches #"[0-9]" (str ch))))))

(defn is_id_start [ch] (and (not (nil? ch)) (not (nil? (re-matches #"(?i)[a-zλ_]" (str ch))))))

(defn is_id [ch] (or (is_id_start ch) (and (not (nil? ch)) (>= (.indexOf "?!-<>=0123456789" ch) 0))))

(defn is_op_char [ch] (and (not (nil? ch)) (>= (.indexOf "+-*/%=&|<>!" (str ch)) 0)))

(defn is_punc [ch] (and (not (nil? ch)) (>= (.indexOf ",;(){}[]" (str ch)) 0)))

(defn is_whitespace [ch] (and (not (nil? ch)) (>= (.indexOf " \t\n" (str ch)) 0)))



