(ns lang.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn inputstream_next [inputstream_state]
	(let [{:keys [pos input line col]} inputstream_state 
		ch (if (> (count input) pos) (.charAt input pos) nil)
		nextpos (if (nil? ch) pos (inc pos))
		nextcol (if (nil? ch) col (inc col))]
		[ch (assoc inputstream_state :pos nextpos :col nextcol)]))

