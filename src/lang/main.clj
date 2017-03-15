(ns lang.main
	(:require [lang.interpret :refer :all])
	(:gen-class))

(defn -main [& args]
	(do 
	(println "lang REPL, arguments: " args)
	(print "> ") (flush)
	(loop [nextinput (read-line)]
	(if (= "exit" nextinput) nil
            (do 
		(try
		(let [[result output] (interpret nextinput)]
			(println (str (clojure.string/join " " output) "\n\n >> " result)))
		(catch Exception e (println (str "ERROR: " (.getMessage e)))))
		(print "> ") (flush)
		(recur (read-line)))))))

