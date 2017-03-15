(ns lang.main
	(:gen-class))

(defn -main [& args]
	(do 
	(println "lang REPL, arguments: " args)
	(loop [nextinput (read-line)]
	(if (= "exit" nextinput) nil
            (do 
		(println (str "You typed: " nextinput))
		(recur (read-line)))))))

