(ns lang.main
	(:require [lang.interpret :refer :all])
	(:gen-class))

(defn- prompt [] 
	(do
	(print "> ")
	(flush)
	(read-line)))

(defn- isexit [line]
	(= "exit" line))

(defn- isgo [line]
	(= "go" line))

(defn- safeinterpret [lines]
	(try
	(interpret lines)
	(catch Exception e (do (println (str "ERROR: " (.getMessage e))) [nil nil]))))

(def EXIT (take 3 (repeat nil)))
(def NO_RESULT (take 2 (repeat nil)))

(defn- processline [line lines]
	(cond 
	(isexit line) EXIT
	(isgo line) (conj (safeinterpret lines) "")
	:else [nil nil (str lines "\n" line)])) 

(defn- renderresult [result output]
	(println (str (clojure.string/join "\n" output) "\n\n -> " result)))

(defn- interactive [] 
	(loop [line (prompt) lines ""]
	(let [[result output newlines :as process_result] (processline line lines)]
	(when (not= EXIT process_result)
	(do
		(when (not= NO_RESULT [result output]) (renderresult result output))
		(recur (prompt) newlines))))))

(defn -main [& args]
	(do 
	(println "lang REPL, arguments: " args)
	(interactive)))

