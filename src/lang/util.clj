(ns lang.util)

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
(defn mapwithpartialresults [fun coll] (loop [accum [] remainingcoll coll] (if (= 0 (count remainingcoll)) accum (recur (conj accum (fun (first remainingcoll) accum)) (rest remainingcoll)))))

(defn long-str [& strings] (clojure.string/join "\n" strings))

