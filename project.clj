(defproject lang "0.1.0-SNAPSHOT"
  :description "programming language based on http://lisperator.net/pltut/ tutorial"
  :url "https://github.com/attil-io/lang"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
;  :main ^:skip-aot lang.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all} :dev {:plugins [[com.jakemccrary/lein-test-refresh "0.18.1"]]}})

