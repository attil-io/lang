(defproject lang "0.1.0-SNAPSHOT"
  :description "programming language based on http://lisperator.net/pltut/ tutorial"
  :url "https://github.com/attil-io/lang"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot lang.main
  :target-path "target/%s"
  :aliases {"test-integration" ["run" test-integration/lang/test.lang]}
  :profiles {:uberjar {:aot :all} :dev {:plugins [[com.jakemccrary/lein-test-refresh "0.18.1"]]}})

