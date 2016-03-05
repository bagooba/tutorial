(defproject tutorial "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :template-additions [".gitignore" "start.sh" "stop.sh"]
  :main ^:skip-aot tutorial.core
  :target-path "target/%s"
  :source-paths ["src/clj"]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]]
                     :source-paths ["dev"]}
             :uberjar {:aot :all}})
