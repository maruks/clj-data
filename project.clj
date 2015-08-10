(defproject org.clojars.maruks/maruks.data "0.0.2-SNAPSHOT"
  :description "Persistent data structures"
  :url "https://github.com/maruks/clj-data"
  :scm {:name "git"
        :url "https://github.com/maruks/clj-data.git"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :repl-options {:port 2323}
  :profiles {:dev {:dependencies [[criterium "0.4.3"]
                                  [org.clojure/test.check "0.7.0"]]}})
