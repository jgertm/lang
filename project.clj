(defproject lang "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.blancas/kern "1.1.0"]
                 [org.clojure/core.match "0.3.0"]
                 [insn "0.4.0"]
                 [potemkin "0.4.5"]]
  :main ^:skip-aot lang.core
  :target-path "target/%s/"
  :java-source-paths ["tmp"]
  :profiles {:uberjar {:aot            :all
                       :resource-paths ["config/uberjar"]}
             :dev     {:source-paths ["dev"]
                       :dependencies [[com.gfredericks/debug-repl "0.0.11"]
                                      [midje "1.9.9"]
                                      [nubank/matcher-combinators "3.1.1"]]
                       :resource-paths ["config/dev"]}} 
  :repl-options {:init-ns          lang.core
                 :nrepl-middleware [com.gfredericks.debug-repl/wrap-debug-repl]})
