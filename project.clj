(defproject lang "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.blancas/kern "1.1.0"]
                 [org.clojure/core.match "0.3.0"]
                 [insn "0.4.0"]
                 [com.gfredericks/debug-repl "0.0.11"]]
  :main ^:skip-aot lang.core
  :repl-options {:init-ns lang.core
                 :nrepl-middleware [com.gfredericks.debug-repl/wrap-debug-repl]})
