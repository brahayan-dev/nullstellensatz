(defproject nullstellensatz "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.219"]
                 [org.clojure/math.combinatorics "0.2.0"]]
  :main ^:skip-aot nullstellensatz.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[nubank/matcher-combinators "3.8.8"]]
                   :plugins [[com.github.clojure-lsp/lein-clojure-lsp "1.4.0"]]}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :aliases {"format-fix" ["clojure-lsp" "format"]
            "format" ["clojure-lsp" "format" "--dry"]
            "clean-ns-fix" ["clojure-lsp" "clean-ns"]
            "diagnostics" ["clojure-lsp" "diagnostics"]
            "clean-ns" ["clojure-lsp" "clean-ns" "--dry"]
            "list-fix" ["do" ["format-fix"] ["clean-ns-fix"]]
            "lint" ["do" ["diagnostics"] ["format"] ["clean-ns"]]})
