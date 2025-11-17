
#_( this hash underscore means ignore following s expression )

(defproject day20 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [clj-commons/pomegranate "1.2.25"] #_(this is for finding those pesky jar files)
                 [com.fzakaria/slf4j-timbre "0.4.1"] #_(this is for silencing logger message errors)
                 [clj-antlr "0.2.14"] #_(this is for the parsing library )
                 ]
  :main ^:skip-aot day20.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
