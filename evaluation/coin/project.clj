(defproject coin "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [anglican "1.0.0"]]
  :resource-paths ["/home/anik/anglican"]
  :main ^:skip-aot coin.coin)
  :target-path "target/%s"
  :repl-options {:init-ns coin.coin}
  :profiles {:uberjar {:aot :all}}