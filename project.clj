(defproject com._0xab/common-clojure-platform "0.4.0"
 :description "Misc Clojure code shared among multiple projects, these have platform-specific dependencies that aren't handled by lein"
 :url "https://github.com/abarbu/common-clojure-platform"
 :license {:name "Eclipse Public License"
           :url "http://www.eclipse.org/legal/epl-v10.html"}
 :dependencies [[org.clojure/clojure "1.5.1"]
                [bwo/monads "0.2.0"]
                [opencv/opencv "2.4.7"]
                [opencv/opencv-native "2.4.7"]
                [com._0xab/nondeterminism "0.1.0"]
                [com._0xab/sanity "1.4.0"]
                [commons-io "2.4"]
                [incanter "1.5.5"]
                [gloss "0.2.1"]
                [org.jblas/jblas "1.2.3"]
                [com.taoensso/timbre "3.2.1"]
                [com._0xab/clatrix "0.4.0-SNAPSHOT"]
                [com._0xab/common-clojure "0.4.0"]
                ;; non-canonical fork because upstream hasn't been updated since 1.0
                ;; but this is the original dev
                [org.clojars.rosejn/clansi "1.2.0-SNAPSHOT"]])
