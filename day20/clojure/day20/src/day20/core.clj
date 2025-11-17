(ns day20.core
  (:gen-class))

;; (require '[cemerick.pomegranate :as pom]
;;          '[cemerick.pomegranate.aether :as aether])

;; (pom/add-dependencies :coordinates '[[incanter "1.9.2"]]
;;                              :repositories (merge aether/maven-central
;;                                                   {"clojars" "https://clojars.org/repo"}))

;; (pom/add-dependencies :coordinates '[[clj-antlr "0.2.14"]]
;;                              :repositories (merge aether/maven-central
;;                                                   {"clojars" "https://clojars.org/repo"}))

(require '[clojure.core.reducers :as r])
(require ['clj-antlr.core :as 'antlr])

(def myparse (antlr/parser "grammars/Day20.g4"))
;;(def myparse (antlr/parser "grammars/Day17.g4"))

;; map myparse over list of inputs seems to quash the compiler in compiler mode
(print (myparse "NSEW"))
(def t (map myparse (list "NS(NSE|WSE)")))

(def s 3)

;;(def s2 (myparse "x=581, y=396..399"))



(defn -main [& args]
  (print "hello world\n"))

