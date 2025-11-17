(ns day20.core
  (:gen-class))

;; (require '[clojure.core.reducers :as r])
(require ['clj-antlr.core :as 'antlr])

;;(def myparse (antlr/parser "grammars/Day20.g4"))
(def myparse (antlr/parser "grammars/Day17.g4"))

;;(def t (myparse "NS(NSE|WSE)"))

(def s 3)

(def s2 (myparse "x=581, y=396..399"))


