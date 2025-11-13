(ns day17.core
  (:gen-class))

(require ['clj-antlr.core :as 'antlr])

;; (def json (antlr/parser "grammars/Json.g4"))
(def parser (antlr/parser "grammars/Day17.g4"))

;; slurp file ?
(def input (slurp "../../example.txt"))
(def string-lines (clojure.string/split-lines input))
(def data (map parser string-lines))

(defn decode [filename]
  (let [in (slurp filename)
        string-lines (clojure.string/split-lines in)    
        data (map parser string-lines)
        data2 (map (fn [d] (let [[_ v1 s1 _ _ v2 s2 _ _ s3] d]
                             (list ((fn [s] (cond (= s "x=") :x
                                                  (= s "y=") :y
                                                  true (throw "not found")))
                                                    v1)
                                   (Integer/parseInt s1)
                                   ((fn [s] (cond (= s "x=") :x
                                                  (= s "y=") :y
                                                  true (throw "not found"))) v2)
                                   (Integer/parseInt s2)
                                   (Integer/parseInt s3)
                                   ))) data)
        ]
    data2))

;; now we can decode input files directly
;;(decode "../../example.txt") =>
;; ((:x 495 :y 2 7)
;;  (:y 7 :x 495 501)
;;  (:x 501 :y 3 7)
;;  (:x 498 :y 2 4)
;;  (:x 506 :y 1 2)
;;  (:x 498 :y 10 13)
;;  (:x 504 :y 10 13)
;;  (:y 13 :x 498 504))








  

  



;; ;; ;; drop 1st and last char from input for some reason we have parens around the input text
;; ;; (defn butfirst-last
;; ;;   [str]
;; ;;   (let* [word str
;; ;;          wordlen (count word)]
;; ;;     (if (< wordlen 2)
;; ;;       ""
;; ;;       (let* [word1 (subs word 0 (- wordlen 1))
;; ;;              word2 (subs word1 1)]
;; ;;         word2))))
;; ;; (butfirst-last "alpha")
;; ;; (butfirst-last "")

;; ;;(def string-lines (map butfirst-last string-lines))

;; ;;(def test-string1 (str (get string-lines 0) \newline))
;; (def test-string1 (get string-lines 0))


;; ;; add newline to string
;; ;; (conj \n "123")
;; ;; (str "123" \newline )


;; (count "alpha")

;; (subs "alpha" 1)

;; (pprint (json "[1,2,3]"))

;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))

;; (pprint (parser test-string1))

;; (nth (parser test-string1) 0)
;; (nth (parser test-string1) 1) ;; 
;; (nth (parser test-string1) 2)
;; (nth (parser test-string1) 3) ;; 
;; (nth (parser test-string1) 4)
;; (nth (parser test-string1) 5) ;; 
;; (nth (parser test-string1) 6)
;; (nth (parser test-string1) 7)
;; (nth (parser test-string1) 8) ;;

;; (def ast (map (fn [str]
;;                 (let [x (parser str)]
;;                   (list (Integer. (nth x 1))
;;                         (Integer. (nth x 3))
;;                         (nth (nth x 5) 0)
;;                         (nth x 8))))
;;               string-lines))

;; ;; (def ast2 (map (fn [x] x) string-lines))

;; ;; does each character occur that many times in a string ?
;; (defn occurs
;;   "how many times a given character appears in a string"
;;   [ch str]
;;   (let [len (count str)]
;;     (defn f [i c]
;;       (loop [i i
;;              c c]
;;         (cond
;;           (>= i len) c
;;           (= (nth str i) ch) (recur (+ i 1) (+ c 1))
;;           true (recur (+ i 1) c))))
;;     (f 0 0)))

;; (occurs \a "aaa")
;; (occurs \b "acbac")
;; (occurs \c "abd")
;; (occurs \a "")


;; ;; for each ast , find how many times character appears in string
;; ;; does character appear required number of times 

;; (defn ast-f  
;;   [x]
;;   (let* [from (nth x 0)
;;          to (nth x 1)
;;          ch (nth x 2)
;;          word (nth x 3)
;;          occ (occurs ch word)
;;          ]
;;     (and (>= occ from) (<= occ to))))
    

;; (ast-f (nth ast 0))


;; (map ast-f ast)

;; (filter (fn [x] (if x x false)) (map ast-f ast))

;; (def part1 (count (filter (fn [x] (if x x false)) (map ast-f ast))))
;; ;; part1 => 542
;; ;; accepted answer


;; ;; part2 the two numbers in the ast say letter must occur only either at 1st number or 2nd number , but not both
;; ;; to be a valid password

;; (defn dodgy-nth
;;   "if index n supplied into string str then return false otherwise compare with character ch"
;;   [str n ch]
;;   (let [len (count str)]
;;     (cond
;;       (>= n len) false
;;       (< n 0) false
;;       true (= (nth str n) ch))))

;; (dodgy-nth "word" 0 \w)
;; (dodgy-nth "word" 1 \o)
;; (dodgy-nth "word" 2 \r)
;; (dodgy-nth "word" 3 \d)
;; (dodgy-nth "word" 4 \?)
    

;; ;; off by 1 error - nth 0 is first character , whereas 1 is first character in puzzle
;; ;; 

;; (defn ast-valid?
;;   [x]
;;   (let* [from (nth x 0)
;;          to (nth x 1)
;;          ch (nth x 2)
;;          word (nth x 3)
;;          occ1 (dodgy-nth word from ch)
;;          occ2 (dodgy-nth word to ch)
;;          ]
;;     (cond
;;       (and occ1 (not occ2)) true
;;       (and occ2 (not occ1)) true
;;       true false)))

;; ;; filter truthy values , then count how many we got
;; (def part2 (count (filter (fn [x] (if x x false)) (map ast-valid? ast))))
;; ;; part2 ==> 434
;; ;; wrong ! what went wrong ??
;; ;;



;; (defn ast-valid?
;;   [x]
;;   (let* [from (nth x 0)
;;          to (nth x 1)
;;          ch (nth x 2)
;;          word (nth x 3)
;;          occ1 (dodgy-nth word (- from 1) ch)
;;          occ2 (dodgy-nth word (- to 1) ch)
;;          ]
;;     (cond
;;       (and occ1 (not occ2)) true
;;       (and occ2 (not occ1)) true
;;       true false)))

;; ;; filter truthy values , then count how many we got
;; (def part2 (count (filter (fn [x] (if x x false)) (map ast-valid? ast))))
;; ;; part2 ==> 360
;; ;; accepted answer

;; ;; mistake or confusion comes from string indexing
;; ;; in puzzle itself uses 1 as 1st index into string
;; ;; clojure and many other languages use 0 as 1st index into string






;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))
