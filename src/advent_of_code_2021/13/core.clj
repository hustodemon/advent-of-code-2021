(ns advent-of-code-2021.13.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


;;
;; Terminology: dot=position of a dot (#) on the paper
;;


(defn- parse-dot [s]
  (map read-string
       (string/split s #",")))

(defn- read-instr [[ws ns]]
  [(keyword ws) (read-string ns)])


(defn- parse-instr [s]
  (->> s
       (re-find #"(.)=(\d*)")
       (rest)
       (read-instr)))


(defn- parse-data [f]
  (let [[dots instr] (->> (string/split (utils/slurp-res f) #"\n\n")
                          (map string/split-lines))]
    [(map parse-dot dots) (map parse-instr instr)]))


(defn- fold-x [[x y :as dot] where]
  (if (< x where)
    dot
    (list (- where (- x where)) y)))


(defn- fold [dot where instr]
  (if (= instr :x)
    (fold-x dot where)
    (reverse (fold-x (reverse dot) where))))


(defn- fold-dots [dots [[fst-inst fst-where] & rst-inst]]
  (if fst-inst
    (let [folded (map #(fold % fst-where fst-inst) dots)]
      (recur folded rst-inst))
    (into #{} dots)))


;; Part 1
(let [d            (parse-data "13/input")
      dots         (first d)
      instructions (second d)
      res          (fold-dots dots [(first instructions)])]
  (println "Part 1: " (count (into #{} res))))


;; Part 2
(defn- print-dots [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    (doseq [y    (range 0 (inc max-y))
            x    (range 0 (inc max-x))
            :let [c (if (get dots (list x y)) "█" ".")]]
      (print c)
      (when (= x max-x) (println)))))


(let [d            (parse-data "13/input")
      dots         (first d)
      instructions (second d)
      dots         (fold-dots dots instructions)]
  (println "Part 2: ")
  (print-dots dots))
