(ns advent-of-code-2021.10.core
  (:require
   [advent-of-code-2021.utils :as utils]))


(def data (-> "10/input" utils/slurp-lines))
(def test-data (-> "10/test-input" utils/slurp-lines))


(def pairs? #{[\{ \}]
              [\[ \]]
              [\( \)]
              [\< \>]})


(def opening? #{\( \[ \{ \<})


(def points {\) 3
             \] 57
             \} 1197
             \> 25137})


(defn- check-closing
  "Return
  - true, if balanced,
  - false, if incomplete,
  - character, if corrupted."
  [string]
  (loop [[fst & rst] string
         opening-acc '()]
    (cond
      (nil? fst)                         (empty? opening-acc)
      (opening? fst)                     (recur rst (conj opening-acc fst))
      (pairs? [(first opening-acc) fst]) (recur rst (rest opening-acc))
      :else                              fst)))


;; finally some fun with transducers!
(defn compute-1 [lines]
  (transduce
   (comp
    (map check-closing)
    (filter #(not (boolean? %)))
    (map points))
   +
   lines))


(println "Part 1: " (compute-1 data))


;; Part 2
(def pairs {\{ \}
            \[ \]
            \( \)
            \< \>})


(def points-2 {\) 1
               \] 2
               \} 3
               \> 4})


(defn- check-and-complete
  "Return
  - true, if balanced,
  - sequence of shortest completion, if incomplete,
  - character, if corrupted."
  [string]
  (loop [[fst & rst] string
         opening-acc '()]
    (cond
      (nil? fst)                         (map pairs opening-acc)
      (opening? fst)                     (recur rst (conj opening-acc fst))
      (pairs? [(first opening-acc) fst]) (recur rst (rest opening-acc))
      :else                              fst)))


(defn- compute-score-2 [se]
  (loop [score       0
         [fst & rst] se]
    (if (nil? fst)
        score
        (recur (+ (* 5 score) (get points-2 fst)) rst))))


(defn compute-2 [lines]
  (let [res-sorted (transduce
                    (comp
                     (map check-and-complete)
                     (filter seq?)
                     (map compute-score-2))
                    conj
                    lines)
        res-sorted (sort res-sorted)]
    (nth res-sorted (/ (count res-sorted) 2))))


(println "Part 2: " (compute-2 data))
