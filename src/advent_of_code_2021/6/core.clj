(ns advent-of-code-2021.6.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


;; a slightly more tidy version can be found in the core2 namespace

(def test-data [3 4 3 1 2])
(def real-data (-> "6/input"
                   (utils/slurp-res)
                   (string/split #",\s*")
                   (as-> ss (map read-string ss))))


(defn cyclic-dec [n]
  (if (< 0 n) (dec n) 6))


(defn step [fish]
  (let [count-producing (count (filter #(= % 0) fish))]
    (concat
     (map cyclic-dec fish)
     (repeat count-producing 8))))


(defn steps [fish] (iterate step fish))


(defn fish-count-after-days [fish count-days]
  (count (nth (steps fish) count-days)))


(println "Part 1: " (fish-count-after-days test-data 80))


;; In the part 2, we obviously need to be more efficient
;; we represent the game as a vector that holds the fish count grouped by time-to-give-birth (TTGB):
;; e.g.: [1 3 0 4] means there is 1 fish with 0 days time-to-give-birth, 3 fish with 1 day etc.
;; the max size of the vector is 8 (we have max 8 days TTGB)
;; computing next day is shifting the vector + adding "re-incarnated" to the
;; position 6

(defn fish->time-to-give-birth-counts ;; todo better name pls
  "Transform list of fish to a \"TTGB vector\" (counts of fish by TTGB)."
  [fishs] ;; no plural in English, wtf?
  (reduce (fn [acc fish] (update acc fish inc)) [0 0 0 0 0 0 0 0 0] fishs))


(defn shift-vector [v]
  (if (seq v)
    (conj 
     (subvec v 1)
     (first v))
    []))


(defn step-2 [[count-giving-birth & _ :as state]]
  (-> state
      (shift-vector)
      (update 6 (partial + count-giving-birth))))


(defn steps-2 [state] (iterate step-2 state))

(println "Part 2: " (apply +
                           (nth (steps-2 (fish->time-to-give-birth-counts real-data)) 256)))

