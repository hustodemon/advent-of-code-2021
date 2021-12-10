(ns advent-of-code-2021.6.core2
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


(def real-data (-> "6/input"
                   (utils/slurp-res)
                   (string/split #",\s*")
                   (as-> ss (map read-string ss))))


(defn data->state
  "Transform list of fish to a vector of counts of fish with given
  \"time-to-give-birth\".
  E.g.: [1 2 3] = 1 fish giving birth now (day 0), 2 fish giving birth in 1 day
  etc."
  [fishs] ;; no plural in English :/
  (reduce (fn [acc fish] (update acc fish inc)) [0 0 0 0 0 0 0 0 0] fishs))

(defn shift-vector [v]
  (if (seq v)
    (conj 
     (subvec v 1)
     (first v))
    []))

(defn step [[count-giving-birth & _ :as state]]
  (-> state
      (shift-vector)
      (update 6 (partial + count-giving-birth))))

(defn steps [state] (iterate step state))

(defn count-of-fish-at-day [data day] ;; data is a vector of numbers
  (let [state-at-day (nth (steps (data->state data)) day)]
    (apply + state-at-day)))            ;

(println "Part 1: " (count-of-fish-at-day real-data 80))
(println "Part 2: " (count-of-fish-at-day real-data 256))

