(ns advent-of-code-2021.1.core
  (:require
   [advent-of-code-2021.utils :as utils]))


(def data (->> "1/input"
               (utils/slurp-lines)
               (map (fn [s] (Integer/parseInt s)))))


(defn count-increasing [cnt [fst snd & _ :as all]]
  (cond
    (nil? snd)  cnt
    (< fst snd) (recur (inc cnt) (rest all))
    :else       (recur cnt       (rest all))))


(count-increasing 0 data)
