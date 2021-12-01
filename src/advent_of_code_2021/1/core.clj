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

(prn "Part 1: " (count-increasing 0 data))


(defn count-increasing-2 [cnt [_ & rst :as ms]]
  (let [win-1 (take 3 ms)
        win-2 (take 3 rst)]
    (cond
      (not= 3 (count win-1) (count win-2))  cnt
      (< (apply + win-1) (apply + win-2))   (recur (inc cnt) rst)
      :else                                 (recur cnt rst))))

(prn "Part 2: " (count-increasing-2 0 data))

