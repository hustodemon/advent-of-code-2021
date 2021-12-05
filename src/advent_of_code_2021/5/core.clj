(ns advent-of-code-2021.5.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


(declare parse-lines)


(def data
  (->> "5/input"
       utils/slurp-lines
       parse-lines))


(defn- parse-lines [lines-as-str]
  (let [parse-coord (fn [s] (mapv read-string (string/split s #",")))
        parse-line  (fn [s] (mapv parse-coord (string/split s #"\s*->\s*")))]
    (map parse-line lines-as-str)))


(defn- numbers-between
  "Numbers x1 and x2 (both inclusive)."
  [x1 x2]
  (let [[smaller bigger] (sort [x1 x2])]
    (range smaller (inc bigger))))


(defn- compute-line-points [[[x1 y1] [x2 y2] :as line-coord]]
  [x1 y1 x2 y2]
  (if (or (= x1 x2) (= y1 y2)) ;; horizontal or vertical
    (for [x (numbers-between x1 x2)
          y (numbers-between y1 y2)]
      [x y])
    []))


(defn- compute
  "Compute the count of overlapping points.
  Takes lines and a line-points-fn (a function generating points
  corresponding to a line between 2 points)."
  [lines line-points-fn]
  (->> lines
       (mapcat line-points-fn)
       (frequencies)
       (vals)
       (filter #(>= % 2))
       (count)))

(defn compute-1 [data] (compute data compute-line-points))


(println "Part 1: " (compute-1 data))


;; Part 2
(defn- walk-closer
  "Given 2 points (src and dst), compute the next point on the path from src to dst."
  [[[x1 y1 :as src] [x2 y2 :as dst] :as line-coord]]
  [(cond
     (< x1 x2) (inc x1)
     (> x1 x2) (dec x1)
     :else     x1) ;; egal
   (cond
     (< y1 y2) (inc y1)
     (> y1 y2) (dec y1)
     :else     y1)])


;; let's try something different here and implement it recursively
(defn- compute-line-points-2 [[[x1 y1 :as src] [x2 y2 :as dst] :as line-coord]]
  (if (= src dst) [src]
      (conj (compute-line-points-2 [(walk-closer line-coord) dst]) src)))


(defn compute-2 [data] (compute data compute-line-points-2))


(println "Part 2: " (time (compute-2 data)))
