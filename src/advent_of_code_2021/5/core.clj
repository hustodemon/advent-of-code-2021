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
  (if (or (= x1 x2) (= y1 y2)) ;; only consider lines with at least one common coord
    (for [x (numbers-between x1 x2)
          y (numbers-between y1 y2)]
      [x y])
    []))


(defn compute-1 [data]
  (->> data
       (mapcat compute-line-points)
       (frequencies)
       (vals)
       (filter #(>= % 2))
       (count)))


(println "Part 1: " (compute-1 data))


(defn- is-diagonal? [[[x1 y1] [x2 y2] :as line-coord]]
  (= (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))


(defn- compute-line-points-2 [[[x1 y1] [x2 y2] :as line-coord]]
  [x1 y1 x2 y2]
  (cond
    ;; horizontal/vertical
    (or (= x1 x2) (= y1 y2))  (for [x (numbers-between x1 x2)
                                    y (numbers-between y1 y2)]
                                [x y])
    ;; diagonal
    (is-diagonal? line-coord) (for [x (numbers-between x1 x2)
                                    y (numbers-between y1 y2)
                                    :when (is-diagonal? [[x y] [x1 y1]])]
                                [x y])
    :else                     []))


(defn compute-2 [data]
  (->> data
       (mapcat compute-line-points-2)
       (frequencies)
       (vals)
       (filter #(>= % 2))
       (count)))

(println "Part 2: " (time (compute-2 data)))
