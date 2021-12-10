(ns advent-of-code-2021.7.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))

(def data (-> "7/input"
              (utils/slurp-res)
              (string/split #",")
              (as-> s (map read-string s))))

(def test-data [16 1 2 0 4 2 7 1 2 14])


(defn- median [c]
  (nth (sort c) (/ (count c) 2)))


(defn compute-1 [data]
  (let [med (median data)]
    (reduce (fn [acc dist] (+ acc (Math/abs (- dist med)))) 0 data)))


(println "Part 1: " (compute-1 data))


;; Part 2
;; ok, Scheiß drauf, it's too late, we're gonna compute it iteratively, the data is not that big
(defn- fuel-burned-between [x y];; please simplify this
  (let [dist (Math/abs (- x y))]
    (/ (* (inc dist) dist) 2)))


;; Brute-force solution
;;(defn- compute-single [pivot v]
;;  (apply + (map #(fuel-burned-between pivot %) v)))
;;
;;
;;(defn compute-2-bf [v]
;;  (let [[mmin mmax] (apply (juxt min max) v)
;;        pos-range   (range mmin (inc mmax))]
;;    (apply min (map #(compute-single % v) pos-range))))


(defn- avg-int-floored [nums]
  (int
   (/ (apply + nums)
      (count nums))))


;; Part 2 Vyjebávač
(defn compute-2 [crab-positions]
  (let [optimal-position     (avg-int-floored crab-positions)
        fuel-needed-per-crab (map (partial fuel-burned-between optimal-position)
                                  crab-positions)]
    (apply + fuel-needed-per-crab)))

(println "Part 2: " (compute-2 data))
