(ns advent-of-code-2021.3.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


(defn- str-to-ints [s]
  (map (fn [si] (Integer/parseInt (str si))) s))


(defn- invert [i]
  (if (= i 0)
    1
    0))


(def data (->> "3/input"
               (utils/slurp-lines)
               (map str-to-ints)))


(def test-data [[0 0 1 0 0] 
                [1 1 1 1 0] 
                [1 0 1 1 0] 
                [1 0 1 1 1] 
                [1 0 1 0 1] 
                [0 1 1 1 1] 
                [0 0 1 1 1] 
                [1 1 1 0 0] 
                [1 0 0 0 0] 
                [1 1 0 0 1] 
                [0 0 0 1 0] 
                [0 1 0 1 0]])


(defn decode-binary [binary-seq]
  (apply + (map * (reverse binary-seq)
                  (iterate (partial *' 2) 1))))

;; Part 1

(defn compute-1 [data]
  (let [cnt               (count data)
        sums              (apply map + data)
        most-common-bits  (map (fn [sum] (if (>= sum (/ cnt 2)) 1 0)) sums)
        least-common-bits (map invert most-common-bits)]
    (* (decode-binary most-common-bits)
       (decode-binary least-common-bits))))


(println "Part 1: " (compute-1 data))


;; Part 2
(defn compute-2 [pos data comp-fn]
  (let [grps     (group-by #(nth % pos) data)
        grp-0    (get grps 0)
        grp-1    (get grps 1)
        selected (if (comp-fn (count grp-0) (count grp-1))
                   grp-0
                   grp-1)]
    (if (= (count selected) 1)
      (decode-binary (first selected))
      (recur (inc pos) selected comp-fn))))


(println "Part 2: " (* (compute-2 0 data >)
                       (compute-2 0 data <=)))
