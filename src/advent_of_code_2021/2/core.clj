(ns advent-of-code-2021.2.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


;; multi-pass, but yolo
(def data (->> "2/input"
               (utils/slurp-lines)
               (map (comp
                     (fn [[dir-str quant-str]] [(keyword dir-str) (Integer/parseInt quant-str)])
                     (fn [s] (string/split s #"\s"))))))

(def test-data
  [[:forward 5]
   [:down 5]
   [:forward 8]
   [:up 3]
   [:down 8]
   [:forward 2]])


(defn compute-next [[cur-pos cur-depth :as current] [dir quant :as instructions]]
  (case dir
    :forward [(+ cur-pos quant) cur-depth]
    :down    [cur-pos (+ cur-depth quant)]
    :up      [cur-pos (- cur-depth quant)]))


(defn compute-pos [[cur-pos cur-depth :as current] [[dir quant :as instructions] & rst]]
  [quant dir]
  (if (empty? instructions)
    (* cur-pos cur-depth)
    (recur (compute-next current instructions) rst)))


(println "Part 1: " (compute-pos [0 0] data))


(defn compute-next-2 [[cur-pos cur-aim cur-depth :as current] [dir quant :as instructions]]
  (case dir
    :forward [(+ cur-pos quant) cur-aim (+ cur-depth (* cur-aim quant))]
    :down    [cur-pos (+ cur-aim quant) cur-depth]
    :up      [cur-pos (- cur-aim quant) cur-depth]))


(defn compute-pos-2 [[cur-pos cur-aim cur-depth :as current] [[dir quant :as instructions] & rst]]
  [quant dir]
  (if (empty? instructions)
    (* cur-pos cur-depth)
    (recur (compute-next-2 current instructions) rst)))


(println "Part 2: " (compute-pos-2 [0 0 0] data))
