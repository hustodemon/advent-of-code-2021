(ns advent-of-code-2021.11.core
  (:require
   [advent-of-code-2021.utils :as utils]))


(defn- parse [data]
  (->> data
       utils/slurp-lines
       (apply map vector)
       (mapv (fn [line] (mapv (comp read-string str) line)))))


(def test-data (parse "11/test-input"))
(def data (parse "11/input"))


(defn- neighbor-locations [grid y x]
  (let [size-x (count (first grid))
        size-y (count grid)]
    (for [nx    (range (dec x) (+ x 2))
          ny    (range (dec y) (+ y 2))
          :when (and (not (and (= nx x) (= ny y)))
                     (>= ny 0)
                     (<= ny (dec size-y))     
                     (>= nx 0)
                     (<= nx (dec size-x)))]
      [ny nx])))


(defn- bounded-inc [n]
  (when (num n)
    (if (< n 9)
      (inc n)
      0)))


(defn- step [grid]
  (mapv #(mapv bounded-inc %) grid))


(defn- flash [grid flashing-coords nr-flashes]
  (loop [[[y x :as fst] & queue] flashing-coords
         visited                 #{}
         grid                    grid
         nr-flashes              nr-flashes]
    (if-not fst
      [grid nr-flashes]
      (let [neighbors  (remove visited (neighbor-locations grid y x))
            updated    (update-in grid [y x] bounded-inc)
            updated    (if (visited fst) grid updated)
            nr-flashes (if (= 0 (get-in updated [y x])) (inc nr-flashes) nr-flashes)] ;; todo tidy
        (recur (into [] (concat neighbors queue)) (conj visited fst) updated nr-flashes)))))


(defn phase [grid] ;; yuck
  (let [size-x                   (count (first grid))
        size-y                   (count grid)
        after-step               (step grid)
        flashing-coords          (for [y     (range size-y)
                                       x     (range size-x)
                                       :when (= 0 (get-in after-step [y x]))]
                          [y x])
        [after-flash nr-flashes] (flash after-step flashing-coords 0)
        ]
    after-flash
    ))

(clojure.pprint/pprint
 (apply map vector
        (phase
        (phase test-data)
         )
        ))


