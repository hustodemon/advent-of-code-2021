(ns advent-of-code-2021.15.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.data.priority-map :refer [priority-map]]))


(def test-data
  [[1 1 6 3 7 5 1 7 4 2]
   [1 3 8 1 3 7 3 6 7 2]
   [2 1 3 6 5 1 1 3 2 8]
   [3 6 9 4 9 3 1 5 6 9]
   [7 4 6 3 4 1 7 1 1 1]
   [1 3 1 9 1 2 8 1 3 7]
   [1 3 5 9 9 1 2 4 2 1]
   [3 1 2 5 4 2 1 6 3 9]
   [1 2 9 3 1 3 8 5 2 1]
   [2 3 1 1 9 4 4 5 8 1]])


(def data
  (->> "15/input"
       (utils/slurp-lines)
       (apply map vector)
       (mapv (fn [line] (mapv (comp read-string str) line)))))


(defn- dist-to-pt [data pt] ;; y x
  (get-in data pt ##Inf))


(defn- neigh-locations [[y x]]
  [[(inc y) x]
   [(dec y) x]
   [y (inc x)]
   [y (dec x)]])


(defn- init-distances [data]
  (for [x    (range (count (first data)))
        y    (range (count data))
        :let [dist (if (= 0 x y) 0 ##Inf)]]
    [[x y] dist]))


(defn- dijkstra [data]
  (loop [distances     (into (priority-map) (init-distances data)) ;; sorted by distances from [0 0]
         processed-pts #{}]
    (if (= (count distances) (count processed-pts))
      distances
      (let [[fst-pt dst-to-pt] (first (remove #(get processed-pts (first %)) distances)) 
            neighbors          (remove processed-pts (neigh-locations fst-pt))
            update-neighbors   (fn [distances neighbor]
                                 (let [dist (+ dst-to-pt (dist-to-pt data neighbor))]
                                   (if (< dist (get distances neighbor ##Inf))
                                     (assoc distances neighbor dist)
                                     distances)))]
        (recur
         (reduce update-neighbors distances neighbors)
         (conj processed-pts fst-pt))))))


(defn compute [data]
  (let [size-x         (count (first data))
        size-y         (count data)
        distances      (dijkstra data)]
    (get distances [(dec size-y) (dec size-x)])))


(time
 (println "Part 1:" (compute data)))
