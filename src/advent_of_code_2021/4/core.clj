(ns advent-of-code-2021.4.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


(def data
  (-> "4/input"
      utils/slurp-res
      (string/split #"\n\n")))


(defn- parse-draws [s]
  (map read-string (string/split s #",")))


(defn- parse-grid
  "Gives us back rows and colums in a single set."
  [s]
  (let [rows-raw (string/split-lines s)
        rows-raw (map string/trim rows-raw)
        rows-str (map #(string/split % #"\s+") rows-raw)
        rows     (map (fn [r] (into #{} ( map read-string r))) rows-str)
        cols     (apply map hash-set rows)
        rows-set (into #{} rows)]
    (into rows-set cols)))


(defn- remove-num-from-grid [grid n]
  (into #{}
        (map (fn [row-or-col] (disj row-or-col n)) grid)))


(defn compute-1 [[fst-draw & rest-draws] grids]
  (let [res-grids  (map (fn [g] (remove-num-from-grid g fst-draw)) grids)
        bingo-grid (first (filter (fn [grid] (some? (get grid #{}))) res-grids))]
    (if bingo-grid
      ;; compute result, divide the sum of grids by 2 (we have both rows and cols there, which is lame from this POV)
      ;; next time, i wouldn't duplicate the columns, would only compute them on-the-fly (and sacrifice the performance)
      (* fst-draw
         (/ (reduce + (apply concat bingo-grid)) 2))
      (recur rest-draws res-grids))))


(println "Part 1: " (compute-1 (parse-draws (first data)) (map parse-grid (rest data))))


;; Part 2
;; doesn't work.
(defn- maps [m f]
  (into #{} (map m f)))

(defn- remove-num-from-grid-2 [grid n]
  (-> grid
                (update :rows #(maps (fn [row-or-col] (disj row-or-col n)) %))
                (update :cols #(maps (fn [row-or-col] (disj row-or-col n)) %))))


(defn- parse-grid-2
  "Gives us back rows and colums in a single set."
  [s]
  (let [rows-raw (string/split-lines s)
        rows-raw (map string/trim rows-raw)
        rows-str (map #(string/split % #"\s+") rows-raw)
        rows     (maps (fn [r] (into #{} (map read-string r))) rows-str)
        cols     (apply map hash-set rows)]
    {:rows rows
     :cols (into #{} cols)}))

(apply concat
 (vals
  (remove-num-from-grid-2
   (parse-grid-2 (second data))
   11)))


(defn compute-2 [[fst-draw & rest-draws] grids]
  (let [res-grids  (maps (fn [g] (remove-num-from-grid-2 g fst-draw)) grids)
        non-bingos (remove (fn [grid] (or (some? (get (:rows grid) #{}))(some? (get (:cols grid) #{})))) res-grids)]
    ;;(doall (map println res-grids))
    (if (= 1 (count non-bingos))
      (* (first   rest-draws)
         (reduce +
                 (apply concat (:rows (remove-num-from-grid-2 (first non-bingos) (first rest-draws))))))
      ;;non-bingo-grid
      (recur rest-draws res-grids))))

;; 9657 too low
;; 9802 too low
(compute-2 (parse-draws (first data)) (map parse-grid-2 (rest data)))
