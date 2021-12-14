(ns advent-of-code-2021.12.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.set :as cset]
   [clojure.string :as string]))


;; copypasta from clojuredocs
(defn- invert-map-of-sets [m]
   (reduce (fn [a [k v]] (assoc a k (conj (get a k #{}) v))) {} (for [[k s] m v s] [v k])))


(defn- conj-set ;; not nice
  ([e]     #{e})
  ([s & e] (apply conj (into #{} s) e)))


(defn- parse-data [res]
  (let [as-map (->> (utils/slurp-lines res)
                    (map (fn [line] (string/split line #"\s*-\s*")))
                    (reduce (fn [acc [from to]] (update acc from conj-set to)) {}))
        rev-map (invert-map-of-sets as-map)]
    (merge-with cset/union as-map rev-map)))


(def test-data (parse-data "12/test-input"))
(def test-data-2 (parse-data "12/test-input-2"))
(def data (parse-data "12/input"))


(defn is-uppercase? [s]
  (re-matches #"[A-Z]+" s))


(defn- compute-next-nodes
  "We discard lowercase nodes that have been already visited"
  [g cur-path cur-node]
  (let [next-nodes             (get g cur-node)
        cur-path-set-lowercase (into #{} (remove is-uppercase? cur-path))
        allowed                (remove cur-path-set-lowercase next-nodes)]
    allowed))


(defn visit [g cur-path cur-node]
  (let [next-nodes (compute-next-nodes g cur-path cur-node)]
    ;;(prn "current node " cur-node " current path " cur-path " curent targets  " (get g cur-node) " next targets " next-nodes)
    (cond
      (= cur-node "end") (cons cur-node cur-path)
      (seq next-nodes)   (mapcat #(visit g (cons cur-node cur-path) %) next-nodes))))

(count
 (filter #(= % "start")
         (reverse
          (visit data [] "start"))))


;; Part 2
(defn- compute-next-nodes
  "We discard lowercase nodes that have been already visited"
  [g cur-path cur-node]
  (let [next-nodes                (remove #{"start"} (get g cur-node))
        cur-path-lowercase        (remove is-uppercase? cur-path)
        cur-path-lowercase-set    (into #{} cur-path-lowercase)
        contains-lowercase-dupes? (not= (count cur-path-lowercase) (count cur-path-lowercase-set))
        allowed                   (remove cur-path-lowercase-set next-nodes)]
    (if contains-lowercase-dupes?
      allowed
      next-nodes)))


(compute-next-nodes test-data '("d" "b" "start") "b")

(defn visit [g cur-path cur-node path-acc]
  (let [next-nodes (compute-next-nodes g cur-path cur-node)]
    ;;(prn "current node " cur-node " current path " cur-path " curent targets  " (get g cur-node) " next targets " next-nodes)
    (cond
      (= cur-node "end") (do (comment prn (reverse (cons cur-node cur-path)))
                           (conj path-acc (cons cur-node cur-path)))
      (seq next-nodes)   (mapcat #(visit g (cons cur-node cur-path) % path-acc) next-nodes))))

;; 291318 high
(count
       (visit data [] "start" []))

(count
 (filter #(= % "end")
         (reverse
          (visit test-data [] "start"))))

(count
      (into #{}
            (reverse
             (visit test-data [] "start"))))
