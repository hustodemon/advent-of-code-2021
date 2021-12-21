(ns advent-of-code-2021.21.core
  (:require
   [clojure.set :refer [rename-keys]]))


(def data
  {:player-1 {:pos 1, :score 0}
   :player-2 {:pos 5, :score 0}})


(def test-data
  {:player-1 {:pos 4, :score 0}
   :player-2 {:pos 8, :score 0}})


(defn- roll-die [die]
  (-> die
      (update :roll-count inc)
      (update :val #(inc (mod % 100)))))


(defn- move-player [player move]
  (let [new-pos (inc (mod (+ (:pos player) (dec move)) 10))]
    (-> player
        (assoc :pos new-pos)
        (update :score + new-pos))))


(defn- round
  "Play a round (throw 3 times) and return update die and player."
  [{:keys [player die] :as game-state}]
  (let [rolls       (take 3 (rest (iterate roll-die die)))
        updated-die (last rolls)
        roll-score  (apply + (map :val rolls))]
    (merge
     game-state
     {:die    updated-die
      :rolls  (map :val rolls)
      :player (move-player player roll-score)})))


(defn- game
  [{:keys [next-turn] :as game-state}]
  (-> game-state
      (rename-keys {next-turn :player})
      (round)
      (rename-keys {:player next-turn})
      (update :next-turn #(if (= % :player-1) :player-2 :player-1))))


(def init-state
  {:die       {:val        0
               :roll-count 0}
   :next-turn :player-1})


(defn- compute-score [state]
  (* (get-in state [:die :roll-count])
     (get-in state [(get state :next-turn) :score])))


(defn compute [data]
  (->> (merge data init-state)
       (iterate game)
       (filter (fn [{:keys [player-1 player-2]}]
                 (or (>= (:score player-1) 1000)
                     (>= (:score player-2) 1000))))
       (first)
       (compute-score)))


(println "Part 1: " (compute data))


