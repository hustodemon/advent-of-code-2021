(ns advent-of-code-2021.14.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.string :as string]))


(defn- parse-rule
  "Parse rule into lhs and rhs pair.
  lhs is a seq of characters, rhs is a character.
  E.g. [(\\c \\h) \b]."
  [rule-str]
  (let [[lhs rhs] (string/split rule-str #"\s*->\s*")]
    [(seq lhs) (first rhs)]))


(defn- parse-data [f]
  (let [string                   (utils/slurp-res f)
        [template-str rules-str] (string/split string #"\n\n")
        rules-pairs              (map parse-rule (string/split-lines rules-str))]
    [template-str
     (into {} rules-pairs)]))


;; Part 1: brute force
(defn- expand-template [template rules]
  (str (first template) ;; prepending the 1st char is ugly as hell!
       (loop [[fst snd & _ :as all] template
              expanded         ""]
         (if-not snd
           expanded
           (recur (rest all) (str expanded
                                  (get rules [fst snd])
                                  snd))))))


(defn compute [template rules]
  (iterate #(expand-template % rules) template))


(let [[template rules] (parse-data "14/input")
      res              (nth (compute template rules) 10)
      freqs            (frequencies res)
      sorted           (sort (vals freqs))]
  (println "Part 1: " (- (last sorted)
                         (first sorted))))


;; Part 2: optimized
;; The model in the computation is not the polymer itself, but the frequencies
;; of the pairs encoded in a map: {"AA" 3 "BB" 1} means a polymer that has 3
;; instances of "AA" and 2 instances of "BB" in it.
(defn- expand-template-2
  "Expand template, e.g. ABBB into AB BB BB and give back the frequency map, e.g. {AB 1, BB 2}."
  [[fst & rest-template]]
  (if-not rest-template
    {}
    (merge-with + {(str fst (first rest-template)) 1} (expand-template-2 rest-template))))


(defn expand-rules
  "Expand rules AB -> C into AB -> [AC, CB] and merge them into a single map."
  [rules]
  (into
   {}
   (map (fn [[[f s :as lhs] rhs]]
          [(apply str lhs)
           [(str f rhs) (str rhs s)]])
        rules)))


(defn- expand-frequency [rule freq]
  (let [[f s] rule]
    (merge-with +
                {f freq}
                {s freq})))


(defn expand
  "Expand the pair frequencies according to rules, e.g. {AA 1, BB 2} becomes {AB
  1, BC 3, CD 2}, if the rules are {AA [AB BC], BB [BC CD]}."
  [pair-frequencies rules]
  (apply merge-with +
         (map (fn [[pair frequency]] (expand-frequency (get rules pair) frequency))
              pair-frequencies)))


(defn compute-counts
  "Given the frequency map, compute occurences of the individual characters."
  [freqs]
  (apply merge-with +
         (mapcat (fn [[[f s :as pair] cnt]] [{f cnt} {s cnt}]) freqs)))


(let [[template rules]  (parse-data "14/input")
      expanded-template (expand-template-2 template)
      expanded-rules    (expand-rules rules)
      solutions         (iterate #(expand % expanded-rules) expanded-template)
      counts            (vals (compute-counts (nth solutions 40)))
      sorted-counts     (sort counts)
      ;; if we wanted to be extra pedantic, we would need to have a special
      ;; handling for the first and last pair, but luckily the rounding
      ;; by division makes this insignificant
      fst               (quot (inc (first sorted-counts)) 2)
      lst               (quot (inc (last sorted-counts)) 2)]
  (println "Part 2: " (- lst fst)))

