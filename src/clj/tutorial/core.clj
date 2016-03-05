(ns tutorial.core
    (:require [clojure.string :as s])
    (:require [clojure.java.io :as j]))

;assignment 1

(defn reverse-string [coll] (apply str (reduce conj () coll)))

(defn split-words [coll] (s/split coll #"\s+")) 

(defn count-letters [c coll] (count (filter #{c} (seq coll)))) 

(defn map-coll [coll] 
  (apply merge 
         (for [c coll] 
              (hash-map c (count-letters c coll)))))

(defn max-map [f coll] (apply max-key val (f coll)))


;assignment 2

(defn alphanumeric? [c] (contains? (apply hash-set (map char (concat (range 48 58) (range 65 91) (range 97 123) [45 47 39]))) c))

(defn lose-punctuation [coll] (apply str (filter alphanumeric? coll)))

(def WPcoll 
  (with-open [rdr (j/reader "/Users/mallory/Downloads/WarandPeace.txt")]
    ;(filter #(not (s/blank? (apply str %))) 
            (map lose-punctuation (apply concat  (map split-words (doall (line-seq rdr)))))))

(defn split-book-n [n coll] (map-coll (take n coll)))

(defn better-split-book [coll]
  (loop [remaining-words coll final-map {}]
    (if (empty? remaining-words)
      final-map
      (let [word (first remaining-words)
            remaining (rest remaining-words)]
        (recur remaining
               (if (contains? final-map word) 
                   (update-in final-map [word] inc)
                   (assoc final-map word 1)))))))

(defn better-map-pattern [n coll]
  (sort-by val (better-split-book (partition n 1 coll)))) 
