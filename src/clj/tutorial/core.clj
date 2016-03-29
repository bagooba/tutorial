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

(def wpcoll 
  (with-open [rdr (j/reader "/Users/mallory/Downloads/WarandPeace.txt")]
    ;(filter #(not (s/blank? (apply str %))) 
            (map lose-punctuation (apply concat  (map split-words (doall (line-seq rdr)))))))

(defn split-book-n [n coll] (map-coll (take n coll)))

;assignment 3

(defn better-split-book [coll]
  (loop [remaining-words coll 
         final-map {}]
    (if (empty? remaining-words)
      final-map
      (let [word (first remaining-words)
            remaining (rest remaining-words)]
        (recur remaining
               (if (contains? final-map word) 
                   (update-in final-map [word] inc)
                   (assoc final-map word 1)))))))

(defn map-frequency [n coll]
  (into {} (sort-by val (better-split-book (partition n 1 coll))))) 

(defn find-random-key [n coll] (key (rand-nth (seq (map-frequency n coll)))))

(defn key321 [coll] 
  (let [coll3 (find-random-key 3 coll)]
    (list (str (first coll3)) (str (second coll3)))))

(defn map-key-all [n coll] (map key (map-frequency n coll)))

;assignment 4

(defn find-prob-key [m] 
  (let [items (keys m)
        sum (reduce + (vals m)) 
        target (rand sum)]
    (loop [[part & remaining] items
           accumulation (m part)]
          (let [_ (println (str "count:" (count remaining)))] (if (> accumulation target)
              part
              (recur remaining (+ accumulation (m (first remaining))))) ))))

(defn matcher [word coll]
   (let [_ (println (str "thisword: " (type word ) " coll: " (type coll)))]
     (filter #(= word (first (key %))) coll)))
(defn wordify-end-punct [coll]
  (s/replace (s/replace (s/replace coll #"\." " .") #"!" " !") #"\?" " ?"))

(defn un-wordify-end-punct [coll]
    (s/replace (s/replace (s/replace coll #" \." ".") #" \!" "!") #" \?" "?"))

;assignment 5

(defn single-to-pair [m word]
  (apply dissoc m (remove #(= (first %) word) (keys m))))

(defn pair-to-triple [m words]
  (let [_ (println (str "words: " words))] 
    (apply dissoc m (remove #(and (= (first %) (first words)) (= (second %) (second words))) (keys m)))))

(defn my-pair-to-triple [words m]
  ;note - takes a map of triples
  (filter #(and (= (first %) (first words)) (= (second %) (second words))) m))

(def wpcoll1 
  (with-open [rdr (j/reader "/Users/mallory/Downloads/WarandPeace.txt")]
    ;(filter #(not (s/blank? (apply str %))) 
            (map wordify-end-punct (apply concat (map split-words (doall (line-seq rdr)))))))

(defn key321 [coll] 
  (let [coll3 (find-random-key 3 coll)]
    (list (str (first coll3)) (str (second coll3)))))

(defn key333 [coll]
  (list (str (last (drop-last coll)) (last coll))))

(defn matching3 [coll]
  (let [coll3 (find-prob-key (map-frequency 3 coll))
        key23 (rest coll3) ] (filter #(= (rest coll3) (key321 (key %))) (map-frequency 3 coll))))

(defn total-rewrite [m]
   (loop [coll2 (map-frequency 2 m)
          coll3 (map-frequency 3 m) 
          final-book (vec (find-prob-key (map-frequency 1 m)))] 
         (let [word (last final-book)
               words (key333 final-book)
               _ (println (str "word2: " word))] 
           (cond 
             (= (into {} (matcher word coll2)) (pair-to-triple coll3 words) {})
                final-book   
             (= (rest final-book) ())
                (let [key2 (find-prob-key (into {} (matcher word coll2)))] 
                    (recur (update-in coll2 [key2] dec)
                           coll3
                           (conj final-book (last key2))))
             (= (pair-to-triple coll3 words) {}) 
                (let [key2 (find-prob-key (into {} (matcher word coll2)))] 
                    (recur (update-in coll2 [key2] dec)
                           coll3
                           (conj final-book (last key2))))
             (not (= (pair-to-triple coll3 words) {}))   
                (let [key3 (find-prob-key (pair-to-triple coll3 words))]
                     (recur coll2 
                           (update-in coll3 [key3] dec)
                           (conj final-book (last key3))))))))
