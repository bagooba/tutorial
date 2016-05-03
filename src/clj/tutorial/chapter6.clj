(ns tutorial.chapter6
     (:require [clojure.string :as s])
       (:refer-clojure :exclude [min max]))

(defn comparator-over-maps [comparison-fn ks]
  (fn [maps] 
      (zipmap ks 
              (map (fn [k] (apply comparison-fn (map k maps)))
                   ks))))
(def min (comparator-over-maps clojure.core/min [:lat :lng]))
(def max (comparator-over-maps clojure.core/max [:lat :lng]))

(defn translate-to-00
  [locations]
   (let [mincoords (min locations)]
     (map #(merge-with - % mincoords) locations)))
(defn scale
  [width height locations]
  (let [maxcoords (max locations)
        ratio {:lat (/ height (:lat maxcoords))
               :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locations)))
