(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn take-href [node]
  "Takes link wrapper, those node with class 'r'.
   Finds there a link and returns its href attribute."
  (:href ((node 2) 1)))

(defn class-r-node? [node]
  "Checks if passed node is the right one, i. e. has class 'r'"
  (and (vector? node)
       (= "r" (:class (node 1)))))

(declare find-in-children)

(defn link-wrappers [node]
  "Gathers all nodes with class 'r' in one vector."
  (if (vector? node)
    (let [content (children node)
          found (filter class-r-node? content)]
      (if (empty? found)
        (reduce find-in-children [] content)
        found))))

(defn find-in-children [acc node]
  "Accumulates all nodes with class 'r' into acc.
   To accomplish this it recursively calls link-wrappers."
  (let [found (link-wrappers node)]
    (if (empty? found) acc (concat acc found))))

(defn get-links []
  (let [data (parse "clojure_google.html")]
    (vec (map take-href (link-wrappers data)))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))
