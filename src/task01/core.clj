(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn take-href [node]
  "Закладывается на определенную структуру ноды.
  Принимает ту самую обертку с классом r"
  (:href ((node 2) 1)))

(defn class-r-node? [node]
  "Проверяет является ли данная нода нужной нам оберткой с классом r"
  (and (vector? node)
       (= "r" (:class (node 1)))))

(declare find-in-children)

(defn link-wrappers [node]
  "Никакой хвостовой рекурсии не получилось. При огромном дереве fail"
  (if (vector? node)
    (let [content (children node)
          found (filter class-r-node? content)]
      (if (empty? found)
        (reduce find-in-children [] content)
        found))))

(defn find-in-children [acc node]
  "Взаимно рекурсивная с link-wrappers функция.
  Нужна для свертки детей очередной ноды"
  (let [found (link-wrappers node)]
    (if (empty? found) acc (concat acc found))))

(defn get-links []
  (let [data (parse "clojure_google.html")]
    (vec (map take-href (link-wrappers data)))))

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))