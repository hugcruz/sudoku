(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [x _]]
  (set (get board x)))

(defn col-values [board [_ y]]
  (set (map #(nth % y) board)))

(defn coord-pairs [coords]
  (apply concat (map (fn [x] (vec (map (fn [y] [x y]) coords))) coords)))

(defn get-corner [[x y]]
  (let [
         n (int (/ x 3))
         m (int (/ y 3))
       ]
    [(* n 3) (* m 3)]
))

(defn block-values [board coord]
  (let [
         [cx cy] (get-corner coord)
         xs      (range cx (+ cx 3))
         ys      (range cy (+ cy 3))
       ]
    (into #{}
          (for [ x xs
                 y ys ]
            (value-at board [x y])
))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union
        (row-values board coord)
        (col-values board coord)
        (block-values board coord)))))

(defn board-set [board]
  (reduce #(set/union %1 (row-values board [%2 0])) #{} (range 0 9))
)

(defn filled? [board]
  (not (contains? (board-set board) 0)))

(defn rows [board]
  (map #(row-values board [%1 0]) (range 0 9)))

(defn valid-rows? [board]
  (every? #(= all-values %1) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %1]) (range 0 9)))

(defn valid-cols? [board]
  (every? #(= all-values %1) (cols board)))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)
       ]
    (block-values board [x y]
)))

(defn valid-blocks? [board]
  (every? #(= all-values %1) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [
         cnt 0
       ]
    (cond
      (= cnt 81) nil
      (not (has-value? board [(int(/ cnt 9)) (mod cnt 9)])) [(int(/ cnt 9)) (mod cnt 9)]
      :else (recur (inc cnt))
)))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [
           location (find-empty-point board)
         ]
      (for [
             option (valid-values-for board location)
             solution (solve (set-value-at board location option))
           ]
        solution
        ))))
