(ns p-p-p-pokerface)

(def ranks {\A 14 \K 13 \Q 12 \J 11 \T 10})
(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (ranks r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn n-of-a-kind? [n hand]
  (not (empty? (filter (fn [x] (= x n))
          (vals (frequencies (map rank hand)))))))



(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (= x 2))
          (vals (frequencies (map rank hand)))))))

(defn straight-ignoring-ace? [ranked]
  (let [sranked (sort ranked)
        first-rank (first sranked)
        straight (range first-rank (+ first-rank 5))]
    (= straight sranked)))

(defn straight? [hand]
  (or (straight-ignoring-ace? (map rank hand))
      (straight-ignoring-ace? (replace {14 1} (map rank hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(def checkers #{[high-card? 0]  [pair? 1]
                [two-pairs? 2]  [three-of-a-kind? 3]
                [straight? 4]   [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]
                [straight-flush? 8]})


(defn value [hand]
  (apply max (map second (filter (fn [[pass val]] pass)
          (map (fn [[check val]] [(check hand) val])
               checkers)))))
