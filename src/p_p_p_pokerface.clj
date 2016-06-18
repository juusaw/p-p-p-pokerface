(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r)
    )
  ))

(defn suit [card]
  (let [[_ s] card]
    (str s)
  ))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-replaced (sort (replace {14 1} ranks))]
    (or
      (= (range (first ranks) (+ 1 (last ranks))) ranks)
      (= (range (first ranks-replaced) (+ 1 (last ranks-replaced))) ranks-replaced))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
  ))
