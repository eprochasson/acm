(ns clojure-implementation.core
  (:gen-class))

(defn buyable-machines
  "Extract the list of machine available for a given day, current inventory and balance"
  [day balance inventory machines]
  (filter
    #(and
       (= day (:day %))
       (let [total-balance  (+ balance (:sell inventory 0))]
         (>= total-balance (:cost %))
         ))
    machines)
  )

(defn available-machines
  "Return machines that are still buyable in the future, depending on today's day"
  [day machines]
  (filter #(> (:day %) day)
    machines)
  )

(defn solve
  "Solve a problem"
  ([days balance machines] ; Entry point
    (solve days 1 balance nil machines))

  ([days day balance inventory machines]
    (if (> day days)
      (+ balance (:sell inventory 0)) ; Last day: cash potential inventory, return the total balance
      (if (= available-machines [])
        (+ balance (* (+ (- days day) 1) (:profit inventory 0) (:sell inventory 0))) ; Not last day, but no machines left: compute remaining profit + sell price

        (let [buy-branches (buyable-machines day balance inventory machines) ;; The branches where we buy a new machine and sell the inventory
              not-buy-branch-balance (solve ; the branch were we don't (and we can get its balance directly)
                                       days
                                       (inc day)
                                       (+ balance (:sell inventory 0))
                                       inventory
                                       (available-machines day machines))]
          (apply max (conj
                       (map
                         #(solve
                            days
                            (inc day) ; the next day
                            (+ (- balance (:cost %) (:sell inventory 0))) ; the new balance
                            % ; The machine we bought
                            (available-machines day machines) ; the list of remaining machines
                            )
                         buy-branches)
                       not-buy-branch-balance
                       )
            )
          )
        )
      )
    )
  )

(defn -main
  "Load a problem and solve it"
  [& args]
  (println (str "Case 1: " (solve 20 10 [
                 {:day 6 :cost 12 :profit 1 :sell 3}
                 {:day 1 :cost 9 :profit 1 :sell 2}
                 {:day 3 :cost 2 :profit 1 :sell 2}
                 {:day 8 :cost 20 :profit 5 :sell 4}
                 {:day 4 :cost 11 :profit 7 :sell 4}
                 {:day 2 :cost 10 :profit 9 :sell 1}]))))