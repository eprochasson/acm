(ns clojure-implementation.core
  (:require [clojure.string :as s])
  (:gen-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File manipulation/parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn parse-line
  "Parse one line of the input file"
  ([day cost sell profit]
    {:day day :cost cost :profit profit :sell sell})
  ([machines days balance]
    (if (= (+ (+ machines days) balance) 0)
      {:eof true}
      {:new-problem true :balance days :days balance})
      )
  )

(defn str->int
  "Convert string to integer"
  [string]
  (Integer. string)
  )

(defn read-problems
  "Read all problems"
  [lines]
  (loop [lines lines
         current-problem {}
         current-machines []
         problems []
         line-counter 0]
    (let [line (apply parse-line (map str->int (s/split (first lines) #" ")))]
      (if (:eof line)
        (conj problems [(:days current-problem) (:balance current-problem) current-machines]) ;; EOF, return all the problems
        (if (:new-problem line)
          ;; New problem. Store the previous one and start again
          (if (= line-counter 0)
            (recur (rest lines) {:days (:days line) :balance (:balance line)} [] problems (inc line-counter))
            (recur (rest lines) {:days (:days line) :balance (:balance line)} [] (conj problems [(:days current-problem) (:balance current-problem) current-machines]) (inc line-counter))
            )
          ;; Not new problem: add the current machine
          (recur (rest lines) current-problem (conj current-machines line) problems (inc line-counter))
          )
        )
      )
    )
  )

(defn parse-input
  "Read the problems from the input file"
  [filename]
  (let [lines (s/split (slurp filename) #"\n")]
    (read-problems lines)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solving the problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (if (= (count machines) 0)
        (+ balance (* (+ (- days day) 1) (:profit inventory 0)) (:sell inventory 0)) ; Not last day, but no machines left: compute remaining profit + sell price
        (let [buy-branches (buyable-machines day balance inventory machines) ;; The branches where we buy a new machine and sell the inventory
              not-buy-branch-balance (solve ; the branch were we don't (and we can get its balance directly)
                                       days
                                       (inc day)
                                       (+ balance (:profit inventory 0))
                                       inventory
                                       (available-machines day machines))]
          (apply max (conj
                       (map
                         #(solve
                            days
                            (inc day) ; the next day
                            (+ (- balance (:cost %)) (:sell inventory 0)) ; the new balance
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
  (let [problems (parse-input "../input.txt")]
    (loop [case 1
           problems problems]

      (println (str "Case " case " " (apply solve (first problems))))
      (if (> (count (rest problems)) 0)
        (recur (inc case) (rest problems))
        )
      )
    )
  )
