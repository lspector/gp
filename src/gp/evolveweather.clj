;; Lee Spector (lspector@hampshire.edu) 20121001

(ns gp.evolveweather)

;; This is a version of evolvefn.clj that was modified to solve a
;; different problem, of finding an equation for the average weather
;; statistics for Amherst, MA. See evolvefn.clj for more extensive
;; comments on the shared code. The changes made here are:
;;
;;   - different data, from http://www.weather.com/outlook/travel/businesstraveler/wxclimatology/monthly/graph/01002
;;   - added sin and cos functions
;;
;; Note that this will probably always run forever.

(def target-data ;; high temps by month
  [[1.0 33.0]
   [2.0 37.0]
   [3.0 45.0]
   [4.0 58.0]
   [5.0 69.0]
   [6.0 78.0]
   [7.0 82.0]
   [8.0 81.0]
   [9.0 73.0]
   [10.0 62.0]
   [11.0 49.0]
   [12.0 38.0]])

(def function-table (zipmap '(+ - * pd sin cos)
                            '(2 2 2 2  1   1)))

(defn random-function 
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  []
  (rand-nth (list 'x (- (rand 2) 1))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))

(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

(defn sin [n] (Math/sin n))

(defn cos [n] (Math/cos n))

(defn error 
  [individual]
  (let [value-function (eval (list 'fn '[x] individual))]
    (reduce + (map (fn [[x y]] 
                     (Math/abs 
                       (- (value-function x) y)))
                   target-data))))

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn random-subtree 
  [i]
  (if (zero? (rand-int (codesize i)))
    i
    (random-subtree 
      (rand-nth
        (apply concat
               (map #(repeat (codesize %) %)
                    (rest i)))))))

(defn replace-random-subtree
  [i replacement]
  (if (zero? (rand-int (codesize i)))
    replacement
    (let [position-to-change 
          (rand-nth 
            (apply concat
                   (map #(repeat (codesize %1) %2)
                        (rest i)
                        (iterate inc 1))))]
          (map #(if %1 (replace-random-subtree %2 replacement) %2)
               (for [n (iterate inc 0)] (= n position-to-change))
               i))))

(defn mutate
  [i]
  (replace-random-subtree i (random-code 2)))

(defn crossover
  [i j]
  (replace-random-subtree i (random-subtree j)))

(defn sort-by-error
  [population]
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

(defn select
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 2)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best error:" best-error)
      (println "Best program:" best)
      (println "     Median error:" (error (nth population 
                                                (int (/ popsize 2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      (if (< best-error 0.1) ;; good enough to count as success
        (println "Success:" best)
        (recur 
          (inc generation)
          (sort-by-error      
            (concat
              (repeatedly (* 1/2 popsize) #(mutate (select population 7)))
              (repeatedly (* 1/4 popsize) #(crossover (select population 7)
                                                      (select population 7)))
              (repeatedly (* 1/4 popsize) #(select population 7)))))))))

(evolve 1000)

