;; Lee Spector (lspector@hampshire.edu) 20131115

(ns gp.even3parity)

;; This is a version of evolvefn.clj that was modified to solve a
;; different problem, of determining whether or not number of true
;; inputs out of three boolean inputs is even. See evolvefn.clj 
;; for more extensive comments on the shared code. The changes made 
;; here are:
;;
;;   - different data
;;   - changed functions and terminals (note: 3 inputs)
;;   - different error function
;;   - changed genetic operator probabilities and tournament sizes
;;
;; May not always succeed; it's harder than one might hope for the 
;; standard GP algorithm.

(def target-data ;; each case in the form [in1 in2 in3 correct_output]
  [[false false false true]
   [false false true false]
   [false true false false]
   [false true true true]
   [true false false false]
   [true false true true]
   [true true false true]
   [true true true false]])

(def function-table (zipmap '(and or nand nor not)
                            '(2   2  2    2   1)))

(defn random-function 
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  []
  (rand-nth '[in1 in2 in3]))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))

(defn nand [a b] (not (and a b)))

(defn nor [a b] (not (or a b)))

(defn error 
  [individual]
  (let [value-function (eval (list 'fn '[in1 in2 in3] individual))]
    (reduce + (map (fn [[in1 in2 in3 correct_output]] 
                     (if (= (value-function in1 in2 in3) correct_output)
                       0
                       1))
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
              (repeatedly (* 1/10 popsize) #(mutate (select population 5)))
              (repeatedly (* 8/10 popsize) #(crossover (select population 5)
                                                       (select population 5)))
              (repeatedly (* 1/10 popsize) #(select population 5)))))))))

(evolve 1000)
