;; Lee Spector (lspector@hampshire.edu) 20151107

(ns gp.even3parity_lexicase)

;; This is a version of even3parity.clj that was modified to use lexicase
;; selection rather than tournament selection. The changes made 
;; here are:
;;
;;   - error function returns sequences of errors per case (not summed)
;;   - other functions sum errors as needed
;;   - sort-by-error adds error sequences to programs as metadata
;;   - select uses lexicase selection, using the metadata error sequences
;;
;; From initial testing this appears to solve the problems in fewer generations
;; than tournament selection (with default parameters), although the selection
;; step here takes longer.

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
    (map (fn [[in1 in2 in3 correct_output]] 
           (if (= (value-function in1 in2 in3) correct_output)
             0
             1))
         target-data)))

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
  (vec (map #(with-meta (last %) {:errors (first %)})
            (sort (fn [[errors1 total-error1 prog1] [errors2 total-error2 prog2]] 
                    (< total-error1 total-error2))
                  (map #(let [errs (error %)]
                          (vector errs (reduce + errs) %)) 
                       population)))))

(defn select
  [population]
  (let [n (count (:errors (meta (first population))))]
    (loop [survivors population
           cases (shuffle (range n))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (rand-nth survivors)
        (let [min-err-for-case 
              (apply min (map #(nth % (first cases))
                              (map #(:errors (meta %)) 
                                   survivors)))]
          (recur (filter #(= (nth (:errors (meta %)) 
                                  (first cases)) 
                             min-err-for-case)
                         survivors)
                 (rest cases)))))))

(defn evolve
  [popsize]
  (println "Starting evolution...")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 2)))]
    (let [best (first population)
          best-error (reduce + (error best))]
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
              (repeatedly (* 1/10 popsize) #(mutate (select population)))
              (repeatedly (* 8/10 popsize) #(crossover (select population)
                                                       (select population)))
              (repeatedly (* 1/10 popsize) #(select population)))))))))

(evolve 1000)
