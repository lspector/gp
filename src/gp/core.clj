(ns gp.core
  (:gen-class))

(defn -main
  "Loads and runs the code in the indicated namespace. This will be called when 'lein run' is run
  on the command line, and there should be a namespace following 'run'. For example, to run
  the code in the gp.even3parity_lexicase namespace, execute 'lein run gp.even3parity_lexicase'."
  [& args]
  (require (symbol (first args))))
