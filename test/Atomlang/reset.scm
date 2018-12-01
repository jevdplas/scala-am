; Source: https://www.tutorialspoint.com/clojure/clojure_atoms_reset.htm (2018-10-21)
; Original code:
; (ns clojure.examples.example
;    (:gen-class))
; (defn example []
;    (def myatom (atom 1))
;    (println @myatom)
;   
;    (reset! myatom 2)
;    (println @myatom))
; (example)

(define myatom (atom 1))
(define val1 (read myatom))
(reset! myatom 2)
(define val2 (read myatom))
(and (eq? val1 1)
     (eq? val2 2))