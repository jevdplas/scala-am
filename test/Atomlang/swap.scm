; Source: https://www.tutorialspoint.com/clojure/clojure_atoms_swap.htm (2018-10-21)
; Original code:
; (ns clojure.examples.example
;    (:gen-class))
; (defn example []
;    (def myatom (atom 1))
;    (println @myatom)
;   
;    (swap! myatom inc)
;    (println @myatom))
; (example)

(define myatom (atom 1))
(define val1 (deref myatom))
(define (inc v)(+ v 1))
(swap! myatom inc)
(= (deref myatom) 2)