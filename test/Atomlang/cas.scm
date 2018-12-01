; Source: https://www.tutorialspoint.com/clojure/clojure_atoms_compare_and_set.htm (2018-10-21)
; Original code:
; (ns clojure.examples.example
;    (:gen-class))
; (defn example []
;    (def myatom (atom 1))
;    (println @myatom)
;    
;    (compare-and-set! myatom 0 3)
;    (println @myatom)
;   
;    (compare-and-set! myatom 1 3)
;    (println @myatom))
; (example)

(define myatom (atom 1))
(compare-and-set! myatom 0 3)
(compare-and-set! myatom 1 2)
(eq? (read myatom) 2)