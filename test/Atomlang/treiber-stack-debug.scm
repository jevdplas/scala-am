; Treiber stack as an example of a lock-free data structure using atoms.

; NOTE: ARTIFICIAL ORDERING IMPOSED ON OUTER LOOP ATOMANALYSIS

(define (loop stk f) (loop stk (f stk) f))

(future (loop (atom '()) (lambda (s) (compare-and-set! (atom '()) #t #t))))
(future (loop (atom '()) (lambda (s) #t)))

