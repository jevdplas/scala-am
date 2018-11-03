; (def myatom (atom 0))
; (defn f [v]
;     (loop [i v]
;         (if (> i 0)
;             (do (swap! myatom inc)
;                 (recur (- i 1))))))
; (def f1 (future (f 80)))
; (def f2 (future (f 70)))
; @f1 @f2
; (print @myatom)

(define myatom (atom 0))
(define (inc i)(+ i 1))
(define (f i)
  (if (> i 0)
      (begin (swap! myatom inc)
             (f (- i 1)))))
(define f1 (future (f 80)))
(define f2 (future (f 70)))
(deref f1)
(deref f2)
(= (deref myatom) 150)
