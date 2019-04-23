(define (do-n n f)
 (letrec ((loop (lambda (i acc)
                 (if (= i n)
                  acc
                  (loop (+ i 1) (cons (f i) acc))))))
   (loop 0 '())))

(define (inc i)(+ i 1))

(do-n 10 inc)