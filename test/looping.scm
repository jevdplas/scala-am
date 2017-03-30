;; Test different looping constructs (named let & do).
;; Source: R5RS specification document.

(and (equal? (let loop ((numbers '(3 -2 1 6 -5))
                        (nonneg '())
                        (neg '()))
               (cond ((null? numbers) (list nonneg neg))
                     ((>= (car numbers) 0)
                      (loop (cdr numbers)
                            (cons (car numbers) nonneg)
                            neg))
                     ((< (car numbers) 0)
                      (loop (cdr numbers)
                            nonneg
                            (cons (car numbers) neg)))))
             '((6 1 3) (-5 -2)))

     (equal? (do ((vec (make-vector 5))
                  (i 0 (+ i 1)))
               ((= i 5) vec)
               (vector-set! vec i i))
             (vector 0 1 2 3 4))

     (= (let ((x '(1 3 5 7 9)))
          (do ((x x (cdr x))
               (sum 0 (+ sum (car x))))
            ((null? x) sum))) 25))