; Example modified from: https://winterbe.com/posts/2015/05/22/java8-concurrency-tutorial-atomic-concurrent-map-examples/
(define N (random 100))
(define (inc i) (+ i 1))

(define (for-each fun lst)
 (if (null? lst)
     #t
     (begin (fun (car lst))
            (for-each fun (cdr lst)))))

(define atomicInt (atom 0))
(define (iter i)
    (if (< i N)
        (cons (future (swap! atomicInt inc) #t)
              (iter (+ i 1)))
        '()))
(define futures (iter 0))
(for-each (lambda (fut) (deref fut) #t) futures)
(= (read atomicInt) N)
