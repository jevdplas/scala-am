Example modified from: https://winterbe.com/posts/2015/05/22/java8-concurrency-tutorial-atomic-concurrent-map-examples/

(define atomicInt (atom 0))
(define (iter i)
    (if (< i 1000)
        (cons (future (swap! atomicInt inc) #t)
              (iter (+ i 1)))))
(define futures (iter 0))
(for-each (lambda (future) (deref future) #t) futures)
(= (read atomicInc) 1000)