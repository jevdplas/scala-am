(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

;; Producer-consumer problem
(define buffer (atom '()))
(define done (atom #f))
(define (do-something element)
  (display element) (newline))
(define (producer n)
  (if (= n 0)
      (reset! done #t)
      (begin
        (swap! buffer (lambda (curr) (cons n curr)))
        (producer (- n 1)))))
(define (consumer)
  (if (null? (read buffer))
      (if (read done)
          'done
          (consumer))
      (let ((thing '())) ; Also possible with compare-and-set!.
        (swap! buffer
               (lambda (curr)
                 (set! thing (car curr)) ; This side effect is not very bad, since thing is not accessible from the outside.
                 (cdr curr)))
        (do-something thing)
        (consumer))))

(define producer-thrd (future (producer 52)))

(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define consumer-thrds (do-n 42 (lambda () (future (consumer)))))

(deref producer-thrd)
(map (lambda (t) (deref t)) consumer-thrds)
