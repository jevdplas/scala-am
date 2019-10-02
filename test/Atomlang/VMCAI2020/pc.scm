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
(define (consumer) ; Susceptible to ABA problem!
  (let ((buf (read buffer)))
    (cond ((and (null? buf) (read done)) 'done)
      ((null? buf) (consumer))
      (#t
          (compare-and-set! buffer buf (cdr buf))
          (do-something (car buf))
          (consumer)))))
(define producer-thrd (future (producer (random 10))))

(define (do-n n f)
  (if (= n 0)
      '()
      (cons (f) (do-n (- n 1) f))))

(define consumer-thrds (do-n (random 10) (lambda () (future (consumer)))))

(deref producer-thrd)
(map (lambda (t) (deref t)) consumer-thrds)
