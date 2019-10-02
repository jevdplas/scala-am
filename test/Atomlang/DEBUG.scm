(define buffer (atom '()))
(define done (atom #f))
(define (do-something element) (display element) (newline))
(define (producer n)
  (if (= n 0)
    (reset! done #t)
    (begin
      (swap! buffer (lambda (curr) (cons n curr)))
      (producer (- n 1)))))
(define (consumer) ; Susceptible to ABA problem!
  (let ((buf (read buffer)))
    (cond ((and (null? buf) (read done)) 'done)
      ((null? buf) (consumer)
        (#t
          (compare-and-set! buffer buf (cdr buf))
          (do-something (car buf)))
          (consumer)))))

(define producer-thrd (future (producer 1)))

(define consumer-thrds (list
                         (future (consumer))
                         (future (consumer))))

(deref producer-thrd)
(deref (car consumer-thrds))
(deref (cadr consumer-thrds))