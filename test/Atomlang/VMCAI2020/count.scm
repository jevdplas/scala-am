(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define (counter)
  (let ((state (atom 0)))
    (lambda (op)
      (if (equal? op 'inc)
          (swap! state (lambda (curr) (+ curr 1)))
          (if (equal? op 'dec)
              (swap! state (lambda (curr) (- curr 1)))
              (if (equal? op 'get)
                  (read state)
                  (error "unknown operation")))))))

(define (thrd cnt ops)
  (if (= ops 0)
      'done
      (let ((op (random 3)))
        (if (= op 0)
            (cnt 'inc)
            (if (= op 1)
                (cnt 'dec)
                (cnt 'get)))
        (thrd cnt (- ops 1)))))

(define (create-thrds cnt n)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (future (thrd cnt (random 10))) acc))))))
    (loop 0 '())))

(define cnt (counter))

(define (launch-experiment n)
  (create-thrds cnt (random n)))
(define (terminate-experiment e)
  (map (lambda (t) (deref t)) e)
  (display "result: ")
  (display (cnt 'get))
  (newline))

(define exp1 (future (launch-experiment (random 2))))
(define exp2 (future (launch-experiment (random 5))))
(define exp3 (future (launch-experiment (random 7))))
(define exp4 (future (launch-experiment (random 10))))

(terminate-experiment exp1)
(terminate-experiment exp2)
(terminate-experiment exp3)
(terminate-experiment exp4)


