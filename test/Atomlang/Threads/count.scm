(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define (counter)
  (let ((lock (t/new-lock))
        (state (atom 0)))
    (lambda (op)
      (if (equal? op 'inc)
          (begin
            (t/acquire lock)
            (reset! state (+ (read state) 1))
            (t/release lock))
          (if (equal? op 'dec)
              (begin
                (t/acquire lock)
                (reset! state (- (read state) 1))
                (t/release lock))
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

(define NOPS 42)
(define (create-thrds cnt n)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (future (thrd cnt NOPS)) acc))))))
    (loop 0 '())))

(define N 42)
(define cnt (counter))
(map (lambda (t) (deref t))
     (create-thrds cnt N))
(display "result: ")
(display (cnt 'get))
(newline)
