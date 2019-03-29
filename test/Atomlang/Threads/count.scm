



(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define (counter)
  (let ((lock (t/new-lock))
        (state (t/ref 0)))
    (lambda (op)
      (if (equal? op 'inc)
          (begin
            (t/acquire lock)
            (t/ref-set state (+ (t/deref state) 1))
            (t/release lock))
          (if (equal? op 'dec)
              (begin
                (t/acquire lock)
                (t/ref-set state (- (t/deref state) 1))
                (t/release lock))
              (if (equal? op 'get)
                  (t/deref state)
                  (error "unknown operation")))))))

(define (thread cnt ops)
  (if (= ops 0)
      'done
      (let ((op (random 3)))
        (if (= op 0)
            (cnt 'inc)
            (if (= op 1)
                (cnt 'dec)
                (cnt 'get)))
        (thread cnt (- ops 1)))))

(define NOPS (int-top))
(define (create-threads cnt n)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (t/spawn (thread cnt NOPS)) acc))))))
    (loop 0 '())))

(define N (int-top))
(define cnt (counter))
(map (lambda (t) (t/join t))
     (create-threads cnt N))
(display "result: ")
(display (cnt 'get))
(newline)
