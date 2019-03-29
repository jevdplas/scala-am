



;; Random number generate sharing state among multiple threads
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define N (+ (int-top) 10))
(define (range from to)
  (if (= from to)
      (cons from '())
      (cons from (range (+ from 1) to))))

(define (rng seed)
  (letrec ((state (t/ref seed))
           (lock (t/new-lock)))
    (lambda ()
      (t/acquire lock)
      (t/ref-set state (modulo (+ (* 5245 (t/deref state)) 12345) 107374182))
      (let ((n (modulo (quotient (t/deref state) 65535) 32768)))
        (t/release lock)
        n))))
(define gen (rng 100))
(define ts (map (lambda (x) (t/spawn (gen))) (range 1 N)))
(map (lambda (t) (t/join t)) ts)
