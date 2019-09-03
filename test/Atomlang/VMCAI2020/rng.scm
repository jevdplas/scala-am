;; Random number generate sharing state among multiple thrds
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define N (+ (random 10) 10))
(define (range from to)
  (if (= from to)
      (cons from '())
      (cons from (range (+ from 1) to))))

(define (rng seed)
  (letrec ((state (atom seed))
           (lock (atom #f)))
    (lambda ()
      (let try ()
        (if (compare-and-set! lock #f #t)
            (begin
              (reset! state (modulo (+ (* 5245 (read state)) 12345) 107374182))
              (let ((n (modulo (quotient (read state) 65535) 32768)))
                (reset! lock #f)
                n))
            (try))))))
(define gen (rng 100))
(define ts (map (lambda (x) (future (gen))) (range 1 N)))
(map (lambda (t) (deref t)) ts)