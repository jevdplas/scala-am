(define (t/new-lock)
  (atom #f))
(define (t/acquire lock)
  (let try ()
    (if (compare-and-set! lock #f #t)
        #t
        (try))))
(define (t/release lock)
  (reset! lock #f))

;; Dining philosophers problem with dictionary
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define N (+ 1 42))
(define Turns 42)

(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))
(define forks
  (build-vector N #f (lambda (i) (t/new-lock))))

(define dictionary (atom 0))
(define dictionary-lock (t/new-lock))

(define (philosopher i)
  (letrec ((left i)
           (right (modulo (- i 1) N))
           (process (lambda (turn)
                      (if (> turn Turns)
                          'done
                          (if #t
                              (begin
                                (t/acquire (vector-ref forks (min left right)))
                                (t/acquire (vector-ref forks (max left right)))
                                ;; eat
                                (display "Eating...")
                                (t/release (vector-ref forks (min left right)))
                                (t/release (vector-ref forks (max left right)))
                                (process (+ turn 1)))
                              (begin
                                (t/acquire dictionary-lock)
                                (if (= (read dictionary) i)
                                    (reset! dictionary (modulo (+ i 1) N))
                                    ;; doesn't have the dictionary
                                    'nothing)
                                (t/release dictionary-lock)
                                (process turn)))))))
    (process 0)))

(define (do-n n f)
  (letrec ((loop (lambda (i acc)
                   (if (= i n)
                       acc
                       (loop (+ i 1) (cons (f i) acc))))))
    (loop 0 '())))

(define philosophers (do-n N (lambda (i) (future (philosopher i)))))

;; Wait until the end
(map (lambda (t) (deref t)) philosophers)
