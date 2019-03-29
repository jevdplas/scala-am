



;; Parallel prefix sum with threads
;; https://stackoverflow.com/questions/10053629/parallel-prefix-sum-fastest-implementation/12874227#12874227
(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define (for-each f l)
  (if (pair? l)
      (begin
        (f (car l))
        (for-each f (cdr l)))
      (if (null? l)
          #t
          (error "for-each applied to a non-list"))))

(define N (expt 2 (int-top)))
(define input
  (build-vector N #f (lambda (i) (t/ref i))))

(define (log2 n)
  (inexact->exact (/ (log n) (log 2))))

(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

(define (range-down from to)
  (letrec ((loop (lambda (i acc)
                   (if (> i from)
                       acc
                       (loop (+ i 1) (cons i acc))))))
    (loop to '())))

(define (reverse l)
  (define (helper l tail)
    (if (null? l) tail
        (helper (cdr l) (cons (car l) tail))))
  (helper l '()))

(define (range-offset a b offset)
  (letrec ((loop (lambda (i acc)
                   (if (>= i b)
                       (reverse acc)
                       (loop (+ i offset) (cons i acc))))))
    (loop a '())))


(define (display-vector v)
  (for-each (lambda (i)
              (display (t/deref (vector-ref v i))) (display " "))
            (range 0 (vector-length v)))
  (newline))

(define (up-sweep-phase v n)
  (define (computation d k)
    (let ((v1 (t/deref (vector-ref v (- (+ k (expt 2 d)) 1))))
          (v2 (t/deref (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)))))
      (t/ref-set (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)) (+ v1 v2))))
  (for-each (lambda (d)
              (for-each (lambda (t) (t/join t))
                        (map (lambda (k)
                               (t/spawn (computation d k)))
                             (range-offset 0 n (expt 2 (+ d 1))))))
            (range 0 (log2 n))))

(define (down-sweep-phase v n)
  (define (computation d k)
    (let ((t (t/deref (vector-ref v (- (+ k (expt 2 d)) 1))))
          (v1 (t/deref (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)))))
      (t/ref-set (vector-ref v (- (+ k (expt 2 d)) 1)) v1)
      (t/ref-set (vector-ref v (- (+ k (expt 2 (+ d 1))) 1)) (+ t v1))))
  (t/ref-set (vector-ref v (- n 1)) 0)
  (for-each (lambda (d)
              (for-each (lambda (t) (t/join t))
                        (map (lambda (k)
                               (t/spawn (computation d k)))
                             (range-offset 0 n (expt 2 (+ d 1))))))
            (range-down (- (log2 n) 1) 0)))

(define (psum v)
  (let ((n (vector-length v)))
    (up-sweep-phase v n)
    (display-vector v)
    (down-sweep-phase v n)))

(display-vector input)
(psum input)
(display-vector input)
