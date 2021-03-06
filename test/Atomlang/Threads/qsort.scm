;; Parallel quick-sort
(define (for-each f l)
  (if (pair? l)
      (begin
        (f (car l))
        (for-each f (cdr l)))
      (if (null? l)
          #t
          (error "for-each applied to a non-list"))))

(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define N 42)
(define array-to-sort
  (build-vector N #f (lambda (i) (atom (random 100)))))

(define (swap array i j)
  (let* ((ref1 (vector-ref array i))
         (ref2 (vector-ref array j))
         (tmp (read ref1)))
    (reset! ref1 (read ref2))
    (reset! ref2 tmp)))

(define (partition array lo hi)
  (let ((pivot (read (vector-ref array hi))))
    (letrec ((loop (lambda (j i)
                     (if (= j hi)
                         i
                         (if (< (read (vector-ref array j)) pivot)
                             (begin
                               (swap array (+ i 1) j)
                               (loop (+ j 1) (+ i 1)))
                             (loop (+ j 1) i))))))
      (let ((i (loop lo (- lo 1))))
        (if (< (read (vector-ref array hi))
               (read (vector-ref array (+ i 1))))
            (swap array (+ i 1) hi)
            #t)
        (+ i 1)))))

(define (quicksort array left right)
  (let ((pivot left))
    (if (<= right left)
        ;; do nothing
        #t
        ;; sort
        (let* ((new-pivot (partition array left right))
               (tleft (future (quicksort array left (- new-pivot 1))))
               (tright (future (quicksort array (+ new-pivot 1) right))))
          (deref tleft)
          (deref tright)
          #t))))

(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

(define (display-array array)
  (for-each (lambda (i)
              (display (read (vector-ref array i)))
              (display " "))
            (range 0 (vector-length array)))
  (newline))

(define (qsort array)
  (display-array array)
  (quicksort array 0 (- (vector-length array) 1))
  (display-array array))

(qsort array-to-sort)
