;; Sudoku checker
(define board
  (list
   (list 6 2 4 5)
   (list 5 1 9 7)
   (list 4 9 6 1)
   (list 2 8 5 4)))

(define (walk-row i)
  (letrec ((loop (lambda (j seen)
                   (if (< j 2)
                       (if (member (list-ref (list-ref board i) j) seen)
                           #f
                           (loop (+ j 1) (cons (list-ref (list-ref board i) j) seen)))
                       #t))))
    (loop 0 '())))

(define (walk-rows)
  (walk-row 0))

(define (walk-col j)
  (letrec ((loop (lambda (i seen)
                   (if (< i 2)
                       (if (member (list-ref (list-ref board i) j) seen)
                           #f
                           (loop (+ i 1) (cons (list-ref (list-ref board i) j) seen)))
                       #t))))
    (loop 0 '())))

(define (walk-cols)
  (let ((wc1 (future (walk-col 0)))
        (wc2 (future (walk-col 1))))
    (and (deref wc1) (deref wc2))))

(define all-rows (future (walk-rows)))
(define all-cols (future (walk-cols)))

(and
 (deref all-rows)
 (deref all-cols)
)
