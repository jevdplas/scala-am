; Simplification of matmul
(define (matrix-multiply A)
  (if (= (vector-length A) 1)
    (vector (vector (vector-ref (vector-ref A 0) 0)))
    (matrix-multiply (vector 1))))

(matrix-multiply (vector (vector 1 4) (vector 0 6)))