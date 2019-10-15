;; Benchmark that compare recursive concurrent matrix multiplication with naive sequential matrix multiplication
(define (build-vector n init f)
  (let loop ((i 0)
             (v (make-vector n init)))
                    (if (< i n)
                      (begin
                        (vector-set! v i (f i))
                        (loop (+ i 1) v))
                      v)))

(define (extract-matrix M size fromx fromy)
  (build-vector size (vector)
    (lambda (i)
      (build-vector size 0 (lambda (j)
                              (vector-ref (vector-ref M (+ fromx i)) (+ fromy j)))))))

(define (combine-matrices size M22)
    (build-vector size (vector)
      (lambda (i)
        (build-vector size 0
          (lambda (j)
            (vector-ref (vector-ref M22 i) j))))))

(define (matrix-multiply B A)
  (if (= (vector-length A) 1)
    (vector (vector (vector-ref (vector-ref A 0) 0)))
    (let* ((A-sub (extract-matrix A (/ (vector-length A) 2) 0 0)))
      (combine-matrices (vector-length A) (matrix-multiply A-sub A-sub)))))

(define A (vector
             (vector 1 4)
             (vector 0 6)))
(matrix-multiply A A)