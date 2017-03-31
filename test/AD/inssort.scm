(define (insertion-sort vector)
  (let ((high (- (vector-length vector) 1)))
    (define (shift-left vector index)
      (vector-set! vector (- index 1) (vector-ref vector index)))
    (define (insert-sort-iter index1)
      (define (insert index1)
        (let ((insert-value (vector-ref vector (- index1 1))))
          (define (insert-iter index2)
              (cond ((and (<= index2 high)
                          (< (vector-ref vector index2)
                             insert-value))
                     (shift-left vector index2)
                     (insert-iter (+ index2 1)))
                    (else (vector-set! vector (- index2 1) insert-value))))
        (insert-iter index1)))
      (if (> index1 0)
	  (begin
             (insert index1)
             (insert-sort-iter (- index1 1)))))
    (insert-sort-iter high)))

(define vect (vector 5 2 7 1 0 9 8 6 3 4))
(insertion-sort vect)
(equal? vect (vector 0 1 2 3 4 5 6 7 8 9))