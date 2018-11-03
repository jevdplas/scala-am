;; Example taken from Dynamic Partial Order Reduction, Figure 1
;; Expected result: #t
(let* ((size 128)
       (max 4)
       (table (atom (make-vector size 0)))
       (thread (lambda (tid)
                (letrec ((hash (lambda (w) (modulo (* w 7) size)))
                         (process (lambda (m)
                                   (if (< m max)
                                    (letrec ((w (+ (* 11 (+ m 1)) tid))
                                             (update (lambda (h)
                                                      (if (swap! table (lambda (v) (vector-set! v h w)))
                                                       # t
                                                       (update (modulo (+ h 1) size))))))
                                     (update (hash w))
                                     (process (+ m 1)))
                                    # t))))
                 (process 0))))
       (t1 (future (thread 1)))
       (t2 (future (thread 2))))
 (and
  (deref t1)
  (deref t2)))
