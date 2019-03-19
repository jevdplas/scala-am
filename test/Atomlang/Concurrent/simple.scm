;; Example based on Dynamic Partial Order Reduction paper
(let* ((x 0)
       (y 0)
       (t1 (future (set! x 1) (set! x 2)))
       (t2 (future (set! y 1) (set! x 3))))
 (deref t1)
 (deref t2)
 (and (= y 1)
  (or (= x 2
       (= x 3)))))