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
                                                       #t
                                                       (update (modulo (+ h 1) size))))))
                                     (update (hash w))
                                     (process (+ m 1)))
                                    #t))))
                 (process 0))))
       (t1 (future (thread 1)))
       (t2 (future (thread 2)))
       (t3 (future (thread 3)))
       (t4 (future (thread 4)))
       (t5 (future (thread 5)))
       (t6 (future (thread 6)))
       (t7 (future (thread 7)))
       (t8 (future (thread 8)))
       (t9 (future (thread 9)))
       (t10 (future (thread 10)))
       (t11 (future (thread 11)))
       (t12 (future (thread 12)))
       (t13 (future (thread 13)))
       (t14 (future (thread 14))))
 (and
  (deref t1)
  (deref t2)
  (deref t3)
  (deref t4)
  (deref t5)
  (deref t6)
  (deref t7)
  (deref t8)
  (deref t9)
  (deref t10)
  (deref t11)
  (deref t12)
  (deref t13)
  (deref t14)))