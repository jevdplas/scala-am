(letrec ((i 100)
         (thread (lambda (n)
                  (if (<= i 0)
                   #t
                   (begin (set! i (- i 1)) (thread n)))))
         (t1 (future (thread 1)))
         (t2 (future (thread 2)))
         (t3 (future (thread 3)))
         (t4 (future (thread 4)))
         (t5 (future (thread 5)))
         (t6 (future (thread 6))))
 (and
  (deref t1)
  (deref t2)
  (deref t3)
  (deref t4)
  (deref t5)
  (deref t6)
  (<=i 0)))