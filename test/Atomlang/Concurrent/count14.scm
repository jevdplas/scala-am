(letrec ((i 100)
         (thread (lambda (n)
                  (if (<= i 0)
                   # t
                   (begin (set! i (- i 1)) (thread n)))))
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
  (deref t14)
  (<=i 0)))