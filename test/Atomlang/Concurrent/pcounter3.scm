(letrec ((counter (atom 0))
         (thread (lambda (n)
                  (letrec ((old (deref counter))
                           (new (+ old 1)))
                   (if (compare-and-set! counter old new)
                    # t
                    (thread n)))))
         (t1 (future (thread 1)))
         (t2 (future (thread 2)))
         (t3 (future (thread 3))))
 (deref t1)
 (deref t2)
 (deref t3)
 (= counter 3))