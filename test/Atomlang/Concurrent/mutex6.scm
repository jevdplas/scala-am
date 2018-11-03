(letrec ((lock (atom # f))
         (acq (lambda ()
               (if (compare-and-set! lock # f # t)
                # t
                (acq))))
         (rel (lambda ()
               (set! lock # f)))
         (counter 0)
         (inc (lambda ()
               (acq)
               (set! counter (+ counter 1))
               (rel)))
         (t1 (future (inc)))
         (t2 (future (inc)))
         (t3 (future (inc)))
         (t4 (future (inc)))
         (t5 (future (inc)))
         (t6 (future (inc))))
 (deref t1)
 (deref t2)
 (deref t3)
 (deref t4)
 (deref t5)
 (deref t6)
 (= counter 6))
