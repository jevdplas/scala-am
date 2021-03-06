(define wants-to-enter-0 (atom #f))
(define wants-to-enter-1 (atom #f))
(define turn (atom 0))
(define counter (atom 0))

(define (p0)
  (reset! wants-to-enter-0 #t)
  (letrec ((wait (lambda ()
                   (if (read wants-to-enter-1)
                       (if (not (= (read turn) 0))
                           (letrec ((wait2 (lambda ()
                                             (if (not (= (read turn) 0))
                                                 (wait2)
                                                 #t))))
                             (reset! wants-to-enter-0 #f)
                             (wait2)
                             (reset! wants-to-enter-0 #t)
                             (wait))
                           (wait))
                       #t))))
    (wait)
    ;; Critical section
    (reset! counter (+ (read counter) 1))
    (reset! turn 1)
    (reset! wants-to-enter-0 #f)))

(define (p1)
  (reset! wants-to-enter-1 #t)
  (letrec ((wait (lambda ()
                   (if (read wants-to-enter-0)
                       (if (not (= (read turn) 1))
                           (letrec ((wait2 (lambda ()
                                             (if (not (= (read turn) 1))
                                                 (wait2)
                                                 #t))))
                             (reset! wants-to-enter-1 #f)
                             (wait2)
                             (reset! wants-to-enter-1 #t)
                             (wait))
                           (wait))
                       #t))))
    (wait)
    ;; Critical section
    (reset! counter (+ (read counter) 1))
    (reset! turn 0)
    (reset! wants-to-enter-1 #f)))

;; Creates the two threads, which each increase the counter.
(define t0 (future (p0)))
(define t1 (future (p1)))
(deref t0)
(deref t1)
(= (read counter) 2)
