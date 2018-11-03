; Inspired by the slidedeck of the course "Multicore Programming" (2017-2018) - Chapter 6.

(define (new-list-with-length) (atom (cons '() 0)))
(define (app lwl e)
 (let ((lst (car lwl))
       (cnt (cdr lwl)))
  (cons (cons e lst)
   (+ cnt 1))))
(define (append atm e)
 (swap! atm app e))

(define (add-lst atm l)
 (for-each (lambda (i) (append atm i))
  l))

(define (count e l)
 (let loop ((curr l)
            (cnt 0))
  (cond ((null? curr) cnt)
   ((equal? (car curr) e) (loop (cdr curr) (+ cnt 1)))
   (else (loop (cdr curr) cnt)))))
(define a (new-list-with-length))
(define l1 '(1 2 3 4 5 6 7 8 9 10))
(define l2 '(11 12 13 14 15 16 17 18 19 20))
(define f1 (future (add-lst a l1)))
(define f2 (future (add-lst a l2)))
(deref f1) (deref f2)
(and
 (= 20 (length (car (deref a))))
 (= 20 (cdr (deref a)))
 (not (member # f (map (lambda (e) (= (count e (car (deref a))) 1)) (append l1 l2)))))