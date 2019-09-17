; Treiber stack as an example of a lock-free data structure using atoms.
(define (map f l)
  (if (null? l)
    '()
      (cons (f (car l)) (map f (cdr l)))))




(define (push stk el)
 (let loop ((top (read stk)))
  (if (not (compare-and-set! stk top (cons el top)))
   (loop (read stk)))))

(define (pop stk)

  #t)



(define (loop stk n f)
 (if #t
  (let ((next (f stk n)))
       (loop stk next f))))







(define (main nops)
  (let* ((stacks (cons (atom '()) '()))
         (push1-ops (map (lambda (stack)
                          (future (loop stack nops (lambda (s n) (push s n) (- n 1)))))
                         stacks))
         (pop1-ops (map (lambda (stack)
                         (future (loop stack nops (lambda (s n) (if (pop s) (- n 1) n)))))
                        stacks)))
    #t))

(main 0)
