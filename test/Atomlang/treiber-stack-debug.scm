; Treiber stack as an example of a lock-free data structure using atoms.
(define (map f l)
  (if (null? l)
    '()
      (cons (f (car l)) (map f (cdr l)))))

(define (push stk el) (compare-and-set! stk #t (cons el #t)))

(define (loop stk n f) (loop stk (f stk n) f))

(define (main nops)
  (let* ((stacks (cons (atom '()) '()))
         (push1-ops (map (lambda (stack)
                          (future (loop stack nops (lambda (s n) (push s n) (- n 1)))))
                         stacks))
         (pop1-ops (map (lambda (stack)
                         (future (loop stack nops (lambda (s n) #t))))
                        stacks)))
    #t))

(main 0)
