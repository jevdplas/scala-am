; Treiber stack as an example of a lock-free data structure using atoms.
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      '()))

(define (new-stack)
 (atom '()))

(define (push stk el)
 (let loop ((top (read stk)))
  (if (not (compare-and-set! stk top (cons el top)))
   (loop (read stk)))))

(define (pop stk)
 (let loop ((top (read stk)))
  (cond ((null? top) #f)
        ((compare-and-set! stk top (cdr top)) (car top))
        (else  (loop (read stk))))))

(define (loop stk n f)
 (if (> n 0)
  (let ((next (f stk n)))
       (loop stk next f))))

(define (create-stacks n)
  (if (= n 0)
      '()
      (cons (new-stack) (create-stacks (- n 1)))))

(define (main nstacks nops)
  (let* ((stacks (create-stacks 0))
         (push1-ops (map (lambda (stack)
                          (future (loop stack nops (lambda (s n) (push s n) (- n 1)))))
                         stacks))
         (pop1-ops (map (lambda (stack)
                         (future (loop stack nops (lambda (s n) (if (pop s) (- n 1) n)))))
                        stacks)))
    #t))

(main 0 0)
