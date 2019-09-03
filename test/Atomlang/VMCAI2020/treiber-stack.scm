; Treiber stack as an example of a lock-free data structure using atoms.
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

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

;; Was:
;; (define stack (new-stack))
;; (define f1 (future (loop stack 25 (lambda (s n) (push s n) (- n 1)))))
;; (define f2 (future (loop stack 25 (lambda (s n) (if (pop s) (- n 1) n)))))
;; 
;; (deref f1)
;; (deref f2)
;; (not (pop stack))

(define (interleave l1 l2)
  (if (and (pair? l1) (pair? l2))
      (cons (car l1) (cons (car l2) (interleave (cdr l1) (cdr l2))))
      '()))

(define (main nstacks nops)
  (let* ((stacks (create-stacks nstacks))
         (push1-ops (map (lambda (stack)
                          (future (loop stack nops (lambda (s n) (push s n) (- n 1)))))
                         stacks))
         (push2-ops (map (lambda (stack)
                           (future (loop stack nops (lambda (s n) (push s n) (- n 1)))))
                         stacks))
         (pop1-ops (map (lambda (stack)
                         (future (loop stack nops (lambda (s n) (if (pop s) (- n 1) n)))))
                        stacks))
         (pop2-ops (map (lambda (stack)
                          (future (loop stack nops (lambda (s n) (if (pop s) (- n 1) n)))))
                        stacks)))
    (letrec ((check-results (lambda (stacks push-ops pop-ops)
                              (if (pair? stacks)
                                  (begin
                                    (deref (car push-ops)) (deref (cadr push-ops))
                                    (deref (car pop-ops)) (deref (cadr pop-ops))
                                    (if (not (pop (car stacks)))
                                        (check-results (cdr stacks) (cddr push-ops) (cddr pop-ops))
                                        #f))
                                  #t))))
      (check-results stacks (interleave push1-ops push2-ops) (interleave pop1-ops pop2-ops)))))

(define (main-loop N)
  (if (= N 0)
      #t
      (begin
        (main (random 5) (random 5))
        (main-loop (- N 1)))))
(main-loop (random 10))
