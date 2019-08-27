; Changed to use CAS, but may now be susceptible to ABA problem.
(define (append l m) ; Do not remove this non-mutable definition!
  (if (null? l)
      m
      (cons (car l) (append (cdr l) m))))
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define *actors* (atom '()))
(define (register-new-actor name f)
  (swap! *actors* (lambda (curr) (cons (list name f (atom '())) curr))))
(define (find-actor name)
  (let ((v (assoc name (read *actors*))))
    (if v
        v
        (error "no actor found"))))
(define (actor-mailbox name)
  (caddr (find-actor name)))
(define (generate-new-name) (random 10000))
(define (actor f)
  (lambda (init-state name)
    (letrec ((loop (lambda (receive state)
                     (let ((mb (actor-mailbox name)))
                       (if (null? (read mb))
                           ;; nothing to receive, wait
                           (loop receive state)
                           ;; message to receive -- Now implemented using CAS (susceptble to ABA) - can also be implemented using swap!.
                           (let retrieve ((mailbox (read mb)))
                             (let ((message (car mailbox)))
                               (if (compare-and-set! mb mailbox (cdr mailbox))
                                   (let ((action (receive name state (car message) (cdr message))))
                                     (if (eq? action 'terminate)
                                         'terminate
                                       (loop (car action) (cdr action))))
                                   (retrieve (read mb))))))))))
      (loop f init-state))))
(define (create act state)
  (let* ((name (generate-new-name)))
    (register-new-actor name (lambda () 'toremove))
    name))
(define (become self act st)
  (let ((r (act st self)))
    (if (eq? r 'terminate)
        'terminate
        (cons r st))))
(define (terminate) 'terminate)
(define (send target tag args)
  (swap! (actor-mailbox target) (lambda (curr) (append curr (list (cons tag args)))))) ; Append is not a mutable operation here.

(define (split from to)
  (let ((half (quotient (- to from) 2)))
    (list (cons from (+ from half))
          (cons (+ from half 1) to))))

(define fact-actor
  (actor
   (lambda (self state tag args)
     (if (eq? tag 'compute)
         (let ((from (car args))
               (to (cadr args))
               (parent (caddr args)))
           (if (<= (- to from) 1)
               (begin
                 (if (= from to)
                     (send parent 'computed (list from))
                     (send parent 'computed (list (* from to))))
                 (terminate))
               (let ((steps (split from to)))
                 (map (lambda (bounds)
                        (send (create fact-actor '())
                              'compute
                              (list (car bounds) (cdr bounds) self)))
                      steps)
                 (become self fact-actor-wait (list 0 (length steps) 1 parent)))))
         (error "unknown message")))))
(define fact-actor-wait
  (actor
   (lambda (self state tag args)
     (let ((received (car state))
           (fragments (cadr state))
           (current (caddr state))
           (parent (cadddr state)))
       (if (eq? tag 'computed)
           (let* ((result (car args))
                  (new-result (* current result)))
             (if (= (+ received 1) fragments)
                 (begin
                   (send parent 'computed (list new-result))
                   (terminate))
                 (become self fact-actor-wait
                         (list
                          (+ received 1) fragments new-result parent))))
           (error "unknown message"))))))
(define master-actor
  (actor
   (lambda (self state tag args)
     (if (eq? tag 'compute)
         (let ((n (car args)))
           (send (create fact-actor '()) 'compute (list 1 n self))
           (become self master-actor '()))
         (if (eq? tag 'computed)
             (let ((res (car args)))
               (display res)
               (terminate))
             (error "unknown message"))))))
(define act (create master-actor '()))
(send act 'compute (list 2))
