



(define (append l m)
  (if (null? l)
      m
      (cons (car l) (append (cdr l) m))))
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define *actors* (t/ref '()))
(define *actors-lock* (t/new-lock))
(define (register-new-actor name f)
  (t/acquire *actors-lock*)
  (t/ref-set *actors* (cons (list name f (t/new-lock) (t/ref '())) (t/deref *actors*)))
  (t/release *actors-lock*))
(define (find-actor name)
  (let ((v (assoc name (t/deref *actors*))))
    (if v
        v
        (error "no actor found"))))
(define (actor-lock name)
  (caddr (find-actor name)))
(define (actor-mailbox name)
  (cadddr (find-actor name)))
(define (generate-new-name) (int-top))
(define (actor f)
  (lambda (init-state name)
    (letrec ((loop (lambda (receive state)
                     (let ((mb (actor-mailbox name))
                           (lock (actor-lock name)))
                       (if (null? (t/deref mb))
                           ;; nothing to receive, wait
                           (begin
                             ;(sleep 0.1)
                             (loop receive state))
                           ;; message to receive
                           (begin
                             (t/acquire (actor-lock name))
                             (let ((message (car (t/deref mb))))
                               (t/ref-set mb (cdr (t/deref mb)))
                               (t/release (actor-lock name))
                               (let ((action (receive name state (car message) (cdr message))))
                                 (if (eq? action 'terminate)
                                     (begin
                                       'done)
                                     (loop (car action) (cdr action)))))))))))
      (loop f init-state))))
(define (create act state)
  (let* ((name (generate-new-name)))
    (register-new-actor name (lambda () 'toremove))
    (t/spawn (act state name))
    name))
(define (become self act st) (cons (act st self) st))
(define (terminate) 'terminate)
(define (send target tag args)
  (let ((lock (actor-lock target))
        (mb (actor-mailbox target)))
    (t/acquire lock)
    (t/ref-set mb (append (t/deref mb) (list (cons tag args))))
    (t/release lock)))

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
(send act 'compute (list (int-top)))
;(sleep 2)

