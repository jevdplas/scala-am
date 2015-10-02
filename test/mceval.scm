(define (map f l) (if (pair? l) (cons (f (car l)) (map f (cdr l))) (if (null? l) '() (error "map applied to a non-list"))))
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
(define (variable? exp)
  (symbol? exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (cons 'if (cons predicate (cons consequent (cons alternative '())))))
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (mk-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (mk-begin seq))))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-actions clause) (cdr clause))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))
(define (make-procedure parameters body env) (cons 'procedure (cons parameters (cons body (cons env '())))))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (letrec ((env-loop (lambda (env)
                       (letrec ((scan (lambda (vars vals)
                                        (cond ((null? vars)
                                               (env-loop (enclosing-environment env)))
                                              ((eq? var (car vars))
                                               (car vals))
                                              (else (scan (cdr vars) (cdr vals)))))))
                         (if (eq? env the-empty-environment)
                             (error "Unbound variable" var)
                             (let ((frame (first-frame env)))
                               (scan (frame-variables frame)
                                     (frame-values frame))))))))
    (env-loop env)))
(define (set-variable-value! var val env)
  (letrec ((env-loop (lambda (env)
                       (letrec ((scan (lambda (vars vals)
                                        (cond ((null? vars)
                                               (env-loop (enclosing-environment env)))
                                              ((eq? var (car vars))
                                               (set-car! vals val))
                                              (else (scan (cdr vars) (cdr vals)))))))
                         (if (eq? env the-empty-environment)
                             (error "Unbound variable -- SET!" var)
                             (let ((frame (first-frame env)))
                               (scan (frame-variables frame)
                                     (frame-values frame))))))))
    (env-loop env)))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (letrec ((scan (lambda (vars vals)
                     (cond ((null? vars)
                            (add-binding-to-frame! var val frame))
                           ((eq? var (car vars))
                            (set-car! vals val))
                           (else (scan (cdr vars) (cdr vals)))))))
      (scan (frame-variables frame)
            (frame-values frame)))))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (cons (cons '= (cons = '()))
        (cons (cons '* (cons * '()))
              (cons (cons '- (cons - '())) '()))))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (cons 'primitive (cons (cadr proc) '())))
       primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (apply-primitive-procedure proc args)
  (let ((f (primitive-implementation proc))
        (n (length args)))
    (cond ((= n 0) (f))
          ((= n 1) (f (car args)))
          ((= n 2) (f (car args) (cadr args)))
          (else (error "ERROR -- can't handle more than two arguments")))))
(define (mceval exp env)
  (letrec ((eval-sequence (lambda (exps env)
                            (cond ((last-exp? exps) (mceval (first-exp exps) env))
                                  (else (mceval (first-exp exps) env)
                                        (eval-sequence (rest-exps exps) env))))))
    (let ((mcapply (lambda (procedure arguments)
                     (cond ((primitive-procedure? procedure)
                            (apply-primitive-procedure procedure arguments))
                           ((compound-procedure? procedure)
                            (eval-sequence
                             (procedure-body procedure)
                             (extend-environment
                              (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
                           (else
                            (error
                             "Unknown procedure type -- APPLY" procedure))))))
      (let ((eval-if (lambda (exp env)
                       (if (true? (mceval (if-predicate exp) env))
                           (mceval (if-consequent exp) env)
                           (mceval (if-alternative exp) env)))))
        (let ((eval-assignment (lambda (exp env)
                                 (set-variable-value! (assignment-variable exp)
                                                      (mceval (assignment-value exp) env)
                                                      env)
                                 'ok)))
          (let ((eval-definition (lambda (exp env)
                                   (define-variable! (definition-variable exp)
                                     (mceval (definition-value exp) env)
                                     env)
                                   'ok)))
            (letrec ((list-of-values (lambda (exps env)
                                       (if (no-operands? exps)
                                           '()
                                           (cons (mceval (first-operand exps) env)
                                                 (list-of-values (rest-operands exps) env))))))
              (cond ((self-evaluating? exp) exp)
                    ((variable? exp) (lookup-variable-value exp env))
                    ((quoted? exp) (text-of-quotation exp))
                    ((assignment? exp) (eval-assignment exp env))
                    ((definition? exp) (eval-definition exp env))
                    ((if? exp) (eval-if exp env))
                    ((lambda? exp)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env))
                    ((begin? exp)
                     (eval-sequence (begin-actions exp) env))
                    ((cond? exp) (mceval (cond->if exp) env))
                    ((application? exp)
                     (mcapply (mceval (operator exp) env)
                              (list-of-values (operands exp) env)))
                    (else
                     (error "Unknown expression type -- EVAL" exp))))))))))
(mceval '(((lambda (f) (lambda (x) (f f x)))
           (lambda (g n)
             (if (= n 0)
                 1
                 (* n (g g (- n 1)))))) 8)
        the-global-environment)