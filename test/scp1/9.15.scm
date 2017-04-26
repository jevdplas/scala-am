(define (ontdubbel! lijst)
  (let ((deEven '())
        (deOneven '()))
    (define (ontdubbel-iter prevE prevO restLijst)
      (cond ((null? restLijst) (set-cdr! prevE '())
                               (set-cdr! prevO '())
                               (cons deEven deOneven))
            ((even? (car restLijst))
             (if (null? prevE)
                 (set! deEven restLijst)
                 (set-cdr! prevE restLijst))
             (ontdubbel-iter restLijst prevO (cdr restLijst)))
            (else (if (null? prevO)
                      (set! deOneven restLijst)
                      (set-cdr! prevO restLijst))
                  (ontdubbel-iter prevE restLijst (cdr restLijst)))))
    (ontdubbel-iter deEven deOneven lijst)))

(equal? (ontdubbel! '(1 2 3 4 5 6 7 8 9 10)) '((2 4 6 8 10) 1 3 5 7 9))