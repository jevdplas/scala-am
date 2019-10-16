(define (map f l)
  (if (pair? l)
    (cons (f (car l)) (map f (cdr l)))
    (if (null? l)
        '()
      (error "map applied to a non-list"))))

(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define (split from to)
  (let ((half (quotient (- to from) 2)))
    (list (cons from (+ from half))
      (cons (+ from half 1) to))))

(define (fact-thrd from to)
  (if (= from to)
    from
    (let ((steps (split from to)))
      (foldl * 1
        (map (lambda (t) (deref t))
          (map (lambda (bounds)
                 (future (fact-thrd (car bounds) (cdr bounds))))
            steps))))))

(deref(future (fact-thrd 1 10)))