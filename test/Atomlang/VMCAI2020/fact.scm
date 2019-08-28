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

(define N (+ 1 42))
(define FragmentSize 10)

(define (split from to)
  (let ((half (quotient (- to from) 2)))
    (list (cons from (+ from half))
             (cons (+ from half 1) to))))

(define (product from to)
  (if (= from to)
      from
      (* from (product (+ from 1) to))))

(define (fact-thrd from to)
  (if (<= (- to from) FragmentSize)
      (product from to)
      (let ((steps (split from to)))
        (foldl * 1
               (map (lambda (t)
                      ;;(display t)
                      (display (deref t))
                      (deref t))
                    (map (lambda (bounds)
                           (future 1))
                            ;(let ((v (fact-thrd (car bounds) (cdr bounds))))
                            ;  (display v)
                            ;  v)))
                         steps))))))

(define (fact-thrd-ref from to result)
  (if (<= (- to from) FragmentSize)
      (let ((partial-fact (product from to)))
        (swap! result (lambda (curr) (* partial-fact curr))))
      (let ((steps (split from to)))
        (map (lambda (t) (deref t))
             (map (lambda (bounds)
                    (future
                     (fact-thrd-ref (car bounds) (cdr bounds)
                                      result)))
                  steps)))))

(define (fact n)
  (let* ((t1 (future (fact-thrd 1 n)))
         (result (atom 1))
         (t2 (future (fact-thrd-ref 1 n result)))
         (res1 (deref t1))
         (res2 (begin (deref t2) (read result))))
    (if (= res1 res2)
        (display res1)
        (display "factorials don't match..."))
(newline)))

(fact N)

