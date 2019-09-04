;; Ugly parallel merge sort.

(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(define (reverse l)
  (let loop ((cur l)
              (res '()))
    (if (null? cur)
      res
      (loop (cdr cur)
        (cons (car cur) res)))))

(define (merge l-one l-two)
  (let loop ((l1 l-one)
              (l2 l-two)
              (res '()))
    (cond ((null? l1) (append (reverse res) l2))
      ((null? l2) (append (reverse res) l1))
      ((<= (car l1) (car l2)) (loop (cdr l1) l2 (cons (car l1) res)))
      (else (loop l1 (cdr l2) (cons (car l2) res))))))

(define (split-at lst n)
  (let loop ((m n)
              (lt lst)
              (fs '()))
    (if (= m 0)
      (cons fs lt)
      (loop (- m 1)
        (cdr lt)
        (cons (car lt) fs)))))

(define (mergesort lst)
  (let ((len (length lst)))
    (cond ((>= 1 len) lst)
      ; ((>= 10 len) (bubble-sort lst))
      (else
        (let* ((splt (split-at lst (floor (/ len 2))))
                (snd  (future (mergesort (cdr splt))))
                (fst  (mergesort (car splt))))
          (merge fst (deref snd)))))))


(define (sorted? l)
  (if (or (null? l) (null? (cdr l)))
    #t
    (if (<= (car l) (cadr l))
      (sorted? (cdr l))
      #f)))

(define (generate-list size)
  (if (= size 0)
      '()
    (cons (- (random 250) (random 500)) (generate-list (- size 1)))))

(define lst (generate-list (random 100)))
(sorted? (mergesort lst))