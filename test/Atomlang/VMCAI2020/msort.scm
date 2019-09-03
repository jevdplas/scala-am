;; Multithreaded merge-sort
(define (merge-lists xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (< (car xs) (car ys))
              (cons (car xs) (merge-lists (cdr xs) ys))
              (cons (car ys) (merge-lists xs (cdr ys)))))))

(define (take l n)
  (if (or (= n 0) (null? l))
      '()
      (cons (car l) (take (cdr l) (- n 1)))))

(define (drop l n)
  (if (or (= n 0) (null? l))
      l
      (drop (cdr l) (- n 1))))

(define (merge-sort l)
  (let ((len (length l)))
    (if (< len 2)
        l
        (if (= len 2)
            (merge-lists (list (car l)) (list (cadr l)))
            (let ((first-half (future (merge-sort (take l (quotient len 2)))))
                  (second-half (future (merge-sort (drop l (quotient len 2))))))
              (merge-lists (deref first-half) (deref second-half)))))))

(define (append l m) ; Do not remove this non-mutable definition!
  (if (null? l)
      m
      (cons (car l) (append (cdr l) m))))

;; Quick-sort code comes from https://stackoverflow.com/questions/43474008/scheme-implementing-a-quick-sort
(define (partition pred xs)
   (let part ((ps '()) (ns '()) ; Initial "positives" `ps`, and "negatives" `ns`
              (xs2 xs) )
      (if (null? xs2)
         (cons ps ns)             ; Returning pair of lists
         (let ((x (car xs2)))     ; Memoization of `(car lst)`
            (if (pred x)
               (part (cons x ps) ns (cdr xs2))
               (part ps (cons x ns) (cdr xs2)) )))))

(define (quick-sort xs)
   (if (null? xs) '()
      (let* ((x (car xs))
             (pn (partition               ; Memoization of `partition`
                    (lambda (y)
                       (< y x) )
                    (cdr xs) ))
             (left (future (quick-sort (car pn)))) ; Extracting positives from pair
             (right (future (quick-sort (cdr pn))))) ; negatives
         (append (deref left)
                 (append (list x)                  ; Pivot
                         (deref right))))))


(define (sorted? l)
  (if (or (null? l) (null? (cdr l)))
      #t
      (if (<= (car l) (cadr l))
          (sorted? (cdr l))
          #f)))

(define (generate-list size)
  (if (= size 0)
      '()
      (cons (random 100) (generate-list (- size 1)))))

(define L (generate-list (random 100)))
(sorted? (merge-sort L))
(sorted? (quick-sort L))
