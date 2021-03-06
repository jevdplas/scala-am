;; Conway's game of life with concurrent computations
(define N (expt 2 2))
(define ITERATIONS 2)
(define MAXthrdSIZE 10)

(define (random-bool)
  (> (random 100) 50))

(define (append l m)
  (if (null? l)
      m
      (cons (car l) (append (cdr l) m))))

(define (build-vector1 n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (build-vector2 n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (if (null? l)
          '()
          (error "map applied to a non-list"))))

(define (for-each f l)
  (if (pair? l)
      (begin
        (f (car l))
        (for-each f (cdr l)))
      (if (null? l)
          #t
          (error "for-each applied to a non-list"))))

(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                       acc
                       (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

(define (new-cell)
  (list
   ;; New content of the cell
   (atom #f)
   ;; Current cell content
   (atom (random-bool))))
(define *field*
  (build-vector1 N (make-vector N (new-cell))
                (lambda (i)
                  (build-vector2 N (new-cell) (lambda (i) (new-cell))))))

(define (display-field)
  (for-each (lambda (i)
              (for-each (lambda (j)
                          (if (read  (cadr (field-ref i j)))
                              (display "x ")
                              (display "  ")))
                        (range 0 N))
              (newline))
            (range 0 N))
  (newline))

(define *current-step* (atom 0))

(define (field-ref i j)
  (vector-ref (vector-ref *field* i) j))

(define (update-to-new cell)
  (let ((old (cadr cell))
        (new (car cell)))
    (reset! old (read new))))

(define (game-of-life-new-step)
  (for-each (lambda (i)
              (for-each (lambda (j)
                          (update-to-new (field-ref i j)))
                        (range 0 N)))
            (range 0 N)))

(define (cell-alive? i j)
  (if (and (>= i 0) (>= j 0) (< i N) (< j N))
      (let* ((cell (field-ref i j))
             (v (read  (cadr cell))))
        v)
      #f))

(define (count-true l)
  (if (null? l)
      0
      (if (car l)
          (+ 1 (count-true (cdr l)))
          (count-true (cdr l)))))

(define (neighbors i j)
  (count-true (list (cell-alive? (- i 1) j) (cell-alive? (- i 1) (- j 1)) (cell-alive? (- i 1) (+ j 1))
                    (cell-alive? i (- j 1)) (cell-alive? i (+ j 1))
                    (cell-alive? (+ i 1) j) (cell-alive? (+ i 1) (- j 1)) (cell-alive? (+ i 1) (+ j 1)))))

(define (cell-live i j)
  (let* ((cell (field-ref i j))
         (ref (car cell)))
    (reset! ref #t)))

(define (cell-die i j)
  (let* ((cell (field-ref i j))
         (ref (car cell)))
    (reset! ref #f)))

(define (game-of-life-thrd fromx tox fromy toy)
  (for-each (lambda (i)
              (for-each
               (lambda (j)
                 (let ((n (neighbors i j)))
                   (if (cell-alive? i j)
                       (if (or (< n 2) (> n 3))
                           (cell-die i j)
                           (cell-live i j))
                       (if (= n 3)
                           (cell-live i j)
                           (cell-die i j)))))
               (range fromy toy)))
            (range fromx tox)))

(define (split-thrds fromx tox fromy toy max)
  (if (and (<= (- tox fromx) max) (<= (- toy fromy) max))
      (list (list fromx tox fromy toy))
      (let ((halfx (+ fromx (quotient (- tox fromx) 2)))
            (halfy (+ fromy (quotient (- toy fromy) 2))))
        (append
         (split-thrds fromx halfx fromy halfy max)
         (append
          (split-thrds fromx halfx (+ halfy 1) toy max)
          (append (split-thrds (+ halfx 1) tox fromy halfy max)
                  (split-thrds (+ halfx 1) tox (+ halfy 1) toy max)))))))

(define (game-of-life-thrds)
  (let ((thrd-bounds (split-thrds 0 N 0 N MAXthrdSIZE)))
    (map (lambda (bound) (future (game-of-life-thrd (car bound) (cadr bound) (caddr bound) (cadddr bound)))) thrd-bounds)))

(define (game-of-life-whole-step)
  (let ((thrds (game-of-life-thrds)))
    (map (lambda (t) (deref t)) thrds)
    (game-of-life-new-step)))

(define (game-of-life iterations)
  (if (= iterations 0)
      (display-field)
      (begin
        (display-field)
        (game-of-life-whole-step)
        (game-of-life (- iterations 1)))))

(game-of-life ITERATIONS)
