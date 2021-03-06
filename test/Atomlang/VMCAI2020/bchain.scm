;; Adapted from https://medium.com/@lhartikk/a-blockchain-in-200-lines-of-code-963cc1cc0e54
;; Adapted from https://medium.com/@lhartikk/a-blockchain-in-200-lines-of-code-963cc1cc0e54
(define (block index prev-hash timestamp data hash)
  (list 'block index prev-hash timestamp data hash))

(define (range a b)
  (letrec ((loop (lambda (i acc)
                   (if (< i a)
                     acc
                     (loop (- i 1) (cons i acc))))))
    (loop (- b 1) '())))

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

(define (block-index b)
  (cadr b))
(define (block-prev-hash b)
  (caddr b))
(define (block-timestamp b)
  (cadddr b))
(define (block-data b)
  (cadddr (cdr b)))
(define (block-hash b)
  (cadddr (cddr b)))

(define foldl
  (lambda (f base lst)
    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))
    (foldl-aux base lst)))

(define (calculate-hash index prev-hash timestamp data)
  (foldl + (+ index prev-hash timestamp) data))

(define (calculate-hash-for-block block)
  (calculate-hash (block-index block) (block-prev-hash block)
    (block-timestamp block) (block-data block)))

(define (current-timestamp) 42)

(define (generate-next-block data previous-block)
  (let* ((next-index (+ (block-index previous-block) 1))
          (next-timestamp (current-timestamp))
          (next-hash (calculate-hash next-index (block-hash previous-block)
                       next-timestamp data)))
    (block next-index (block-hash previous-block) next-timestamp data next-hash)))

(define *blockchain*
  (atom (list
         (block 0 0 1509658251 '(0) ;; "genesis block"
                (calculate-hash 0 0 1598658251 '(0))))))

(define (show-blockchain)
  (for-each (lambda (b) (display (block-data b)) (display " "))
    (read *blockchain*))
  (newline))

(define (get-latest-block)
  (car (read *blockchain*)))

(define (is-valid-new-block new-block previous-block)
  (and (= (+ 1 (block-index previous-block)) (block-index new-block))
    (equal? (block-hash previous-block) (block-prev-hash new-block))
    (equal? (calculate-hash-for-block new-block) (block-hash new-block))))

(define (add-block b)
  (let add ((chain (read *blockchain*)))
    (if (is-valid-new-block b (car chain))
      (if (compare-and-set! *blockchain* chain (cons b chain))
        #t
        (add (read *blockchain*)))
      #f)))

(define (mine-new-block data)
  (let ((latest-block (car (read *blockchain*))))
    (generate-next-block data latest-block)))

(define (miner n data)
  (if (= n 0)
      'done
    (let ((new-block (mine-new-block data)))
      (if (add-block new-block)
        (miner (- n 1) data)
        (miner n data)))))

(show-blockchain)
(define NMiners (random 10))
(define NBlocksPerMiner (random 10))
(define miner-data (list '(1) '(2) '(3) '(4) '(5) '(6) '(7) '(8) '(9) '(10)))
(define miners
  (map (lambda (i) (future
                     (miner NBlocksPerMiner
                       (list-ref miner-data (modulo i (length miner-data))))))
       (range 0 NMiners)))
(define extra-miner1 (future (miner NBlocksPerMiner '(1 10 15))))
(define extra-miner2 (future (miner NBlocksPerMiner '(28 7 6))))
(map (lambda (t) (deref t)) miners)
(deref extra-miner1)
(deref extra-miner2)
(define miners2
  (map (lambda (i)
         (future
          (miner NBlocksPerMiner
                 (list-ref miner-data (modulo i (length miner-data))))))
       (range 0 NMiners)))
(map (lambda (t) (deref t)) miners2)
(show-blockchain)
