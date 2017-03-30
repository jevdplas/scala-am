
(define (compute-shortest-path graph from-label to-label)
  (define (read-out-path current)
    (cond
      ((eq? current from-label)
       (list current))
      (else
        (append
          (read-out-path
            (car (graph 'lookup-node-info current)))
          (list current)))))

  (graph 'map-over-nodes (lambda (x y)
                           (graph 'change-node-info x false)))
  
  
  (priority-first-traversal
    graph
    from-label
    (lambda (a b c d e)
      (list a (if b (cdr b) 0)
            c (if e e 0)))
    (lambda (a b c d e)
      (if e
          (/ 1 (+ e (cdr b)))
          1))
    (lambda (item1 item2)
      (eq? (caddr item1)
           (caddr item2)))
    (lambda (item) (caddr item))
    (lambda (item)
      (graph 'change-node-info
             (caddr item)
             (cons (car item)
                   (+ (cadr item)
                      (cadddr item))))))
  (read-out-path to-label))
  