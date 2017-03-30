(define (depth-first-traversal graph)
  (define (is-white? graph label)
    (eq? (get-color graph label) 'white))
  (define (set-color! graph label col)
    (graph 'change-node-status label col))
  (define (get-color graph label)
    (graph 'lookup-node-status label))
  (define (make-info value predecessor t1 t2)
    (list value predecessor t1 t2))
  (define (set-value! graph label value)
    (let ((l (graph 'lookup-node-info label)))
      (graph 'change-node-info label (list value (cadr l) (caddr l) (cadddr l)))))
  (define (get-value graph label)
    (let ((l (graph 'lookup-node-info label)))
      (car l)))
  (define (set-pred! graph label pred)
    (let ((l (graph 'lookup-node-info label)))
      (graph 'change-node-info label (list (car l) pred (caddr l) (cadddr l)))))
  (Define (get-pred graph label)
    (let ((l (graph 'lookup-node-info label)))
      (cadr l)))
  (define (set-t1! graph label t1)
    (let ((l (graph 'lookup-node-info label)))
      (graph 'change-node-info label (list (car l) (cadr l) t1 (cadddr l)))))
  (define (get-t1 graph label)
    (let ((l (graph 'lookup-node-info label)))
      (caddr l)))
  (define (set-t2! graph label t2)
    (let ((l (graph 'lookup-node-info label)))
      (graph 'change-node-info label (list (car l) (cadr l) (caddr l) t2))))
  (define (get-t2 graph label)
    (let ((l (graph 'lookup-node-info label)))
      (cadddr l)))

  ;the body
  (if (graph 'empty?)
      #f
      (let ((time 0))
        (define (depth-first-visit label)
          (set-color! graph label 'gray)
          (set! time (+ time 1))
          (set-t1! graph label time)
          (graph 'foreach-neighbour label 
                 (lambda (from-label from-info to-label to-info edge-info)
                        (if (is-white? graph to-label)
                            (begin
                              (set-pred! graph to-label from-label)
                              (depth-first-visit to-label)))))
          (set-color! graph label 'black)
          (set! time (+ time 1))
          (set-t2! graph label time)
        (graph 'foreach-node (lambda (label info) 
                               (graph 'change-node-info label (make-info #f '() '()'()))
                               (set-color! graph label 'white)))
        (graph 'foreach-node (lambda (label info) 
                               (if (is-white? graph label)
                                   (depth-first-visit label)))))))

(define (test-dfs graph)
  (depth-first-traversal graph))

