;;; Taken from https://github.com/acieroid/abstract-benchmarks/blob/master/scm-big/conform.scm
;;; Expected result: #t
;;; CONFORM -- Type checker, written by Jim Miller.

(let ((sort-list (lambda (obj pred)
                   (letrec ((merge (lambda (one two)
                                     (cond ((null? one) two)
                                           ((pred (car two) (car one))
                                            (cons (car two)
                                                  (merge (cdr two) one)))
                                           (else
                                            (cons (car one)
                                                  (merge (cdr one) two)))))))
                     (let ((loop #f))
                       (letrec ((split-list (lambda (l one two)
                                              (if (pair? l)
                                                  (split-list (cdr l) two (cons (car l) one))
                                                  (merge (loop one) (loop two))))))

                         (set! loop (lambda (l)
                                      (if (and (pair? l) (pair? (cdr l)))
                                          (split-list l '() '())
                                          l)))
                         (loop obj)))))))

  (let ((adjoin (lambda (element set)
                  (if (memq element set) set (cons element set)))))

    (letrec ((eliminate (lambda (element set)
                          (cond ((null? set) set)
                                ((eq? element (car set)) (cdr set))
                                (else (cons (car set) (eliminate element (cdr set))))))))

      (let ((intersect (lambda (list1 list2)
                         (letrec ((loop (lambda (l)
                                          (cond ((null? l) '())
                                                ((memq (car l) list2) (cons (car l) (loop (cdr l))))
                                                (else (loop (cdr l)))))))
                           (loop list1)))))

        (letrec ((union (lambda (list1 list2)
                          (if (null? list1)
                              list2
                              (union (cdr list1)
                                     (adjoin (car list1) list2))))))

          (let ((make-internal-node (lambda (name green red blue)
                                      (cons name
                                            (cons green
                                                  (cons red
                                                        blue)))))
                (internal-node-name (lambda (node) (car node)))
                (internal-node-green-edges (lambda (node) (cadr node)))
                (internal-node-red-edges (lambda (node) (caddr node)))
                (internal-node-blue-edges (lambda (node) (cdddr node)))
                (set-internal-node-name! (lambda (node name) (set-car! node name)))
                (set-internal-node-green-edges! (lambda (node edges) (set-car! (cdr node) edges)))
                (set-internal-node-red-edges! (lambda (node edges) (set-car! (cddr node) edges)))
                (set-internal-node-blue-edges! (lambda (node edges) (set-cdr! (cddr node) edges))))
            (let ((make-node (lambda (name blue-edges)
                               (let ((name (if (symbol? name) (symbol->string name) name))
                                     (blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (car blue-edges))))
                                 (make-internal-node name '() '() blue-edges))))
                  (copy-node (lambda (node)
                               (make-internal-node (internal-node-name node) '() '() (internal-node-blue-edges node)))))
              (let ((none-node (make-node 'none (cons #t '())))
                    (any-node (make-node 'any (cons '() '()))))
                (let ((none-node? (lambda (node) (eq? node none-node)))
                      (any-node? (lambda (node) (eq? node any-node))))
                  (let ((name internal-node-name)
                        (make-edge-getter (lambda (selector)
                                            (lambda (node)
                                              (if (or (none-node? node) (any-node? node))
                                                  (error "Can't get edges from the ANY or NONE nodes")
                                                  (selector node)))))
                        (make-edge-setter (lambda (mutator!)
                                            (lambda (node value)
                                              (cond ((any-node? node) (error "Can't set edges from the ANY node"))
                                                    ((none-node? node) 'OK)
                                                    (else (mutator! node value)))))))


                    (let ((red-edges (make-edge-getter internal-node-red-edges))
                          (green-edges (make-edge-getter internal-node-green-edges))
                          (blue-edges (make-edge-getter internal-node-blue-edges))
                          (set-red-edges! (make-edge-setter set-internal-node-red-edges!))
                          (set-green-edges! (make-edge-setter set-internal-node-green-edges!))
                          (set-blue-edges! (make-edge-setter set-internal-node-blue-edges!)))

                      (let ((make-blue-edge (lambda (op arg res) (cons op (cons arg res))))
                            (blue-edge-operation (lambda (edge) (car edge)))
                            (blue-edge-arg-node (lambda (edge) (cadr edge)))
                            (blue-edge-res-node (lambda (edge) (cddr edge)))
                            (set-blue-edge-operation! (lambda (edge value) (set-car! edge value)))
                            (set-blue-edge-arg-node! (lambda (edge value) (set-car! (cdr edge) value)))
                            (set-blue-edge-res-node! (lambda (edge value) (set-cdr! (cdr edge) value))))

                        (let ((operation blue-edge-operation)
                              (arg-node blue-edge-arg-node)
                              (res-node blue-edge-res-node)
                              (set-arg-node! set-blue-edge-arg-node!)
                              (set-res-node! set-blue-edge-res-node!))
                          (letrec ((lookup-op (lambda (op node)
                                                (letrec ((loop (lambda (edges)
                                                                 (cond ((null? edges) '())
                                                                       ((eq? op (operation (car edges))) (car edges))
                                                                       (else (loop (cdr edges)))))))
                                                  (loop (blue-edges node))))))
                            (let ((has-op? (lambda (op node) (not (null? (lookup-op op node))))))
                              (let ((make-internal-graph (lambda (nodes met joined) (cons nodes (cons met joined))))
                                    (internal-graph-nodes (lambda (graph) (car graph)))
                                    (internal-graph-already-met (lambda (graph) (cadr graph)))
                                    (internal-graph-already-joined (lambda (graph) (cddr graph)))
                                    (set-internal-graph-nodes! (lambda (graph nodes) (set-car! graph nodes))))
                                (let ((make-empty-table (lambda () (cons 'TABLE '())))
                                      (lookup (lambda (table x y)
                                                (let ((one (assq x (cdr table))))
                                                  (if one
                                                      (let ((two (assq y (cdr one))))
                                                        (if two (cdr two) #f))
                                                      #f))))
                                      (insert! (lambda (table x y value)
                                                 (let ((make-singleton-table (lambda (x y)
                                                                               (cons (cons x y) '()))))
                                                   (let ((one (assq x (cdr table))))
                                                     (if one
                                                         (set-cdr! one (cons (cons y value) (cdr one)))
                                                         (set-cdr! table (cons (cons x (make-singleton-table y value))
                                                                               (cdr table)))))))))
                                  (let ((make-graph (lambda (nodes)
                                                      (make-internal-graph nodes (make-empty-table) (make-empty-table))))
                                        (graph-nodes internal-graph-nodes)
                                        (already-met internal-graph-already-met)
                                        (already-joined internal-graph-already-joined)
                                        (add-graph-nodes! (lambda (graph nodes)
                                                            (set-internal-graph-nodes! graph (cons nodes (internal-graph-nodes graph))))))
                                    (let ((find-canonical-representative (lambda (element classification)
                                                                           (letrec ((loop (lambda (classes)
                                                                                            (cond ((null? classes) (error "Can't classify" element))
                                                                                                  ((memq element (car classes)) (car (car classes)))
                                                                                                  (else (loop (cdr classes)))))))
                                                                             (loop classification)))))

                                      (let ((copy-graph (lambda (g)
                                                          (let ((copy-list (lambda (l) (map (lambda (x) x) l))))
                                                            (make-internal-graph
                                                             (copy-list (graph-nodes g))
                                                             (already-met g)
                                                             (already-joined g)))))
                                            (clean-graph (lambda (g)
                                                           (let ((clean-node (lambda (node)
                                                                               (if (not (or (any-node? node) (none-node? node)))
                                                                                   (begin
                                                                                     (set-green-edges! node '())
                                                                                     (set-red-edges! node '()))))))
                                                             (for-each clean-node (graph-nodes g))
                                                             g)))
                                            (canonicalize-graph (lambda (graph classes)
                                                                  (let ((fix (lambda (node)
                                                                               (let ((fix-set (lambda (object selector mutator)
                                                                                                (mutator object
                                                                                                         (map (lambda (node)
                                                                                                                (find-canonical-representative node classes))
                                                                                                              (selector object))))))
                                                                                 (if (not (or (none-node? node) (any-node? node)))
                                                                                     (begin
                                                                                       (fix-set node green-edges set-green-edges!)
                                                                                       (fix-set node red-edges set-red-edges!)
                                                                                       (for-each
                                                                                        (lambda (blue-edge)
                                                                                          (set-arg-node! blue-edge
                                                                                                         (find-canonical-representative (arg-node blue-edge) classes))
                                                                                          (set-res-node! blue-edge
                                                                                                         (find-canonical-representative (res-node blue-edge) classes)))
                                                                                        (blue-edges node))))
                                                                                 node)))
                                                                        (fix-table (lambda (table)
                                                                                     (let ((canonical? (lambda (node) (eq? node (find-canonical-representative node classes))))
                                                                                           (filter-and-fix (lambda (predicate-fn update-fn list)
                                                                                                             (letrec ((loop  (lambda (list)
                                                                                                                               (cond ((null? list) '())
                                                                                                                                     ((predicate-fn (car list))
                                                                                                                                      (cons (update-fn (car list)) (loop (cdr list))))
                                                                                                                                     (else (loop (cdr list)))))))
                                                                                                               (loop list)))))
                                                                                       (let ((fix-line (lambda (line)
                                                                                                         (filter-and-fix
                                                                                                          (lambda (entry) (canonical? (car entry)))
                                                                                                          (lambda (entry) (cons (car entry)
                                                                                                                                (find-canonical-representative (cdr entry) classes)))
                                                                                                          line))))
                                                                                         (if (null? table)
                                                                                             '()
                                                                                             (cons (car table)
                                                                                                   (filter-and-fix
                                                                                                    (lambda (entry) (canonical? (car entry)))
                                                                                                    (lambda (entry) (cons (car entry) (fix-line (cdr entry))))
                                                                                                    (cdr table)))))))))
                                                                    (make-internal-graph
                                                                     (map (lambda (class) (fix (car class))) classes)
                                                                     (fix-table (already-met graph))
                                                                     (fix-table (already-joined graph)))))))
                                        (let ((green-edge? (lambda (from-node to-node)
                                                             (cond ((any-node? from-node) #f)
                                                                   ((none-node? from-node) #t)
                                                                   ((memq to-node (green-edges from-node)) #t)
                                                                   (else #f))))

                                              (red-edge? (lambda (from-node to-node)
                                                           (cond ((any-node? from-node) #f)
                                                                 ((none-node? from-node) #t)
                                                                 ((memq to-node (red-edges from-node)) #t)
                                                                 (else #f)))))
                                          (let ((sig
                                                 (let ((none-comma-any (cons none-node any-node)))
                                                   (lambda (op node)
                                                     (let ((the-edge (lookup-op op node)))
                                                       (if (not (null? the-edge))
                                                           (cons (arg-node the-edge) (res-node the-edge))
                                                           none-comma-any)))))
                                                (arg (lambda (pair) (car pair)))
                                                (res (lambda (pair) (cdr pair))))
                                            (let ((conforms? (lambda (t1 t2)
                                                               (let ((nodes-with-red-edges-out '()))
                                                                 (let ((add-red-edge! (lambda (from-node to-node)
                                                                                        (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
                                                                                        (set! nodes-with-red-edges-out
                                                                                              (adjoin from-node nodes-with-red-edges-out))))
                                                                       (greenify-red-edges! (lambda (from-node)
                                                                                              (set-green-edges! from-node
                                                                                                                (append (red-edges from-node) (green-edges from-node)))
                                                                                              (set-red-edges! from-node '())))
                                                                       (delete-red-edges! (lambda (from-node) (set-red-edges! from-node '()))))
                                                                   (letrec ((does-conform (lambda (t1 t2)
                                                                                            (cond ((or (none-node? t1) (any-node? t2)) #t)
                                                                                                  ((or (any-node? t1) (none-node? t2)) #f)
                                                                                                  ((green-edge? t1 t2) #t)
                                                                                                  ((red-edge? t1 t2) #t)
                                                                                                  (else
                                                                                                   (add-red-edge! t1 t2)
                                                                                                   (letrec ((loop (lambda (blues)
                                                                                                                    (if (null? blues)
                                                                                                                        #t
                                                                                                                        (let* ((current-edge (car blues))
                                                                                                                               (phi (operation current-edge)))
                                                                                                                          (and (has-op? phi t1)
                                                                                                                               (does-conform
                                                                                                                                (res (sig phi t1))
                                                                                                                                (res (sig phi t2)))
                                                                                                                               (does-conform
                                                                                                                                (arg (sig phi t2))
                                                                                                                                (arg (sig phi t1)))
                                                                                                                               (loop (cdr blues))))))))
                                                                                                     (loop (blue-edges t2))))))))
                                                                     (let ((result (does-conform t1 t2)))
                                                                       (for-each (if result greenify-red-edges! delete-red-edges!)
                                                                                 nodes-with-red-edges-out)
                                                                       result)))))))
                                              (let ((equivalent? (lambda (a b) (and (conforms? a b) (conforms? b a)))))
                                                (let ((classify (lambda (nodes)
                                                                  (letrec ((node-loop (lambda (classes nodes)
                                                                                        (if (null? nodes)
                                                                                            (map (lambda (class)
                                                                                                   (sort-list class
                                                                                                              (lambda (node1 node2)
                                                                                                                (< (string-length (name node1))
                                                                                                                   (string-length (name node2))))))
                                                                                                 classes)
                                                                                            (let ((this-node (car nodes)))
                                                                                              (letrec ((add-node (lambda (classes)
                                                                                                                   (cond ((null? classes) (cons (cons this-node '()) '()))
                                                                                                                         ((equivalent? this-node (caar classes))
                                                                                                                          (cons (cons this-node (car classes))
                                                                                                                                (cdr classes)))
                                                                                                                         (else (cons (car classes)
                                                                                                                                     (add-node (cdr classes))))))))
                                                                                                (node-loop (add-node classes)
                                                                                                           (cdr nodes))))))))
                                                                    (node-loop '() nodes)))))
                                                  (let ((reduce (lambda (graph)
                                                                  (let ((classes (classify (graph-nodes graph))))
                                                                    (canonicalize-graph graph classes))))
                                                        (blue-edge-operate (lambda (arg-fn res-fn graph op sig1 sig2)
                                                                             (make-blue-edge op
                                                                                             (arg-fn graph (arg sig1) (arg sig2))
                                                                                             (res-fn graph (res sig1) (res sig2))))))

                                                    (let ((join #f))
                                                      (letrec ((meet (lambda (graph node1 node2)
                                                                       (cond ((eq? node1 node2) node1)
                                                                             ((or (any-node? node1) (any-node? node2)) any-node)
                                                                             ((none-node? node1) node2)
                                                                             ((none-node? node2) node1)
                                                                             ((lookup (already-met graph) node1 node2))
                                                                             ((conforms? node1 node2) node2)
                                                                             ((conforms? node2 node1) node1)
                                                                             (else
                                                                              (let ((result
                                                                                     (make-node (string-append "(" (name node1) " ^ " (name node2) ")") '())))
                                                                                (add-graph-nodes! graph result)
                                                                                (insert! (already-met graph) node1 node2 result)
                                                                                (set-blue-edges! result
                                                                                                 (map
                                                                                                  (lambda (op)
                                                                                                    (blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
                                                                                                  (intersect (map operation (blue-edges node1))
                                                                                                             (map operation (blue-edges node2)))))
                                                                                result))))))
                                                        (set! join (lambda (graph node1 node2)
                                                                     (cond ((eq? node1 node2) node1)
                                                                           ((any-node? node1) node2)
                                                                           ((any-node? node2) node1)
                                                                           ((or (none-node? node1) (none-node? node2)) none-node)
                                                                           ((lookup (already-joined graph) node1 node2))
                                                                           ((conforms? node1 node2) node1)
                                                                           ((conforms? node2 node1) node2)
                                                                           (else
                                                                            (let ((result
                                                                                   (make-node (string-append "(" (name node1) " v " (name node2) ")") '())))
                                                                              (add-graph-nodes! graph result)
                                                                              (insert! (already-joined graph) node1 node2 result)
                                                                              (set-blue-edges! result
                                                                                               (map
                                                                                                (lambda (op)
                                                                                                  (blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
                                                                                                (union (map operation (blue-edges node1))
                                                                                                       (map operation (blue-edges node2)))))
                                                                              result)))))
                                                        (let ((make-lattice (lambda (g print?)
                                                                              (let ((step (lambda (g)
                                                                                            (let* ((copy (copy-graph g))
                                                                                                   (nodes (graph-nodes copy)))
                                                                                              (for-each (lambda (first)
                                                                                                          (for-each (lambda (second)
                                                                                                                      (meet copy first second) (join copy first second))
                                                                                                                    nodes))
                                                                                                        nodes)
                                                                                              copy))))
                                                                                (letrec ((loop (lambda (g count)
                                                                                                 (if print? (display count))
                                                                                                 (let ((lattice (step g)))
                                                                                                   (if print? (begin (display " -> ") (display (length (graph-nodes lattice)))))
                                                                                                   (let* ((new-g (reduce lattice))
                                                                                                          (new-count (length (graph-nodes new-g))))
                                                                                                     (if (= new-count count)
                                                                                                         (begin
                                                                                                           (if print? (newline))
                                                                                                           new-g)
                                                                                                         (begin
                                                                                                           (if print? (begin (display " -> ") (display new-count) (newline)))
                                                                                                           (loop new-g new-count))))))))
                                                                                  (let ((graph
                                                                                         (make-graph
                                                                                          (adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
                                                                                    (loop graph (length (graph-nodes graph)))))))))
                                                          (let ((a '())
                                                                (b '())
                                                                (c '())
                                                                (d '()))
                                                            (let ((setup (lambda ()
                                                                           (set! a (make-node 'a '()))
                                                                           (set! b (make-node 'b '()))
                                                                           (set-blue-edges! a (cons (make-blue-edge 'phi any-node b) '()))
                                                                           (set-blue-edges! b (cons (make-blue-edge 'phi any-node a)
                                                                                                    (cons (make-blue-edge 'theta any-node b) '())))
                                                                           (set! c (make-node "c" '()))
                                                                           (set! d (make-node "d" '()))
                                                                           (set-blue-edges! c (cons (make-blue-edge 'theta any-node b) '()))
                                                                           (set-blue-edges! d (cons (make-blue-edge 'phi any-node c)
                                                                                                    (cons (make-blue-edge 'theta any-node d) '())))
                                                                           #t)))
                                                              (let ((test (lambda ()
                                                                            (setup)
                                                                            (map name
                                                                                 (graph-nodes (make-lattice (make-graph (cons a
                                                                                                                              (cons b
                                                                                                                                    (cons c
                                                                                                                                          (cons d
                                                                                                                                                (cons any-node (cons none-node '()))))))) #f))))))
                                                                (let ((result                                                                       '("(((b v d) ^ a) v c)"
                                                                                                                                                      "(c ^ d)"
                                                                                                                                                      "(b v (a ^ d))"
                                                                                                                                                      "((a v d) ^ b)"
                                                                                                                                                      "(b v d)"
                                                                                                                                                      "(b ^ (a v c))"
                                                                                                                                                      "(a v (c ^ d))"
                                                                                                                                                      "((b v d) ^ a)"
                                                                                                                                                      "(c v (a v d))"
                                                                                                                                                      "(a v c)"
                                                                                                                                                      "(d v (b ^ (a v c)))"
                                                                                                                                                      "(d ^ (a v c))"
                                                                                                                                                      "((a ^ d) v c)"
                                                                                                                                                      "((a ^ b) v d)"
                                                                                                                                                      "(((a v d) ^ b) v (a ^ d))"
                                                                                                                                                      "(b ^ d)"
                                                                                                                                                      "(b v (a v d))"
                                                                                                                                                      "(a ^ c)"
                                                                                                                                                      "(b ^ (c v d))"
                                                                                                                                                      "(a ^ b)"
                                                                                                                                                      "(a v b)"
                                                                                                                                                      "((a ^ d) ^ b)"
                                                                                                                                                      "(a ^ d)"
                                                                                                                                                      "(a v d)"
                                                                                                                                                      "d"
                                                                                                                                                      "(c v d)"
                                                                                                                                                      "a"
                                                                                                                                                      "b"
                                                                                                                                                      "c"
                                                                                                                                                      "any"
                                                                                                                                                      "none")))
                                                                  (equal? (test) result))))))))))))))))))))))))))))))))))

