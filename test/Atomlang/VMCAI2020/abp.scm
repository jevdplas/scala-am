;; Inspired from http://alexsclassblogg.blogspot.be/2013/11/python-alternating-bit-socket.html

(define (t/new-lock)
  (atom #f))
(define (t/acquire lock)
  (let try ()
    (if (compare-and-set! lock #f #t)
        #t
        (try))))
(define (t/release lock)
  (reset! lock #f))

(define (build-vector n init f)
  (letrec ((v (make-vector n init))
           (loop (lambda (i)
                   (if (< i n)
                       (begin
                         (vector-set! v i (f i))
                         (loop (+ i 1)))
                       v))))
    (loop 0)))

(define CLOSING -1)
(define (server data in in-lock
                out out-lock
                seqnumber nexttosend)
  (let ((v (begin
             (t/acquire in-lock)
             (let ((r (read in)))
               (reset! in #f)
               (t/release in-lock)
               r))))
    ;(sleep 1)
    (if v
        (begin
          ;; received something
          (if (= v CLOSING)
              'done
              (if (= v seqnumber)
                  ;; expecting what we were
                  (let ((seqnumber2 (if (= seqnumber 0) 1 0))
                        (nexttosend2 (+ nexttosend 1)))
                    (t/acquire out-lock)
                    (reset! out (cons (vector-ref data nexttosend2) seqnumber2))
                    (t/release out-lock)
                    (server data in in-lock
                            out out-lock
                            seqnumber2 nexttosend2))
                  ;; Not expecting
                  (let ((seqnumber2 (if (= seqnumber 0) 1 0))
                        (nexttosend2 (- nexttosend 1)))
                    (t/acquire out-lock)
                    (reset! out (cons (vector-ref data nexttosend2) seqnumber2))
                    (t/release out-lock)
                    (server data in in-lock
                            out out-lock
                            seqnumber2 nexttosend2)))))
        ;; not received anything
        (server data in in-lock
                out out-lock
                seqnumber nexttosend))))

(define (client i in in-lock
                out out-lock
                ack)
  (let ((v (begin
             (t/acquire in-lock)
             (let ((r (read in)))
               (reset! in #f)
               (t/release in-lock)
               r))))
    (if v
        (begin
          ;; received something
          (if (= (cdr v) ack)
              (let ((ack2 (if (= ack 0) 1 0)))
                (if (>= (+ i 1) N)
                    (begin
                      (t/acquire out-lock)
                      (reset! out CLOSING)
                      (t/release out-lock))
                    (begin
                      (t/acquire out-lock)
                      (reset! out ack)
                      (t/release out-lock)
                      (client (+ i 1) in in-lock
                              out out-lock
                              ack2))))
              (begin
                (t/acquire out-lock)
                (reset! out ack)
                (t/release out-lock)
                (client i in in-lock
                        out out-lock
                        ack))))
        ;; not received anything
          (client i in in-lock
                  out out-lock
                  ack))))

(define N (random 5))
(define data-to-send (build-vector N 0 (lambda (i) (random 100))))
(define data-to-send2 (build-vector N 0 (lambda (i) (random 100))))
(define client->server (atom 0))
(define client->server2 (atom 0))
(define client->server2-lock (t/new-lock))
(define client->server-lock (t/new-lock))
(define server->client (atom #f))
(define server->client2 (atom #f))
(define server->client-lock (t/new-lock))
(define server->client2-lock (t/new-lock))
(define s (future (server data-to-send client->server client->server-lock
                          server->client server->client-lock
                          0 -1)))
(define c (future (client 0 server->client server->client-lock
                          client->server client->server-lock
                          1)))
(define s2 (future (server data-to-send2 client->server2 client->server2-lock
                          server->client2 server->client2-lock
                          0 -1)))
(define c2 (future (client 0 server->client2 server->client2-lock
                          client->server2 client->server2-lock
                          1)))

(deref s)
(deref c)
(deref s2)
(deref c2)

(define s (future (server data-to-send client->server client->server-lock
                          server->client server->client-lock
                          0 -1)))
(define c (future (client 0 server->client server->client-lock
                          client->server client->server-lock
                          1)))
(define s2 (future (server data-to-send2 client->server2 client->server2-lock
                          server->client2 server->client2-lock
                          0 -1)))
(define c2 (future (client 0 server->client2 server->client2-lock
                          client->server2 client->server2-lock
                          1)))
(deref s)
(deref c)
(deref s2)
(deref c2)
