; Source:
;  * Functional Programming Patterns in Scala and Clojure - Michael Bevilacqua-Linn (The Pragmatic Bookshelf)
;  * https://doc.lagout.org/programmation/Functional%20Programming%20Patterns%20in%20Scala%20and%20Clojure_%20Write%20Lean%20Programs%20for%20the%20JVM%20%5BBevilacqua-Linn%202013-11-02%5D.pdf (2018-10-21)
;  * http://media.pragprog.com/titles/mbfpp/code/ClojureExamples/src/mbfpp/oo/strategy/people_example.clj (2018-10-12)
; Original code:
; (defn person-collector [valid?]
;   (let [valid-people (atom [])]
;     (fn [person]
;         (if (valid? person)
;             (swap! valid-people conj person))
;         @valid-people)))


(define (make-collector valid?)
  (let ((validated (atom '())))
    (lambda (item)
      (if (valid? item)
          (swap! validated (lambda (old)(cons item old))))
      (read validated))))

(define c (make-collector string?))
(c "Humans")
(c "Dolphines")
(c 123)
(c #t)
(equal? (c "Chickens") '("Chickens" "Dolphines" "Humans"))
