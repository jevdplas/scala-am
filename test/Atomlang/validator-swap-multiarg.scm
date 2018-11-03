; Source:
;  * Functional Programming Patterns in Scala and Clojure - Michael Bevilacqua-Linn (The Pragmatic Bookshelf)
;  * https://doc.lagout.org/programmation/Functional%20Programming%20Patterns%20in%20Scala%20and%20Clojure_%20Write%20Lean%20Programs%20for%20the%20JVM%20%5BBevilacqua-Linn%202013-11-02%5D.pdf (2018-10-21)
;  * http://media.pragprog.com/titles/mbfpp/code/ClojureExamples/src/mbfpp/oo/strategy/people_example.clj (2018-10-21)
; Original code:
; (defn make-cash-register []
;   (let [register (atom 0)]
;     (set-validator! register (fn [new-total] (>= new-total 0)))
;     register))

; (defn add-cash [register to-add]
;   (swap! register + to-add))

; (defn reset [register]
;   (swap! register (fn [oldval] 0)))

(define (make-cash-register)
  (let ((register (atom 0)))
    (set-validator! register (lambda (total)(>= total 0)))
    register))

(define (add-cash register amount)
  (swap! register + amount))

(define (reset register)
  (swap! register (lambda (old) 0)))