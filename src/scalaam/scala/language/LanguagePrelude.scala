package scalaam.language

object LanguagePrelude {
  val atomlangPrelude: String =
    """(define (swap! at fn)
          |  (let ((vl (read at)))
          |    (if (not (compare-and-set! at vl (fn vl)))
          |      (swap! at fn))))""".stripMargin

  object Prelude extends Enumeration {
    type Prelude = Value
    val lock, list, scme, none = Value
  }

  def selectPrelude(p: Prelude.Value): String = p match {
      case Prelude.lock => lockPrelude
      case Prelude.list => listPrelude
      case Prelude.scme => schemePrelude
      case Prelude.none => ""
  }

  // Lock implementation by means of atoms.
  val lockPrelude: String =
    """(define (t/new-lock)
        |  (atom #f))
        |(define (t/acquire lock)
        |  (let try ()
        |    (if (compare-and-set! lock #f #t)
        |        #t
        |        (try))))
        |(define (t/release lock)
        |  (reset! lock #f))""".stripMargin

  // Implementation of two basic list primitives.
  val listPrelude: String =
    """(define (map f l)
        |  (if (null? l)
        |      '()
        |      (cons (f (car l))
        |            (map f (cdr l)))))
        |(define (for-each f l)
        |  (if (not (null? l))
        |      (begin (f (car l))
        |             (for-each f (cdr l)))))""".stripMargin

  // Mimicking of atoms and futures for standard Scheme.
  val schemePrelude: String =
    """(define atom list)
      |(define read car)
      |(define (future x) x)
      |(define (deref x) x)
      |(define (compare-and-set! x old new)
      |  (if (eq? (car x) old)
      |      (set-car! x new)))(define reset! set-car!)
      |(define (swap! x f)(set-car! x (f (car x))))""".stripMargin
}
