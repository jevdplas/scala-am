package scalaam.language.atomlang

import scalaam.core.Position
import scalaam.language.scheme.SchemeExp

/*
/** Atom creation: (atom v) */
case class AtomlangAtom(exp: SchemeExp, pos: Position) extends SchemeExp {
    override def toString: String = {
        s"(atom $exp)"
    }
}
*/

/** Dereferencing of a future or of an atom: (deref v) */
case class AtomlangDeref(exp: SchemeExp, pos: Position) extends SchemeExp {
    override def toString: String = {
        s"(deref $exp)"
    }
}

/** Future creation: (future e) */
case class AtomlangFuture(exps: List[SchemeExp], pos: Position) extends SchemeExp {
    override def toString = {
        val body = exps.mkString(" ")
        s"(future $body)"
    }
}
