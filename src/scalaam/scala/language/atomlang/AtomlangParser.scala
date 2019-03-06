package scalaam.language.atomlang

import scalaam.core.Identifier
import scalaam.language.scheme._
import scalaam.language.sexp._

/** Object that provides a method to compile an s-expression into an Atomlang (Scheme) expression. */
object AtomlangCompiler extends SchemeCompiler {
    
    /** List of reserved keywords for Atomlang. */
    override def reserved: List[String] = super.reserved ++ List("deref", "future")
    
    /** Compiles an s-expression into an Atomlang (Scheme) expression. */
    override def compile(exp: SExp): SchemeExp = exp match {
        case SExpPair(SExpId(Identifier("deref", _)), SExpPair(expr, SExpValue(ValueNil, _), _), _) => AtomlangDeref(this.compile(expr), exp.pos)
        case SExpPair(SExpId(Identifier("deref", _)), _, _) => throw new Exception(s"Invalid Atomlang deref: $exp (${exp.pos}).")
        case SExpPair(SExpId(Identifier("future", _)), body, _) => AtomlangFuture(this.compileBody(body), exp.pos)
        case SExpPair(SExpId(Identifier("swap", _)), SExpPair(atom, SExpPair(fun, args, _), _), _) => AtomlangSwap(this.compile(atom), this.compile(fun), this.compileBody(args), exp.pos)
        case SExpPair(SExpId(Identifier("swap", _)), _, _) => throw new Exception(s"Invalid Atomlang swap!: $exp (${exp.pos})")
        case _ => super.compile(exp)
    }
}

/** Object that provides a method to rename variables in a program. */
object AtomlangRenamer extends SchemeRenamer {
    
    /** Renames all variables in a program so that each variable has a unique name. */
    override def rename(exp: SchemeExp, names: AtomlangRenamer.NameMap, count: AtomlangRenamer.CountMap): (SchemeExp, AtomlangRenamer.CountMap) = exp match {

        case AtomlangDeref(exp, pos) =>
            this.rename(exp, names, count) match {
                case (exp1, count1) => (AtomlangDeref(exp1, pos), count1)
            }
        case AtomlangFuture(body, pos) =>
            this.renameList(body, names, count) match {
                case (body1, count1) => (AtomlangFuture(body1, pos), count1)
            }
//        case AtomlangSwap(atom, fun, args, pos) =>
//            this.renameList(atom :: fun :: args, names, count) match {
//                case (atom1 :: fun1 :: args1, count1) => (AtomlangSwap(atom1, fun1, args1, pos), count1)
//            }
        case _ => super.rename(exp, names, count)
    }
}

/** Object that provides a method to remove defines in a program. */
object AtomlangUndefiner extends SchemeUndefiner {
    
    import scala.util.control.TailCalls._
    
    /** Replaces defines in a program by letrec expressions. */
    override def undefineExpr(exp: SchemeExp): TailRec[SchemeExp] = exp match {
        case AtomlangDeref(exp, pos) => tailcall(this.undefine1(exp)).map(e => AtomlangDeref(e, pos))
        case AtomlangFuture(body, pos) => tailcall(this.undefineBody(body)).map(b => AtomlangFuture(b, pos))
        case AtomlangSwap(atom, fun, args, pos) =>
            tailcall(this.undefine1(atom)).flatMap(atomv =>
                tailcall(this.undefine1(fun)).flatMap(funv =>
                    this.trampolineM(this.undefine1, args).map(argsv => AtomlangSwap(atomv, funv, argsv, pos))))
        case _ => super.undefineExpr(exp)
    }
}

/** Parser for Atomlang programs. */
object AtomlangParser {
    
    /** Compiles an s-expression into an Atomlang (Scheme) expression. */
    def compile(exp: SExp): SchemeExp = AtomlangCompiler.compile(exp)
    
    /** Renames all variables in a program so that each variable has a unique name. */
    def rename(exp: SchemeExp): SchemeExp = AtomlangRenamer.rename(exp)
    
    /** Replaces defines in a program by letrec expressions. */
    def undefine(exps: List[SchemeExp]): SchemeExp = AtomlangUndefiner.undefine(exps)
    
    /** Fully parses a string representing an Atomlang program. */
    def parse(s: String): SchemeExp = undefine(SExpParser.parse(s).map(compile))
}