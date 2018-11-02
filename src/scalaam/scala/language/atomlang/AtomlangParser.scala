package scalaam.language.atomlang

import scalaam.core.Identifier
import scalaam.language.scheme._
import scalaam.language.sexp._

object AtomlangCompiler extends SchemeCompiler {

    override val reserved: List[String] = reserved ++ List("deref", "future")

    override def compile(exp: SExp): SchemeExp = exp match {
      case SExpPair(SExpId(Identifier("deref", _)), SExpPair(expr, SExpValue(ValueNil, _), _), _) => AtomlangDeref(compile(expr), exp.pos)
      case SExpPair(SExpId(Identifier("deref", _)), _, _) => throw new Exception(s"Invalid Atomlang deref: $exp (${exp.pos}).")

      case SExpPair(SExpId(Identifier("future", _)), body, _) => AtomlangFuture(compileBody(body), exp.pos)

      case _ => super.compile(exp)
    }
}

object AtomlangRenamer extends SchemeRenamer {

    override def rename(exp: SchemeExp, names: AtomlangRenamer.NameMap, count: AtomlangRenamer.CountMap): (SchemeExp, AtomlangRenamer.CountMap) = exp match {
    
      case AtomlangDeref(exp, pos) =>
          rename(exp, names, count) match {
              case (exp1, count1) => (AtomlangDeref(exp1, pos), count1)
          }
      case AtomlangFuture(body, pos) =>
          renameList(body, names, count) match {
              case (body1, count1) => (AtomlangFuture(body1, pos), count1)
          }
      case _ => super.rename(exp, names, count)
    }
}

object AtomlangUndefiner extends SchemeUndefiner {
    import scala.util.control.TailCalls._
    
    override def undefineExpr(exp: SchemeExp): TailRec[SchemeExp] = exp match {
        case AtomlangDeref(exp, pos) => tailcall(undefine1(exp)).map(e => AtomlangDeref(e, pos))
        case AtomlangFuture(body, pos) => tailcall(undefineBody(body)).map(b => AtomlangFuture(b, pos))
        case _ => super.undefineExpr(exp)
    }
}

object AtomlangParser {
    
    def compile(exp: SExp): SchemeExp = AtomlangCompiler.compile(exp)
    def rename(exp: SchemeExp): SchemeExp = AtomlangRenamer.rename(exp)
    def undefine(exps: List[SchemeExp]): SchemeExp = AtomlangUndefiner.undefine(exps)
    def parse(s: String): SchemeExp = undefine(SExpParser.parse(s).map(compile _))
}