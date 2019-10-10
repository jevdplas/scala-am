package scalaam.language.scheme

import scalaam.core.{Position, Identifier}
import scalaam.language.sexp._

/**
  * Object that provides a method to compile an s-expression into a Scheme expression
  */
trait SchemeCompiler {
  class SchemeCompilerException(reason: String, position: Position) extends Exception(reason)
  import scala.util.control.TailCalls._

  /**
    * Reserved keywords
    */
  def reserved: List[String] =
    List("lambda", "if", "let", "let*", "letrec", "cond", "case", "set!", "begin", "define", "do")

  // TODO update compile so it returns a TailRec
  def compile(exp: SExp): SchemeExp = this._compile(exp).result

  def _compile(exp: SExp): TailRec[SchemeExp] = exp match {
    case SExpPair(SExpId(Identifier("quote", _)), SExpPair(quoted, SExpValue(ValueNil, _), _), _) =>
      _compile(SExpQuoted(quoted, exp.pos))
    case SExpPair(SExpId(Identifier("quote", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme quote: $exp", exp.pos)
    case SExpPair(
        SExpId(Identifier("lambda", _)),
        SExpPair(args, SExpPair(first, rest, _), _),
        _
        ) =>
      for {
        argsv  <- tailcall(this.compileArgs(args))
        firstv <- tailcall(this._compile(first))
        restv  <- tailcall(this.compileBody(rest))
      } yield SchemeLambda(argsv, firstv :: restv, exp.pos)
    case SExpPair(SExpId(Identifier("lambda", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme lambda: $exp", exp.pos)
    case SExpPair(
        SExpId(Identifier("if", _)),
        SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil, _), _), _), _),
        _
        ) =>
      for {
        condv <- tailcall(this._compile(cond))
        consv <- tailcall(this._compile(cons))
        altv  <- tailcall(this._compile(alt))
      } yield SchemeIf(condv, consv, altv, exp.pos)
    case SExpPair(
        SExpId(Identifier("if", _)),
        SExpPair(cond, SExpPair(cons, SExpValue(ValueNil, _), _), _),
        _
        ) =>
      /* Empty else branch is replaced by #f (R5RS states it's unspecified) */
      for {
        condv <- tailcall(this._compile(cond))
        consv <- tailcall(this._compile(cons))
      } yield SchemeIf(condv, consv, SchemeValue(ValueBoolean(false), exp.pos), exp.pos)
    case SExpPair(SExpId(Identifier("if", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme if: $exp", exp.pos)
    case SExpPair(
        SExpId(Identifier("let", _)),
        SExpPair(SExpId(name), SExpPair(bindings, SExpPair(first, rest, _), _), _),
        _
        ) =>
      for {
        bindingsv <- tailcall(this.compileBindings(bindings))
        firstv    <- tailcall(this._compile(first))
        restv     <- tailcall(this.compileBody(rest))
      } yield SchemeNamedLet(name, bindingsv, firstv :: restv, exp.pos)
    case SExpPair(
        SExpId(Identifier("let", _)),
        SExpPair(bindings, SExpPair(first, rest, _), _),
        _
        ) =>
      for {
        bindingsv <- tailcall(this.compileBindings(bindings))
        firstv    <- tailcall(this._compile(first))
        restv     <- tailcall(this.compileBody(rest))
      } yield SchemeLet(bindingsv, firstv :: restv, exp.pos)
    case SExpPair(SExpId(Identifier("let", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme let: $exp", exp.pos)
    case SExpPair(
        SExpId(Identifier("let*", _)),
        SExpPair(bindings, SExpPair(first, rest, _), _),
        _
        ) =>
      for {
        bindingsv <- tailcall(this.compileBindings(bindings))
        firstv    <- tailcall(this._compile(first))
        restv     <- tailcall(this.compileBody(rest))
      } yield SchemeLetStar(bindingsv, firstv :: restv, exp.pos)
    case SExpPair(SExpId(Identifier("let*", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme let*: $exp", exp.pos)
    case SExpPair(
        SExpId(Identifier("letrec", _)),
        SExpPair(bindings, SExpPair(first, rest, _), _),
        _
        ) =>
      for {
        bindingsv <- tailcall(this.compileBindings(bindings))
        firstv    <- tailcall(this._compile(first))
        restv     <- tailcall(this.compileBody(rest))
      } yield SchemeLetrec(bindingsv, firstv :: restv, exp.pos)
    case SExpPair(SExpId(Identifier("letrec", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme letrec: $exp", exp.pos)
    case SExpPair(
        SExpId(Identifier("set!", _)),
        SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _),
        _
        ) =>
      for {
        valuev <- tailcall(this._compile(value))
      } yield SchemeSet(v, valuev, exp.pos)
    case SExpPair(SExpId(Identifier("set!", _)), _, _) =>
      throw new SchemeCompilerException(s"Invalid Scheme set!: $exp", exp.pos)
    case SExpPair(SExpId(Identifier("begin", _)), body, _) =>
      tailcall(this.compileBody(body)).map(SchemeBegin(_, exp.pos))
    case SExpPair(SExpId(Identifier("cond", _)), clauses, _) =>
      tailcall(this.compileCondClauses(clauses)).map(SchemeCond(_, exp.pos))
    case SExpPair(SExpId(Identifier("case", _)), SExpPair(exp, clauses, _), _) =>
      tailcall(this.compileCaseClauses(clauses)).flatMap({
        case (c, d) => tailcall(this._compile(exp)).map(expv => SchemeCase(expv, c, d, exp.pos))
      })
    case SExpPair(SExpId(Identifier("and", _)), args, _) =>
      tailcall(this.compileBody(args)).map(SchemeAnd(_, exp.pos))
    case SExpPair(SExpId(Identifier("or", _)), args, _) =>
      tailcall(this.compileBody(args)).map(SchemeOr(_, exp.pos))
    case SExpPair(
        SExpId(Identifier("define", _)),
        SExpPair(SExpId(name), SExpPair(value, SExpValue(ValueNil, _), _), _),
        _
        ) =>
      tailcall(this._compile(value)).map(SchemeDefineVariable(name, _, exp.pos))
    case SExpPair(
        SExpId(Identifier("define", _)),
        SExpPair(SExpPair(SExpId(name), args, _), SExpPair(first, rest, _), _),
        _
        ) =>
      for {
        argsv  <- tailcall(this.compileArgs(args))
        firstv <- tailcall(this._compile(first))
        restv  <- tailcall(this.compileBody(rest))
      } yield SchemeDefineFunction(name, argsv, firstv :: restv, exp.pos)
    case SExpPair(
        SExpId(Identifier("do", _)),
        SExpPair(bindings, SExpPair(SExpPair(test, finals, _), commands, _), _),
        _
        ) =>
      for {
        bindingsv <- tailcall(this.compileDoBindings(bindings))
        testv     <- tailcall(this._compile(test))
        finalsv   <- tailcall(this.compileBody(finals))
        commandsv <- tailcall(this.compileBody(commands))
      } yield SchemeDo(bindingsv, testv, finalsv, commandsv, exp.pos)
    case SExpPair(f, args, _) =>
      for {
        fv    <- tailcall(this._compile(f))
        argsv <- tailcall(this.compileBody(args))
      } yield SchemeFuncall(fv, argsv, exp.pos)
    case SExpId(v) =>
      if (reserved.contains(v.name)) {
        throw new SchemeCompilerException(s"Invalid Scheme identifier (reserved): $exp", exp.pos)
      } else {
        done(SchemeVar(v))
      }
    case SExpValue(value, _)   => done(SchemeValue(value, exp.pos))
    case SExpQuoted(quoted, _) => done(SchemeQuoted(quoted, exp.pos))
  }

  def compileArgs(args: SExp): TailRec[List[Identifier]] = args match {
    case SExpPair(SExpId(id), rest, _) =>
      for {
        restv <- tailcall(this.compileArgs(rest))
      } yield id :: restv
    case SExpValue(ValueNil, _) => done(Nil)
    case _                      => throw new SchemeCompilerException(s"Invalid Scheme argument list: $args", args.pos)
  }

  def compileBody(body: SExp): TailRec[List[SchemeExp]] = body match {
    case SExpPair(exp, rest, _) =>
      for {
        expv  <- tailcall(this._compile(exp))
        restv <- tailcall(this.compileBody(rest))
      } yield expv :: restv
    case SExpValue(ValueNil, _) => done(Nil)
    case _                      => throw new SchemeCompilerException(s"Invalid Scheme body: $body", body.pos)
  }

  def compileBindings(bindings: SExp): TailRec[List[(Identifier, SchemeExp)]] = bindings match {
    case SExpPair(SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _), rest, _) =>
      if (reserved.contains(v.name)) {
        throw new SchemeCompilerException(s"Invalid Scheme identifier (reserved): $v", bindings.pos)
      } else {
        for {
          valuev <- tailcall(this._compile(value))
          restv  <- tailcall(this.compileBindings(rest))
        } yield (v, valuev) :: restv
      }
    case SExpValue(ValueNil, _) => done(Nil)
    case _                      => throw new SchemeCompilerException(s"Invalid Scheme bindings: $bindings", bindings.pos)
  }

  def compileDoBindings(bindings: SExp): TailRec[List[(Identifier, SchemeExp, Option[SchemeExp])]] =
    bindings match {
      case SExpPair(SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _), rest, _) =>
        if (reserved.contains(v.name)) {
          throw new SchemeCompilerException(
            s"Invalid Scheme identifier (reserved): $v",
            bindings.pos
          )
        } else {
          for {
            valuev <- tailcall(this._compile(value))
            restv  <- tailcall(this.compileDoBindings(rest))
          } yield (v, valuev, None) :: restv
        }
      case SExpPair(
          SExpPair(SExpId(v), SExpPair(value, SExpPair(step, SExpValue(ValueNil, _), _), _), _),
          rest,
          _
          ) =>
        if (reserved.contains(v.name)) {
          throw new SchemeCompilerException(
            s"Invalid Scheme identifier (reserved): $v",
            bindings.pos
          )
        } else {
          for {
            valuev <- tailcall(this._compile(value))
            stepv  <- tailcall(this._compile(step))
            restv  <- tailcall(this.compileDoBindings(rest))
          } yield (v, valuev, Some(stepv)) :: restv
        }
      case SExpValue(ValueNil, _) => done(Nil)
      case _ =>
        throw new SchemeCompilerException(s"Invalid Scheme do-bindings: $bindings", bindings.pos)
    }

  def compileCondClauses(clauses: SExp): TailRec[List[(SchemeExp, List[SchemeExp])]] =
    clauses match {
      case SExpPair(
          SExpPair(SExpId(Identifier("else", _)), SExpPair(first, rest, _), _),
          SExpValue(ValueNil, _),
          _
          ) =>
        for {
          firstv <- tailcall(this._compile(first))
          restv  <- tailcall(this.compileBody(rest))
        } yield List((SchemeValue(ValueBoolean(true), clauses.pos), firstv :: restv))
      case SExpPair(SExpPair(cond, SExpPair(first, rest, _), _), restClauses, _) =>
        for {
          condv        <- tailcall(this._compile(cond))
          firstv       <- tailcall(this._compile(first))
          restv        <- tailcall(this.compileBody(rest))
          restClausesv <- tailcall(this.compileCondClauses(restClauses))
        } yield (condv, firstv :: restv) :: restClausesv
      case SExpPair(SExpPair(cond, SExpValue(ValueNil, _), _), restClauses, _) =>
        for {
          condv        <- tailcall(this._compile(cond))
          restClausesv <- tailcall(this.compileCondClauses(restClauses))
        } yield (condv, Nil) :: restClausesv
      case SExpValue(ValueNil, _) => done(Nil)
      case _ =>
        throw new SchemeCompilerException(s"Invalid Scheme cond clauses: $clauses", clauses.pos)
    }

  def compileCaseClauses(
      clauses: SExp
  ): TailRec[(List[(List[SchemeValue], List[SchemeExp])], List[SchemeExp])] =
    clauses match {
      case SExpPair(
          SExpPair(SExpId(Identifier("else", _)), SExpPair(first, rest, _), _),
          SExpValue(ValueNil, _),
          _
          ) =>
        for {
          firstv <- tailcall(this._compile(first))
          restv  <- tailcall(this.compileBody(rest))
        } yield (List(), firstv :: restv)
      case SExpPair(SExpPair(objects, body, _), restClauses, _) =>
        tailcall(this.compileCaseClauses(restClauses)).flatMap({
          case (compiled, default) =>
            tailcall(this.compileCaseObjects(objects)).flatMap(
              objectsv =>
                tailcall(this.compileBody(body)).map(bodyv => ((objectsv, bodyv) :: compiled, default))
            )
        })
      case SExpValue(ValueNil, _) => done((Nil, Nil))
      case _ =>
        throw new SchemeCompilerException(s"Invalid Scheme case clauses: $clauses", clauses.pos)
    }

  def compileCaseObjects(objects: SExp): TailRec[List[SchemeValue]] = objects match {
    case SExpPair(SExpValue(v, _), rest, _) =>
      for {
        restv <- tailcall(this.compileCaseObjects(rest))
      } yield SchemeValue(v, objects.pos) :: restv
    case SExpPair(SExpId(id), rest, _) =>
      /* identifiers in case expressions are treated as symbols */
      for {
        restv <- tailcall(this.compileCaseObjects(rest))
      } yield SchemeValue(ValueSymbol(id.name), id.pos) :: restv
    case SExpValue(ValueNil, _) => done(Nil)
    case _ =>
      throw new SchemeCompilerException(s"Invalid Scheme case objects: $objects", objects.pos)
  }
}
