package scalaam.language.scheme

import scalaam.core.{Position, Identifier}

/**
  * Remove defines from a Scheme expression, replacing them by let bindings.
  * For example:
  *   (define foo 1)
  *   (define (f x) x)
  *   (f foo)
  * Will be converted to:
  *   (letrec ((foo 1)
  *            (f (lambda (x) x)))
  *     (f foo))
  * Which is semantically equivalent with respect to the end result
  */
trait SchemeUndefiner {
  import scala.util.control.TailCalls._

  def undefine(exps: List[SchemeExp]): SchemeExp =
    this.undefine(exps, List()).result

  def undefine(exps: List[SchemeExp], defs: List[(Identifier, SchemeExp)]): TailRec[SchemeExp] =
    exps match {
      case Nil => done(SchemeBegin(Nil, Position.none))
      case SchemeDefineFunction(name, args, body, pos) :: rest =>
        tailcall(
          tailcall(this.undefineBody(body)).flatMap(
            bodyv =>
              this.undefine(
                SchemeDefineVariable(name, SchemeLambda(args, bodyv, exps.head.pos), pos) :: rest,
                defs
              )
          )
        )
      case SchemeDefineVariable(name, value, _) :: rest =>
        tailcall(this.undefine1(value)).flatMap(v => tailcall(this.undefine(rest, (name, v) :: defs)))
      case _ :: _ =>
        if (defs.isEmpty) {
          tailcall(this.undefineBody(exps)).flatMap({
            case Nil        => done(SchemeBegin(Nil, Position.none))
            case exp :: Nil => done(exp)
            case exps       => done(SchemeBegin(exps, exps.head.pos))
          })
        } else {
          tailcall(this.undefineBody(exps))
            .flatMap(body => done(SchemeLetrec(defs.reverse, body, exps.head.pos)))
        }
    }

  def trampolineM[A, B](f: A => TailRec[B], l: List[A]): TailRec[List[B]] = l match {
    case Nil => done(Nil)
    case x :: xs =>
      tailcall(f(x)).flatMap(y => tailcall(trampolineM(f, xs)).flatMap(ys => done(y :: ys)))
  }

  def undefine1(exp: SchemeExp): TailRec[SchemeExp] = this.undefine(List(exp), List())

  def undefineBody(exps: List[SchemeExp]): TailRec[List[SchemeExp]] = exps match {
    case Nil                                   => done(Nil)
    case SchemeDefineFunction(_, _, _, _) :: _ => tailcall(this.undefine(exps, List())).map(v => List(v))
    case SchemeDefineVariable(_, _, _) :: _    => tailcall(this.undefine(exps, List())).map(v => List(v))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeLambda(args, body, pos) =>
          tailcall(this.undefineBody(body)).map(b => SchemeLambda(args, b, pos))
        case SchemeFuncall(f, args, pos) =>
          tailcall(this.undefine1(f)).flatMap(
            fun => trampolineM(this.undefine1, args).map(argsv => SchemeFuncall(fun, argsv, pos))
          )
        case SchemeIf(cond, cons, alt, pos) =>
          tailcall(this.undefine1(cond)).flatMap(
            condv =>
              tailcall(this.undefine1(cons)).flatMap(
                consv => tailcall(this.undefine1(alt)).map(altv => SchemeIf(condv, consv, altv, pos))
              )
          )
        case SchemeLet(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(this.undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv => tailcall(this.undefineBody(body)).map(bodyv => SchemeLet(bindingsv, bodyv, pos))
          )
        case SchemeLetStar(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(this.undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv =>
              tailcall(this.undefineBody(body)).map(bodyv => SchemeLetStar(bindingsv, bodyv, pos))
          )
        case SchemeLetrec(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(this.undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv =>
              tailcall(this.undefineBody(body)).map(bodyv => SchemeLetrec(bindingsv, bodyv, pos))
          )
        case SchemeNamedLet(name, bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(this.undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv =>
              tailcall(this.undefineBody(body)).map(bodyv => SchemeNamedLet(name, bindingsv, bodyv, pos))
          )
        case SchemeSet(variable, value, pos) =>
          tailcall(this.undefine1(value)).map(v => SchemeSet(variable, v, pos))
        case SchemeBegin(exps, pos) =>
          tailcall(this.undefineBody(exps)).map(expsv => SchemeBegin(expsv, pos))
        case SchemeAnd(args, pos) =>
          trampolineM(this.undefine1, args).map(argsv => SchemeAnd(argsv, pos))
        case SchemeOr(args, pos)       => trampolineM(this.undefine1, args).map(argsv => SchemeOr(argsv, pos))
        case SchemeVar(id)             => done(SchemeVar(id))
        case SchemeQuoted(quoted, pos) => done(SchemeQuoted(quoted, pos))
        case SchemeValue(value, pos)   => done(SchemeValue(value, pos))
      }
      exp2.flatMap(e2 => tailcall(this.undefineBody(rest)).flatMap(e3 => done(e2 :: e3)))
    }
  }
}
