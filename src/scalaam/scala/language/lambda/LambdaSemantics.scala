package scalaam.language.lambda

import scalaam.core._

case class LambdaSemantics[V, A <: Address, T, C](allocator: Allocator[A, T, C])(
    implicit val timestamp: Timestamp[T, C],
    implicit val lambdaLattice: LambdaLattice[V, A]
) extends Semantics[LambdaExp, A, V, T, C] {

  implicit val lattice: Lattice[V] = lambdaLattice

  case class FrameFuncallOperator(fexp: LambdaExp, args: List[LambdaExp], env: Environment[A])
      extends Frame
  case class FrameFuncallOperands(
      f: V,
      fexp: LambdaExp,
      cur: LambdaExp,
      args: List[(LambdaExp, V)],
      toeval: List[LambdaExp],
      env: Environment[A]
  ) extends Frame

  def stepEval(e: LambdaExp, env: Environment[A], store: Store[A, V], t: T): Set[Action.A] =
    e match {
      case LambdaFun(_, _, _) =>
        Action.Value(LambdaLattice[V, A].function(e, env), store)
      case LambdaCall(f, args, _) =>
        Action.Push(FrameFuncallOperator(f, args, env), f, env, store)
      case LambdaVar(id) =>
        Action.fromMF(
          env.lookupMF(id).flatMap(a => store.lookupMF(a).map(v => Action.Value(v, store)))
        )
    }

  def stepKont(v: V, frame: Frame, store: Store[A, V], t: T): Set[Action.A] = frame match {
    case FrameFuncallOperator(fexp, Nil, _) =>
      evalCall(v, fexp, Nil, store, t)
    case FrameFuncallOperator(fexp, arg :: args, env) =>
      Action.Push(FrameFuncallOperands(v, fexp, arg, List.empty, args, env), arg, env, store)
    case FrameFuncallOperands(f @ _, fexp, cur, args, Nil, env @ _) =>
      evalCall(f, fexp, ((cur, v) :: args).reverse, store, t)
    case FrameFuncallOperands(f @ _, fexp, cur, args, argtoeval :: argstoeval, env) =>
      Action.Push(
        FrameFuncallOperands(v, fexp, argtoeval, (cur, v) :: args, argstoeval, env),
        argtoeval,
        env,
        store
      )
  }

  def evalCall(
      f: V,
      fexp: LambdaExp,
      argsv: List[(LambdaExp, V)],
      store: Store[A, V],
      t: T
  ): Set[Action.A] =
    LambdaLattice[V, A]
      .closures(f)
      .map({
        case clo @ (LambdaFun(args, body, pos @ _), defenv) =>
          if (args.length == argsv.length) {
            val (callenv, store2) = bindArgs(args.zip(argsv.map(_._2)), defenv, store, t)
            Action.StepIn(fexp, clo, body, callenv, store2)
          } else {
            println(s"args: $args, argsv: $argsv")
            Action.Err(ArityError(fexp, args.length, argsv.length))
          }
        case (lam, _) => Action.Err(TypeError(lam, "closure", "not a closure"))
      })

  case class ArityError(call: LambdaExp, expected: Int, got: Int)   extends Error
  case class TypeError(e: LambdaExp, expected: String, got: String) extends Error

  def bindArgs(
      l: List[(Identifier, V)],
      env: Environment[A],
      store: Store[A, V],
      t: T
  ): (Environment[A], Store[A, V]) =
    l.foldLeft((env, store))({
      case ((env, store), (id, value)) => {
        val a = allocator.variable(id, t)
        (env.extend(id.name, a), store.extend(a, value))
      }
    })
}
