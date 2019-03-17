package scalaam.language.atomlang

import scalaam.core._
import scalaam.language.scheme._

/**
  * This class extends SchemeSemantics with constructs for concurrency using futures and atoms.
  *
  * @param addressAllocator An allocator for memory.
  * @param tidAllocator     An allocator for timestamps.
  * @param t                A timestamp.
  * @param lat              A lattice.
  * @tparam A The type of addresses.
  * @tparam V The type of values.
  * @tparam T The type of timestamps.
  * @tparam C The type of expressions.
  */
class AtomlangSemantics[A <: Address, V, T, C, TID <: ThreadIdentifier](addressAllocator: Allocator[A, T, C], tidAllocator: TIDAllocator[TID, T, C])(
    implicit val t: Timestamp[T, C],
    implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends BaseSchemeSemantics[A, V, T, C](addressAllocator)(t, lat)
        with AtomlangPrimitives[A, V, T, C] {
    
    import Action.{Err, NewFuture, Push}
    import schemeLattice._
    
    case class EmptyBodyException() extends Error
    
    /**
      * Performs an evaluation step of a given expression.
      *
      * @param e     The expression to evaluate.
      * @param env   The environment in which to evaluate the expression.
      * @param store The store to use for the evaluation.
      * @param t     The current timestamp.
      */
    override def stepEval(e: SchemeExp, env: Env, store: Sto, t: T): Actions = e match {
        case AtomlangDeref(exp, _) => Push(FrameDeref(), exp, env, store)
        // NewFuture contains a frame used to evaluate the rest of the body of the future (after having evaluated the first expression).
        // Another implementation strategy would be to alter the parser and to create an explicit "begin" expression using the expressions in the future's body.
        case AtomlangFuture(body, _) =>
            body match {
                case Nil => Err(EmptyBodyException()) // Disallow a body of a future to be empty.
                case fst :: rst =>
                    val tid = tidAllocator.allocate(e, t)
                    val tidv = future(tid)
                    val fram = FrameBegin(rst, env)
                    NewFuture(tid, tidv, fst, fram, env, store) // Let the machine handle the actual thread creation.
            }
        case AtomlangSwap(atomExp, funExp, argExps, _) => Push(FrameSwapAtom(atomExp, funExp, argExps, env), atomExp, env, store)
        case _ => super.stepEval(e, env, store, t)
    }
    
    /**
      * Performs a continuation step after the evaluator reached a value.
      *
      * @param v     The value that was reached by evaluation.
      * @param frame The topmost stackframe/continuation frame.
      * @param store The store to continue with.
      * @param t     The current timestamp.
      */
    override def stepKont(v: V, frame: Frame, store: Sto, t: T): Actions = frame match {
        case FrameDeref() => // Todo: extend to atoms.
            val futures = getFutures(v)
            if (futures.isEmpty) {
                Set(Err(TypeError("Cannot dereference non-future values.", v)))
            } else {
                futures.map(tid => Action.DerefFuture(tid, store))
            }
        case FrameSwapAtom(atomExp, funExp, argExps, env) => Push(FrameSwapFun(v, atomExp, funExp, argExps, env), funExp, env, store)
        case FrameSwapFun(atomv, atomExp, funExp, argExps, env) => swapArgs(atomv, atomExp, v, funExp, List(), argExps, env, store, t)
        case FrameSwapArgs(atomv, atomExp, funv, funExp, args, argExp :: rest, env) => swapArgs(atomv, atomExp, funv, funExp, (argExp, v) :: args, rest, env, store, t)
        case FrameSwapContinue(atomv, atomExp, funv, funExp, args, cnt) => evalSwapContinue(atomv, atomExp, funv, funExp, args, cnt, v, store, t)
        case _ => super.stepKont(v, frame, store, t)
    }
    
    def swapArgs(atomv: V, atomExp: SchemeExp, funv: V, funExp: SchemeExp, args: List[(SchemeExp, V)], toEval: List[SchemeExp], env: Env, store: Sto, t: T): Actions = toEval match {
        case Nil => evalSwap(atomv, atomExp, funv, funExp, args, store, t)
        case argExp :: _ => Push(FrameSwapArgs(atomv, atomExp, funv, funExp, args, toEval, env), argExp, env, store)
    }
    
    def evalSwap(atomv: V, atomExp: SchemeExp, function: V, fexp: SchemeExp, args: List[(SchemeExp, V)], store: Sto, t: T): Actions = {
        val atomValues = getPointerAddresses(atomv).flatMap(store.lookup(_).map(deref))
        if (atomValues.isEmpty) {
            Set(Err(TypeError("Pointer to atom expected.", atomv)))
        } else {
            atomValues.flatMap(_.mapSet({v =>
                    val argsv: List[(SchemeExp, V)] = (atomExp, v) :: args
                    val frame: AtomLangFrame = FrameSwapContinue(atomv, atomExp, function, fexp, args, v)
                    // Part analogous to evalCall.
                    val fromClo: Actions = getClosures(function).map({
                        case (SchemeLambda(args, body, pos), env1) =>
                            if (args.length == argsv.length) {
                                bindArgs(args.zip(argsv.map(_._2)), env1, store, t) match {
                                    case (env2, store) =>
                                        if (body.length == 1)
                                            Set(Action.Swap(atomExp, Action.StepIn(fexp, (SchemeLambda(args, body, pos), env1), body.head, env2, store), frame))
                                        else
                                            Set(Action.Swap(atomExp,
                                                    Action.StepIn(fexp,
                                                    (SchemeLambda(args, body, pos), env1),
                                                    SchemeBegin(body, pos),
                                                    env2,
                                                    store), frame))
                                }
                            } else { Set(Action.Err(ArityError(fexp, args.length, argsv.length))) }
                        case (lambda, env1) =>
                            Set(Action.Err(TypeError("operator expected to be a closure, but is not", closure((lambda, env1)))))
                    }).flatten // Apparently, flatMap does not equal map + flatten (the former gives a type error).
                    val fromPrim: Actions =
                        getPrimitives[Primitive](function).flatMap(prim => prim.callActionEffs(fexp, argsv, store, t))
                    if (fromClo.isEmpty && fromPrim.isEmpty) {
                        Set(Action.Err(TypeError("operator expected to be a function, but is not", function)))
                    } else {
                        fromClo ++ fromPrim
                    }})
                (err => Action.Err(err))).flatten
        }
    }
    
    /**
      * evalSwapContinue verifies whether the old and new value of an atom still are equal. If so, the value resultv = f(atom) is valid,
      * otherwise, f(atom) needs to be evaluated again.
      * @param atomv    The value for the atom itself.
      * @param atomExp  The atom expression.
      * @param function The value for the function.
      * @param fexp     The function expression.
      * @param args     The arguments to the function (except the atom).
      * @param cnt      The value of the atom used to calculate f(atom).
      * @param resultv  The result of calculating f(atom).
      * @param store    The store.
      * @return Actions to be performed by the machine.
      */
    def evalSwapContinue(atomv: V, atomExp: SchemeExp, function: V, fexp: SchemeExp, args: List[(SchemeExp, V)], cnt: V, resultv: V, store: Sto, t: T): Actions = {
        val atomValues = getPointerAddresses(atomv).flatMap(a => store.lookup(a).map(v => (a, deref(v))))
        atomValues.flatMap{case (a,v) => v.mapSet(v =>
            lat.binaryOp(SchemeOps.BinaryOperator.Eq)(cnt, v).mapSet({e =>
                    val fromTrue:  Actions = if (lat.isTrue(e))  Set(Action.Value(resultv, store.updateOrExtend(a, resultv))) else Set()
                    val fromFalse: Actions = if (lat.isFalse(e)) evalSwap(atomv, atomExp, function, fexp, args, store, t)     else Set()
                    fromTrue ++ fromFalse
                    })(err => Set(Action.Err(err))))(err => Set(Action.Err(err)))}.flatten.flatten
    }
    
    trait AtomLangFrame extends SchemeFrame
    
    case class FrameDeref() extends AtomLangFrame
    
    case class FrameSwapAtom(atomExp: SchemeExp, funExp: SchemeExp, argExps: List[SchemeExp], env: Env) extends AtomLangFrame
    case class FrameSwapFun(atomv: V, atomExp: SchemeExp, funExp: SchemeExp, argExps: List[SchemeExp], env: Env) extends AtomLangFrame
    case class FrameSwapArgs(atomv: V, atomExp: SchemeExp, funv: V, funExp: SchemeExp, args: List[(SchemeExp, V)], toEval: List[SchemeExp], env: Env) extends SchemeFrame
    case class FrameSwapContinue(atomv: V, atomExp: SchemeExp, funv: V, funExp: SchemeExp, args: List[(SchemeExp, V)], atomContents: V) extends AtomLangFrame
}