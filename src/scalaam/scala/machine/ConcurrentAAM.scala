package scalaam.machine

import scalaam.core.Annotations.unsound
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.graph.Graph.GraphOps
import scalaam.graph._
import scalaam.machine.Strategy.Strategy

import scala.core.{Expression, MachineUtil}

/**
  * Implementation of an abstract PCESK machine. This machine uses parts of an underlying AAM.<br><br>
  *
  * Based on https://github.com/acieroid/scala-am/blob/744a13a5b957c73a9d0aed6e10f7dae382c9b2e3/src/main/scala/machine/concurrent/ConcurrentAAM.scala
  */
class ConcurrentAAM[Exp <: Expression, A <: Address, V, T, TID <: ThreadIdentifier](
    val t: StoreType,
    val sem: Semantics[Exp, A, V, T, Exp],
    val allocator: TIDAllocator[TID, T, Exp]
)(implicit val timestamp: Timestamp[T, Exp], implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
    with MachineUtil[Exp, A, V] {

  import sem.Action.{DerefFuture, Err, Eval, NewFuture, Push, StepIn, Value, A => Act}

  /** Certain parts of this AAM will be reused. */
  val seqAAM = new AAM[Exp, A, V, T](t, sem)

  import seqAAM._

  /** Various type declarations. */
  type KAddr   = KA
  type VStore  = Store[A, V]
  type KStore  = Store[KAddr, Set[Kont]]
  type Threads = BlockableTMap[TID, Context, V]

  /**
    * The state of one thread.
    *
    * @param tid     The thread identifier corresponding to this context.
    * @param control The control component of the thread.
    * @param cc      The address of the current continuation.
    * @param time    A timestamp.
    * @param kstore  A continuation store.
    */
  case class Context(tid: TID, control: Control, cc: KAddr, time: T, kstore: KStore) {
    override def toString: String = control.toString

    def halted: Boolean = (cc, control) match {
      case (HaltKontAddr, ControlKont(_)) => true
      case (_, ControlError(_))           => true
      case _                              => false
    }

    /** Executes one action on a state corresponding to a given tid and returns the result. */
    def act(
        threads: Threads,
        action: Act,
        cc: KAddr,
        time: T,
        old: VStore,
        kstore: KStore
    ): (Threads, VStore) = action match {
      case Value(v, store, _) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlKont(v), cc, timestamp.tick(time), kstore)
          ),
          store
        )
      case Push(frame, e, env, store, _) =>
        val cc_ = KontAddr(e, time)
        (
          threads.updateThread(
            tid,
            this,
            Context(
              tid,
              ControlEval(e, env),
              cc_,
              timestamp.tick(time),
              kstore.extend(cc_, Set(Kont(frame, cc)))
            )
          ),
          store
        )
      case Eval(e, env, store, _) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlEval(e, env), cc, timestamp.tick(time), kstore)
          ),
          store
        )
      case StepIn(f, _, e, env, store, _) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlEval(e, env), cc, timestamp.tick(time, f), kstore)
          ),
          store
        )
      case Err(e) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlError(e), cc, timestamp.tick(time), kstore)
          ),
          old
        )
      case NewFuture(tid_ : TID @unchecked, tidv, fst, frame: Frame @unchecked, env, store, _) =>
        val cc_ = KontAddr(fst, time)
        val newPState = Context(
          tid_,
          ControlEval(fst, env),
          cc_,
          timestamp.initial(tid_.toString),
          Store.empty[KA, Set[Kont]](t).extend(cc_, Set(Kont(frame, HaltKontAddr)))
        )
        val curPState = Context(tid, ControlKont(tidv), cc, timestamp.tick(time), kstore)
        (threads.updateThread(tid, this, curPState).newThread(tid_, newPState), store)
      case DerefFuture(tid_ : TID @unchecked, store, _) =>
        if (threads.threadFinished(tid_))
          (
            threads.updateThread(
              tid,
              this,
              Context(tid, ControlKont(threads.getResult(tid_)), cc, timestamp.tick(time), kstore)
            ),
            store
          )
        else
          (threads.blockThread(tid, this, tid_), store)
      case a => throw new Exception(s"Unsupported action: $a.\n")
    }

    /** Produces the states following this state by applying the given actions. */
    def next(actions: Set[Act], threads: Threads, store: VStore, cc: KAddr): Set[State] = {
      actions.map(
        action =>
          act(threads, action, cc, time, store, kstore) match {
            case (threads, store) => State(threads, store)
          }
      )
    }

    /** Steps from this state to the next. Returns a set of successorStates. */
    def step(threads: Threads, store: VStore): Set[State] =
      control match {
        case ControlEval(exp, env) => next(sem.stepEval(exp, env, store, time), threads, store, cc)
        case ControlKont(v) if cc == HaltKontAddr =>
          Set(State(threads.finishThread(tid, this, v), store))
        case ControlKont(v) =>
          kstore.lookup(cc) match {
            case Some(konts) =>
              konts.flatMap({
                case Kont(frame, cc_) =>
                  next(sem.stepKont(v, frame, store, time), threads, store, cc_)
              })
            case None => Set()
          }
        case ControlError(e) => Set(State(threads.failThread(tid, this, e), store))
        case e               => throw new Exception(s"Unsupported control sequence: $e.\n")
      }
  }

  /**
    * State of the machine.
    *
    * @param threads Threadmap mapping thread identifiers to contexts or to values.
    * @param store   A store shared by all threads in a state.
    */
  case class State(threads: Threads, store: VStore) extends GraphElement with SmartHash {
    override def toString: String = threads.toString

    override def label: String = toString

    override def color: Color = if (halted) Colors.Yellow else Colors.Green

    override def metadata =
      GraphMetadataMap(
        Map("halted" -> GraphMetadataBool(halted), "type" -> GraphMetadataString("conc"))
      )

    def halted: Boolean = threads.allDone

    /** Step the context(s) corresponding to a TID. Returns a set of tuples consisting out of the tid that was stepped and a resulting state. */
    def stepOne(tid: TID): Set[(TID, State)] = { // TODO: Just return a set of states?
      threads.getRunnable(tid).flatMap(state => state.step(threads, store).map((tid, _)))
    }

    /** Step the context(s) corresponding to multiple TIDs. Returns a set of tuples containing the tid that was stepped and a resulting state. */
    def stepMultiple(): Set[(TID, State)] = threads.threadsRunnable.flatMap(tid => stepOne(tid))

    /** Step the context(s) corresponding to a single TID. Returns a set of tuples containing the tid that was stepped and a resulting state. */
    @unsound
    def stepAny(): Set[(TID, State)] = {
      for (tid <- threads.threadsRunnable) {
        val next = stepOne(tid)
        if (next.nonEmpty) return next
      }
      Set()
    }

    /** Step the context(s) corresponding to a single, random TID. Returns a set of tuples containing the tid that was stepped and a resulting state. */
    @unsound
    def stepRandom(): Set[(TID, State)] = {
      for (tid <- scala.util.Random.shuffle(threads.threadsRunnable)) {
        val next = stepOne(tid)
        if (next.nonEmpty) return next
      }
      Set()
    }

    /**
      * Steps the machine from one state to the next. Different strategies may be used. <br><br>
      *
      * Strategies:<br>
      * <i>AllInterleavings</i>:  All threads that are not halted make a step.<br>
      * <i>OneInterleaving</i>: One thread that is not halted makes a step.
      */
    def step(strategy: Strategy): Set[(TID, State)] = strategy match {
      case Strategy.AllInterleavings   => stepMultiple()
      case Strategy.OneInterleaving    => stepAny()
      case Strategy.RandomInterleaving => stepRandom()
      case _                           => throw new Exception("Unsupported analysis strategy.")
    }
  }

  def run[G](program: Exp, timeout: Timeout.T, strategy: Strategy)(
      implicit ev: Graph[G, State, Transition]
  ): G = {

    /** Fixed-point computation to explore the entire state graph. */
    @scala.annotation.tailrec
    def loop(
        work: List[State],
        visited: Set[State] = Set.empty,
        graph: G = Graph[G, State, Transition].empty
    ): G = {
      if (timeout.reached) graph
      else
        work match {
          case Nil => graph
          case state :: work =>
            if (visited contains state)
              loop(work, visited, graph)
            else if (state.halted)
              loop(work, visited + state, graph)
            else {
              val next = state.step(strategy).map(_._2)
              val graph_ = next.foldLeft(graph)((g, state_) => g.addEdge(state, empty, state_))
              loop(work ++ next, visited + state, graph_)
            }
        }
    }

    val cc: KAddr           = HaltKontAddr
    val env: Environment[A] = Environment.initial[A](sem.initialEnv)
    val control: Control    = ControlEval(program, env)
    val kstore: KStore      = Store.empty[KA, Set[Kont]](t)
    val time: T             = timestamp.initial("")
    val tid: TID            = allocator.allocate(program, NoPosition, time)
    val context: Context    = Context(tid, control, cc, time, kstore)
    val threads: Threads    = BlockableTMap[TID, Context, V](Map(tid -> Set(context)))(lattice)
    val vstore: VStore      = Store.initial[A, V](t, sem.initialStore)(lattice)
    val state: State        = State(threads, vstore)

    loop(List(state))
  }

  def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
    run(program, timeout, Strategy.AllInterleavings)
  }
}

/** Enumeration listing strategies for the exploration of the concurrent state space. */
object Strategy extends Enumeration {
  type Strategy = Value
  val AllInterleavings, OneInterleaving, RandomInterleaving = Value
}

class ConcreteConcurrentAAM[Exp <: Expression, A <: Address, V, T, TID <: ThreadIdentifier](
    val t: StoreType,
    val sem: Semantics[Exp, A, V, T, Exp],
    val allocator: TIDAllocator[TID, T, Exp]
)(implicit val timestamp: Timestamp[T, Exp], implicit val lattice: Lattice[V])
    extends MachineAbstraction[Exp, A, V, T, Exp]
    with MachineUtil[Exp, A, V] {

  import sem.Action.{DerefFuture, Err, Eval, NewFuture, Push, StepIn, Value, A => Act}

  /** Certain parts of this AAM will be reused. */
  val seqAAM = new AAM[Exp, A, V, T](t, sem)

  import seqAAM._

  /** Various type declarations. */
  type KAddr   = KA
  type VStore  = Store[A, V]
  type KStore  = Store[KAddr, Set[Kont]]
  type Threads = BlockableTMap[TID, Context, V]

  /**
    * The state of one thread.
    *
    * @param tid     The thread identifier corresponding to this context.
    * @param control The control component of the thread.
    * @param cc      The address of the current continuation.
    * @param time    A timestamp.
    * @param kstore  A continuation store.
    */
  case class Context(tid: TID, control: Control, cc: KAddr, time: T, kstore: KStore) {
    override def toString: String = control.toString

    def halted: Boolean = (cc, control) match {
      case (HaltKontAddr, ControlKont(_)) => true
      case (_, ControlError(_))           => true
      case _                              => false
    }

    /** Executes one action on a state corresponding to a given tid and returns the result. */
    def act(
        threads: Threads,
        action: Act,
        cc: KAddr,
        time: T,
        old: VStore,
        kstore: KStore
    ): (Threads, VStore) = action match {
      case Value(v, store, _) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlKont(v), cc, timestamp.tick(time), kstore)
          ),
          store
        )
      case Push(frame, e, env, store, _) =>
        val cc_ = KontAddr(e, time)
        (
          threads.updateThread(
            tid,
            this,
            Context(
              tid,
              ControlEval(e, env),
              cc_,
              timestamp.tick(time),
              kstore.extend(cc_, Set(Kont(frame, cc)))
            )
          ),
          store
        )
      case Eval(e, env, store, _) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlEval(e, env), cc, timestamp.tick(time), kstore)
          ),
          store
        )
      case StepIn(f, _, e, env, store, _) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlEval(e, env), cc, timestamp.tick(time, f), kstore)
          ),
          store
        )
      case Err(e) =>
        (
          threads.updateThread(
            tid,
            this,
            Context(tid, ControlError(e), cc, timestamp.tick(time), kstore)
          ),
          old
        )
      case NewFuture(tid_ : TID @unchecked, tidv, fst, frame: Frame @unchecked, env, store, _) =>
        val cc_ = KontAddr(fst, time)
        val newPState = Context(
          tid_,
          ControlEval(fst, env),
          cc_,
          timestamp.initial(tid_.toString),
          Store.empty[KA, Set[Kont]](t).extend(cc_, Set(Kont(frame, HaltKontAddr)))
        )
        val curPState = Context(tid, ControlKont(tidv), cc, timestamp.tick(time), kstore)
        (threads.updateThread(tid, this, curPState).newThread(tid_, newPState), store)
      case DerefFuture(tid_ : TID @unchecked, store, _) =>
        // println(s"[debug] deref future $tid_, val is ${threads.getResult(tid_)}")
        if (threads.threadFinished(tid_))
          (
            threads.updateThread(
              tid,
              this,
              Context(tid, ControlKont(threads.getResult(tid_)), cc, timestamp.tick(time), kstore)
            ),
            store
          )
        else
          (threads.blockThread(tid, this, tid_), store)
      case a => throw new Exception(s"Unsupported action: $a.\n")
    }

    /** Produces the states following this state by applying the given actions. */
    def next(actions: Set[Act], threads: Threads, store: VStore, cc: KAddr): Set[State] = {
      actions.map(
        action =>
          act(threads, action, cc, time, store, kstore) match {
            case (threads, store) => State(threads, store)
          }
      )
    }

    /** Steps from this state to the next. Returns a set of successorStates. */
    def step(threads: Threads, store: VStore): Set[State] =
      control match {
        case ControlEval(exp, env)                => next(sem.stepEval(exp, env, store, time), threads, store, cc)
        case ControlKont(v) if cc == HaltKontAddr =>
          // println(s"[debug] Thread $tid finished with value $v")
          Set(State(threads.finishThread(tid, this, v), store))
        case ControlKont(v) =>
          kstore.lookup(cc) match {
            case Some(konts) =>
              konts.flatMap({
                case Kont(frame, cc_) =>
                  next(sem.stepKont(v, frame, store, time), threads, store, cc_)
              })
            case None => Set()
          }
        case ControlError(e) =>
          println(s"Error reached in concrete: $e")
          Set(State(threads.failThread(tid, this, e), store))
        case e               => throw new Exception(s"Unsupported control sequence: $e.\n")
      }
  }

  /**
    * State of the machine.
    *
    * @param threads Threadmap mapping thread identifiers to contexts or to values.
    * @param store   A store shared by all threads in a state.
    */
  case class State(threads: Threads, store: VStore) extends GraphElement with SmartHash {
    override def toString: String = threads.toString

    override def label: String = toString

    override def color: Color = if (halted) Colors.Yellow else Colors.Green

    override def metadata =
      GraphMetadataMap(
        Map("halted" -> GraphMetadataBool(halted), "type" -> GraphMetadataString("conc"))
      )

    def halted: Boolean = threads.allDone

    /** Step the context(s) corresponding to a single, random TID. Returns a set of tuples containing the tid that was stepped and a resulting state. */
    def step(): Set[(TID, State)] = {
      for (tid <- scala.util.Random.shuffle(threads.threadsRunnable.toList)) {
        val next = threads.getRunnable(tid).flatMap(state => {state.step(threads, store).map((tid, _))})
        if (next.nonEmpty) return next
      }
      Set()
    }
  }

  def run[G](program: Exp, timeout: Timeout.T)(
      implicit ev: Graph[G, State, Transition]
  ): G = {
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    var graph = Future { Graph[G, State, Transition].empty }

    /** Fixed-point computation to explore the entire state graph. */
    @scala.annotation.tailrec
    def loop(cur: State): Unit = {
      if (!timeout.reached && !cur.halted) {
        val next = cur.step().map(_._2)
        next.foreach(st => graph = graph.map(g => g.addEdge(cur, empty, st))) // Next should only be of size 1, but if it's not, this will allow to find it.
        if (next.size != 1) {
          println(s"State: $cur")
          next.foreach(println)
          System.err.println(s"Execution was not concrete! (got ${next.size} resulting states)")
          return
        }
        loop(next.head)
      }
    }
    val cc: KAddr           = HaltKontAddr
    val env: Environment[A] = Environment.initial[A](sem.initialEnv)
    val control: Control    = ControlEval(program, env)
    val kstore: KStore      = Store.empty[KA, Set[Kont]](t)
    val time: T             = timestamp.initial("")
    val tid: TID            = allocator.allocate(program, NoPosition, time)
    val context: Context    = Context(tid, control, cc, time, kstore)
    val threads: Threads    = BlockableTMap[TID, Context, V](Map(tid -> Set(context)))(lattice)
    val vstore: VStore      = Store.initial[A, V](t, sem.initialStore)(lattice)
    val state: State        = State(threads, vstore)

    graph = graph.map(g => g.addNode(state))
    loop(state)
    println(s"Execution finished, in ${timeout.time} seconds, waiting for graph")
    val res = Await.result(graph, Duration.Inf)
    println(s"Number of nodes: ${ev.nodes(res)}")
    res
  }
}
