package scala.machine

import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.machine.AAM
import scalaam.graph.Graph.GraphOps
import scalaam.graph._

import scala.core.{Expression, MachineUtil}

class ModAtom[Exp <: Expression, A <: Address, V, T, TID <: ThreadIdentifier](
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
  type KAddr = KA

  type VStore = Store[A, V]
  type KStore = Store[KAddr, Set[Kont]]

  type Created    = Set[State]
  type Successors = Set[State]
  type Joined     = Set[TID]

  type Threads   = Map[TID, Set[State]]
  type RetVals   = Map[TID, V]
  type JoinDeps  = Map[TID, Set[TID]]
  type ReadDeps  = Map[TID, Set[A]]
  type WriteDeps = Map[TID, Set[A]]

  type Edges          = Map[State, Set[(Transition, State)]] // A map is used to overwrite any old edges that would remain present in a List or Set.
  type GraphEdges     = List[(State, Transition, State)]
  type UnlabeledEdges = Map[State, Set[State]]

  var theStore: VStore = Store.initial[A, V](t, sem.initialStore)(lattice) // This is ugly!

  /** Class used to return all information resulting from stepping this state. */
  case class StepResult(
      successors: Successors,
      created: Created,
      result: Option[V],
      effects: Effects,
      store: VStore,
      kstore: KStore
  ) {
    def merge(acc: StepResult): StepResult =
      StepResult(
        successors ++ acc.successors,
        created ++ acc.created,
        Option.empty,
        effects ++ acc.effects,
        acc.store.join(store),
        acc.kstore.join(kstore)
      )
  }

  /**
    * The execution state of one process (thread), containing its identifier and running information.
    * The variable store is shared among all threads; the continuation store is not.
    *
    * @param tid     The thread identifier corresponding to this context.
    * @param control The control component of the thread.
    * @param cc      The address of the current continuation.
    * @param time    A timestamp.
    * //@param kstore  A continuation store.
    */
  case class State(tid: TID, control: Control, cc: KAddr, time: T)
      extends GraphElement
      with SmartHash {
    override def toString: String = control.toString

    override def label: String = toString

    override def color: Color = control match {
      case ControlEval(_, _)        => Colors.Blue
      case ControlKont(_) if halted => Colors.Grass
      case ControlKont(_)           => Colors.Yellow
      case ControlError(_)          => Colors.Red
    }

    override def metadata =
      GraphMetadataMap(
        Map("halted" -> GraphMetadataBool(halted), "type" -> GraphMetadataString("concMod"))
      )

    /** Indicates whether this state is a final state. */
    def halted: Boolean = (cc, control) match {
      case (HaltKontAddr, ControlKont(_)) => true
      case (_, ControlError(_))           => true
      case _                              => false
    }

    /** Indicates whether this state represents an error. */
    def errored: Boolean = control match {
      case ControlError(_) => true
      case _               => false
    }

    /** Helper function to create new results easily without having to write all fields explicitly. */
    private def newResult(
        successor: State,
        effects: Effects,
        store: VStore,
        kstore: KStore,
        created: Created = Set.empty,
        result: Option[V] = Option.empty
    ): StepResult = {
      StepResult(Set(successor), created, result, effects, store, kstore)
    }

    /**
      * Executes an action on this state and returns the result as well as other bookkeeping information in the form of a stepResults instance.
      * @param action  The action to perform on this state.
      * @param old     The store that was used to obtain the action.
      * @param cc      The address of the current continuation. May differ from the instance variable cc stored in this state (e.g. after a frame has been popped of the stack).
      * @param results A map of TIDs to values, used to retrieve the return values of other processes.
      * @return Returns a stepResult containing all successor states of this states after performing the actions
      *         as well as bookkeeping information and the resulting store.
      */
    private def act(
        action: Act,
        time: T,
        old: VStore,
        kstore: KStore,
        cc: KAddr,
        results: RetVals
    ): StepResult = action match {
      // The semantics reached a value => continue with this value.
      case Value(v, store, effs) =>
        newResult(State(tid, ControlKont(v), cc, timestamp.tick(time)), effs, store, kstore)
      // A frame needs to be pushed on the stack and the evaluation needs to continue by evaluating 'e'.
      case Push(frame, e, env, store, effs) =>
        val cc_ = KontAddr(e, time)
        newResult(
          State(tid, ControlEval(e, env), cc_, timestamp.tick(time)),
          effs,
          store,
          kstore.extend(cc_, Set(Kont(frame, cc)))
        )
      // The expression 'e' needs to be evaluated in the given environment.
      case Eval(e, env, store, effs) =>
        newResult(State(tid, ControlEval(e, env), cc, timestamp.tick(time)), effs, store, kstore)
      // The evaluation steps into a function body. Same than Eval, except that the function is used to tick the timestamp.
      case StepIn(f, _, e, env, store, effs) =>
        newResult(State(tid, ControlEval(e, env), cc, timestamp.tick(time, f)), effs, store, kstore)
      // The semantics reached an error, which needs to be returned.
      case Err(e) =>
        // println(s"Error state: $e")
        newResult(State(tid, ControlError(e), cc, timestamp.tick(time)), Set.empty, old, kstore)
      // A new process is spawn by the semantics. The machine allocates a new TID and records the state of the new process.
      case NewFuture(ftid: TID @unchecked, tidv, fst, frame: Frame @unchecked, env, store, effs) =>
        val cc_       = KontAddr(fst, time)
        val newPState = State(ftid, ControlEval(fst, env), cc_, timestamp.initial(ftid.pos.toString))
        val curPState = State(tid, ControlKont(tidv), cc, timestamp.tick(time))
        StepResult(
          Set(curPState),
          Set(newPState),
          Option.empty,
          effs ++ Effects.spawn(ftid),
          store,
          kstore.extend(cc_, Set(Kont(frame, HaltKontAddr)))
        )
      // The semantics wants to read a value from another thread, which needs to be looked up in the 'results' map.
      case DerefFuture(ftid: TID @unchecked, store, effs) =>
        StepResult(
          results.get(ftid).map(v => State(tid, ControlKont(v), cc, timestamp.tick(time))).toSet,
          Set.empty,
          Option.empty,
          effs ++ Effects.join(ftid),
          store,
          kstore
        )
      // An unknown action has been returned. Should not happen, therefore this is an error.
      case a => throw new Exception(s"Unsupported action: $a.\n")
    }

    /**
      * Produces the states following this state by applying the given actions successively, thereby updating the store.
      * @param actions The set of actions to perform.
      * @param old     The store that was used to obtain the actions.
      * @param cc      The address of the current continuation. May differ from the instance variable cc stored in this state.
      * @param results A map of TIDs to values, used to retrieve the return values of other processes.
      * @return Returns a stepResult containing all successor states and bookkeeping information, as well as the final store.
      */
    private def next(
        actions: Set[Act],
        old: VStore,
        kstore: KStore,
        cc: KAddr,
        results: RetVals
    ): StepResult = {
      val init: StepResult = StepResult(Set.empty, Set.empty, Option.empty, Set.empty, old, kstore)
      actions.foldLeft(init)(
        (acc, curAction) => act(curAction, time, acc.store, acc.kstore, cc, results).merge(acc)
      )
    }

    /**
      * Decides on what to do by looking at the control component of the state and executes the according actions
      * if necessary.
      * @param store The store to use while stepping this state.
      * @return A tuple containing of:
      *         * the successor states of this state;
      *         * the processes created by this thread;
      *         * the processes of which this thread reads the result;
      *         * a possible result value (filled when the process finishes execution);
      *         * the effects generated during this step of execution;
      *         * the resulting store.
      */
    def step(store: VStore, kstore: KStore, results: RetVals): StepResult = control match {
      // Evaluate the given expression in the given environment.
      case ControlEval(exp, env) =>
        next(sem.stepEval(exp, env, store, time), store, kstore, cc, results)
      // The end of evaluation has been reached. Return the final result.
      case ControlKont(v) if cc == HaltKontAddr =>
        StepResult(Set.empty, Set.empty, Some(v), Set.empty, store, kstore)
      // Continue with a given value, given the continuation frame in this state. Pops this frame of the stack.
      case ControlKont(v) =>
        val init: StepResult =
          StepResult(Set.empty, Set.empty, Option.empty, Set.empty, store, kstore)
        kstore.lookup(cc).foldLeft(init)(
            (acc1, konts) => // Lookup all associated continuation frames.
              konts.foldLeft(acc1) {
                case (acc2, Kont(frame, cc_)) => // For each frame, generate the next actions and accumulate everything (starting from acc1).
                  next(
                    sem.stepKont(v, frame, acc2.store, time),
                    acc2.store,
                    acc2.kstore,
                    cc_,
                    results
                  ).merge(acc2) // Note that the frame is popped of the stack by passing cc_.
              }
          )
      // Handle an error. This results in no successor states.
      case ControlError(_) => StepResult(Set.empty, Set.empty, None, Set.empty, store, kstore)
      // An unknown control component has been reached. Should not happen so throw an error.
      case e => throw new Exception(s"Unsupported control sequence: $e.\n")
    }
  }

  /**
    * Analyses a given program in a thread-modular way. Returns a state graph representing the collecting semantics of the program.
    * @param program The program to be analysed.
    * @param timeout A timeout after which the analysis should stop to prevent looping.
    * @param ev      A implicit graph.
    * @tparam G The type of the graph to be returned.
    * @return Returns a state graph representing the collecting semantics of the program that was analysed.
    */
  def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {

    /**
      * Contains bookkeeping information for the inner loop of the algorithm.
      * @param created The threads created by the current thread.
      * @param effects The effects generated during the evaluation of the current thread.
      * @param result  The result value of the thread (initially bottom).
      * @param edges   A list of graph edges.
      */
    case class InnerLoopState(created: Created, effects: Effects, result: V, edges: UnlabeledEdges)
        extends SmartHash {
      def add(
          crea: Created,
          effs: Effects,
          res: Option[V],
          baseState: State,
          successors: Successors
      ): InnerLoopState = InnerLoopState(
        created ++ crea,
        effects ++ effs,
        lattice.join(result, res.getOrElse(lattice.bottom)),
        // Replace the edges from a re-evaluated state.
        edges + (baseState -> successors)
      )
    }

    /**
      * Performs the analysis for a single thread.
      * @param work      A worklist.
      * @param results   A mapping from TIDs to result values.
      * @param store     The global store.
      * @param iteration The iteration of the outer loop.
      * @param visited   A list of visited states to be omitted during looping. This argument can (should) be omitted.
      * @param iState    An InnerLoopState containing initial bookkeeping information. This argument can (should) be omitted.
      * @return A set of a new global store and an innerloopstate containing extra bookkeeping information for the outer loop.
      */
    @scala.annotation.tailrec
    def innerLoop(
        work: List[State],
        results: RetVals,
        store: VStore,
        kstore: KStore,
        iteration: Int,
        visited: Set[State] = Set.empty,
        iState: InnerLoopState = InnerLoopState(Set.empty, Set.empty, lattice.bottom, Map.empty)
    ): (VStore, KStore, InnerLoopState) = {
      if (timeout.reached || work.isEmpty) (store, kstore, iState)
      else {
        val (work_, visited_, store_, kstore_, iState_): (
            List[State],
            Set[State],
            VStore,
            KStore,
            InnerLoopState) =
          work.foldLeft((List.empty[State], visited, store, kstore, iState)) {
            case (acc @ (workAcc, visitedAcc, storeAcc, kstoreAcc, iStateAcc), curState) =>
              if (visitedAcc.contains(curState))
                acc  // If the state has been explored already, do not take a step.
              else { // If the state has not been explored yet, do take a step.
                val StepResult(succs, crea, res, effs, sto, ksto) = curState.step(storeAcc, kstoreAcc, results)
                val vis =
                  if (sto.asInstanceOf[DeltaStore[A, V]].updated.nonEmpty || ksto.asInstanceOf[DeltaStore[KAddr, Set[Kont]]].updated.nonEmpty)
                    Set.empty[State]
                  else
                    visitedAcc + curState // Immediately clear the visited set upon a store change.
                (
                  workAcc ++ succs,
                  vis,
                  sto.asInstanceOf[DeltaStore[A, V]].clearUpdated,
                  ksto.asInstanceOf[DeltaStore[KAddr, Set[Kont]]].clearUpdated,
                  iStateAcc.add(crea, effs, res, curState, succs)
                )
              }
          }
        innerLoop(work_, results, store_, kstore_, iteration, visited_, iState_)
      }
    }

    /**
      * Contains bookkeeping information for the outer loop of the algorithm.
      * @param threads   A map of thread identifiers to sets of initial thread states.
      * @param readDeps  A map indicating the addresses a thread reads.
      * @param writeDeps A map indicating the addresses a thread writes to.
      * @param joinDeps  A map indicating the TIDs a thread joins.
      * @param results   A map of tids to result values.
      * @param store     The store.
      * @param edges     The edges of the graph.
      */
    case class OuterLoopState(
        threads: Threads,
        readDeps: ReadDeps,
        writeDeps: WriteDeps,
        joinDeps: JoinDeps,
        results: RetVals,
        store: VStore,
        kstore: KStore,
        edges: Edges
    ) extends SmartHash

    /**
      * Performs the fixed-point computation for the entire program. Uses the innerLoop method to analyse single threads and uses the results and dependencies returned from this
      * method to steer the computation.
      * @param work       A worklist.
      * @param oState     An OuterLoopState to contain track of the dependencies between threads and the current global store.
      * @param iteration  The current iteration of the outer loop. This argument can be omitted.
      * @return A Map of TIDs to graphs.
      */
    @scala.annotation.tailrec
    def outerLoop(work: List[State], oState: OuterLoopState, iteration: Int = 1): OuterLoopState = {
      if (timeout.reached || work.isEmpty) oState
      else {
        val next: (List[State], OuterLoopState) = work.foldLeft((List[State](), oState)) {
          case ((workAcc, oStateAcc), curState) =>
            val stid: TID = curState.tid
            val (store, kstore, InnerLoopState(created, effects, result, graph)) = innerLoop(
              List(curState),
              oStateAcc.results,
              oStateAcc.store,
              oStateAcc.kstore,
              iteration)
            // todoCreated contains the initial states of threads that have never been explored. threads is updated accordingly to newThreads to register these new states.
            val (todoCreated, newThreads): (Set[State], Threads) =
              created.foldLeft((Set[State](), oStateAcc.threads)) {
                case ((createdAcc, threadsAcc), curState) =>
                  if (threadsAcc(curState.tid).contains(curState))
                    (createdAcc, threadsAcc) // There already is an identical thread, so do nothing.
                  else
                    (
                      createdAcc + curState,
                      threadsAcc + (curState.tid -> (threadsAcc(curState.tid) + curState))
                    )
              }
            // Update module dependencies. todoEffects indicates which modules have to be reanalysed.
            val (readDeps, writeDeps, joinDeps): (ReadDeps, WriteDeps, JoinDeps) =
              effects.foldLeft((oStateAcc.readDeps, oStateAcc.writeDeps, oStateAcc.joinDeps)) {
                case ((rAcc, wAcc, jAcc), curEff) =>
                  curEff match {
                    case JoinEff(tid: TID @unchecked) =>
                      (rAcc, wAcc, jAcc + (tid -> (jAcc(tid) + stid)))
                    case ReadAddrEff(target: A @unchecked) =>
                      (rAcc + (stid -> (rAcc(stid) + target)), wAcc, jAcc)
                    case WriteAddrEff(target: A @unchecked) =>
                      (rAcc, wAcc + (stid -> (wAcc(stid) + target)), jAcc)
                    case _ => (rAcc, wAcc, jAcc)
                  }
              }
            // Wherever there is an R/W or W/W conflict, add the states that need to be re-explored due to a store change.
            val todoEffects: List[State] = (readDeps.keySet.foldLeft(Set[TID]())(
              (acc, curTid) =>
                if (stid != curTid && writeDeps(stid)
                      .intersect(oStateAcc.readDeps(curTid))
                      .exists(addr => oStateAcc.store.lookup(addr) != store.lookup(addr)))
                  acc + curTid
                else acc
            ) ++ writeDeps.keySet.foldLeft(Set[TID]())(
              (acc, curTid) =>
                if (stid != curTid && writeDeps(stid)
                      .intersect(oStateAcc.writeDeps(curTid))
                      .exists(addr => oStateAcc.store.lookup(addr) != store.lookup(addr)))
                  acc + curTid
                else acc
            )).toList.flatMap(newThreads)
            // Join the old and new return value. If the return value changes, all other threads joining in this thread need to be reanalysed.
            val retVal: V = lattice.join(oStateAcc.results(stid), result)
            val todoJoined: List[State] =
              if (oStateAcc.results(stid) == retVal) List.empty
              else joinDeps(stid).flatMap(newThreads).toList
            (
              workAcc ++ todoCreated ++ todoEffects ++ todoJoined,
              OuterLoopState(
                newThreads,
                readDeps,
                writeDeps,
                joinDeps,
                oStateAcc.results + (stid -> retVal),
                store,
                kstore,
                oStateAcc.edges ++ graph.mapValues(
                  set => set.map((BaseTransition(iteration.toString), _))
                )
              )
            )
        }
        outerLoop(next._1, next._2, iteration + 1)
      }
    }

    /** Filters out unreachable graph components that may result from invalidating edges. */
    @scala.annotation.tailrec
    def findConnectedStates(
        work: List[State],
        edges: Edges,
        visited: Set[State] = Set.empty,
        acc: GraphEdges = List.empty
    ): GraphEdges = {
      if (timeout.reached || work.isEmpty) return acc
      if (visited.contains(work.head)) findConnectedStates(work.tail, edges, visited, acc)
      else {
        val head = work.head
        val next = edges(head)
        // Prepend the edges and work upfront the respective lists (assume next to be much shorter than work/acc).
        findConnectedStates(
          next.map(_._2).toList ++ work.tail,
          edges,
          visited + head,
          next.map(t => (head, t._1, t._2)).toList ++ acc
        )
      }
    }

    val cc: KAddr           = HaltKontAddr
    val env: Environment[A] = Environment.initial[A](sem.initialEnv)
    val control: Control    = ControlEval(program, env)
    val kstore: KStore      = Store.initial[KAddr, Set[Kont]](t, Set.empty)
    val time: T             = timestamp.initial("")
    val tid: TID            = allocator.allocate(program, NoPosition, time)
    val state: State        = State(tid, control, cc, time)
    val threads: Threads    = Map(tid -> Set(state)).withDefaultValue(Set.empty)
    val vstore: VStore      = Store.initial[A, V](t, sem.initialStore)(lattice)
    val oState: OuterLoopState = OuterLoopState(
      threads,
      Map.empty.withDefaultValue(Set.empty),
      Map.empty.withDefaultValue(Set.empty),
      Map.empty.withDefaultValue(Set.empty),
      Map.empty.withDefaultValue(lattice.bottom),
      vstore,
      kstore,
      Map.empty
    )

    val result: OuterLoopState = outerLoop(List(state), oState)
    theStore = result.store
    Graph[G, State, Transition].empty
      .addEdges(findConnectedStates(result.threads.values.flatten.toList, result.edges))
  }
}
