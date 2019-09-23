package scala.machine

import scalaam.core.Annotations._
import scalaam.core.Effects.Effects
import scalaam.core.StoreType.StoreType
import scalaam.core._
import scalaam.machine.AAM
import scalaam.graph.Graph.GraphOps
import scalaam.graph._

import scala.core.MachineUtil

abstract class AtomAnalysis[Exp, A <: Address, V, T, TID <: ThreadIdentifier](val t: StoreType, val sem: Semantics[Exp, A, V, T, Exp], val allocator: TIDAllocator[TID, T, Exp])
                                                                             (implicit val timestamp: Timestamp[T, Exp], implicit val lattice: Lattice[V])
  extends MachineAbstraction[Exp, A, V, T, Exp] with MachineUtil[Exp, A, V] {

    import sem.Action.{DerefFuture, Err, Eval, NewFuture, Push, StepIn, Value, A => Act}

    /** Certain parts of this AAM will be reused. */
    val seqAAM = new AAM[Exp, A, V, T](t, sem)

    import seqAAM._

    /* Various type declarations. */

    type KAddr = KA

    type VStore = Store[A, V]
    type KStore = Store[KAddr, Set[Kont]]

    type Created = Set[State]
    type Successors = Set[State]
    type Joined = Set[TID]

    type Threads = Map[TID, Set[State]]
    type RetVals = Map[TID, V]

    type RestartTarget // To specify in concrete classes.

    type JoinDeps = Map[TID, Set[RestartTarget]]
    type ReadDeps = Map[A, Set[RestartTarget]]
    type WriteDeps = Map[A, Set[RestartTarget]]

    type Edges = Map[State, Set[(Transition, State)]] // A map is used to overwrite any old edges that would remain present in a List or Set.
    type GraphEdges = List[(State, Transition, State)]
    type UnlabeledEdges = Map[State, Set[State]]

    /* Variables for benchmarking. */
    var theStore: VStore = Store.initial[A, V](t, sem.initialStore)(lattice)
    var theLabels: List[(Int, Map[Int, Int])] = List()

    /** Class collecting the dependencies of all threads. localWrites collects the addresses written in a current intra-module analysis. */
    case class Deps(joined: JoinDeps, read: ReadDeps, written: WriteDeps, localWrites: Set[A] = Set())

    /** Class used to return all information resulting from stepping this state. */
    case class StepResult(successors: Successors, created: Created, result: Option[V], effects: Effects, store: VStore, kstore: KStore) {
        def merge(acc: StepResult): StepResult = StepResult(successors ++ acc.successors,
                                                            created ++ acc.created,
                                                            Option.empty,
                                                            effects ++ acc.effects,
                                                            acc.store.join(store),
                                                            acc.kstore.join(kstore))
    }

    /**
      * The execution state of one process (thread), containing its identifier and running information.
      * The variable store is shared among all threads; the continuation store is not.
      *
      * @param tid     The thread identifier corresponding to this context.
      * @param control The control component of the thread.
      * @param cc      The address of the current continuation.
      * @param time    A timestamp.
      *                //@param kstore  A continuation store.
      */
    case class State(tid: TID, control: Control, cc: KAddr, time: T)
      extends GraphElement
        with SmartHash {
        override def toString: String = s" [$tid :: ${control.toString}] "

        override def label: String = toString

        override def color: Color = control match {
            case ControlEval(_, _) => Colors.Blue
            case ControlKont(_) if halted => Colors.Grass
            case ControlKont(_) => Colors.Yellow
            case ControlError(_) => Colors.Red
        }

        def isError: Boolean = control match {
            case ControlError(_) => true
            case _ => false
        }

        override def metadata =
            GraphMetadataMap(Map("halted" -> GraphMetadataBool(halted), "type" -> GraphMetadataString("AtomAnalysis")))

        /** Indicates whether this state is a final state. */
        def halted: Boolean = (cc, control) match {
            case (HaltKontAddr, ControlKont(_)) => true
            case (_, ControlError(_)) => true
            case _ => false
        }

        /** Indicates whether this state represents an error. */
        def errored: Boolean = control match {
            case ControlError(_) => true
            case _ => false
        }

        /** Helper function to create new results easily without having to write all fields explicitly. */
        private def newResult(successor: State, effects: Effects, store: VStore, kstore: KStore,
                              created: Created = Set.empty, result: Option[V] = Option.empty): StepResult = {
            StepResult(Set(successor), created, result, effects, store, kstore)
        }

        /**
          * Executes an action on this state and returns the result as well as other bookkeeping information in the form of a stepResults instance.
          *
          * @param action  The action to perform on this state.
          * @param old     The store that was used to obtain the action.
          * @param cc      The address of the current continuation. May differ from the instance variable cc stored in this state (e.g. after a frame has been popped of the stack).
          * @param results A map of TIDs to values, used to retrieve the return values of other processes.
          * @return Returns a stepResult containing all successor states of this states after performing the actions
          *         as well as bookkeeping information and the resulting store.
          */
        private def act(action: Act, time: T, old: VStore, kstore: KStore, cc: KAddr, results: RetVals): StepResult = action match {
            // The semantics reached a value => continue with this value.
            case Value(v, store, effs) => newResult(State(tid, ControlKont(v), cc, timestamp.tick(time)), effs, store, kstore)
            // A frame needs to be pushed on the stack and the evaluation needs to continue by evaluating 'e'.
            case Push(frame, e, env, store, effs) =>
                val cc_ = KontAddr(e, time)
                newResult(State(tid, ControlEval(e, env), cc_, timestamp.tick(time)), effs, store, kstore.extend(cc_, Set(Kont(frame, cc))))
            // The expression 'e' needs to be evaluated in the given environment.
            case Eval(e, env, store, effs) => newResult(State(tid, ControlEval(e, env), cc, timestamp.tick(time)), effs, store, kstore)
            // The evaluation steps into a function body. Same than Eval, except that the function is used to tick the timestamp.
            case StepIn(f, _, e, env, store, effs) => newResult(State(tid, ControlEval(e, env), cc, timestamp.tick(time, f)), effs, store, kstore)
            // The semantics reached an error, which needs to be returned.
            case Err(e) => newResult(State(tid, ControlError(e), cc, timestamp.tick(time)), Set.empty, old, kstore)
            // A new process is spawn by the semantics. The machine allocates a new TID and records the state of the new process.
            case NewFuture(ftid: TID@unchecked, tidv, fst, frame: Frame@unchecked, env, store, effs) =>
                val cc_ = KontAddr(fst, time)
                val newPState = State(ftid, ControlEval(fst, env), cc_, timestamp.initial(ftid.pos.toString))
                val curPState = State(tid, ControlKont(tidv), cc, timestamp.tick(time))
                StepResult(Set(curPState), Set(newPState), Option.empty, effs ++ Effects.spawn(ftid), store, kstore.extend(cc_, Set(Kont(frame, HaltKontAddr))))
            // The semantics wants to read a value from another thread, which needs to be looked up in the 'results' map.
            case DerefFuture(ftid: TID@unchecked, store, effs) =>
                StepResult(results.get(ftid).map(v => State(tid, ControlKont(v), cc, timestamp.tick(time))).toSet,
                           Set.empty, Option.empty, effs ++ Effects.join(ftid), store, kstore)
            // An unknown action has been returned. Should not happen, therefore this is an error.
            case a => throw new Exception(s"Unsupported action: $a.\n")
        }

        /**
          * Produces the states following this state by applying the given actions successively, thereby updating the store.
          *
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
          *
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
            case ControlEval(exp, env) => next(sem.stepEval(exp, env, store, time), store, kstore, cc, results)
            // The end of evaluation has been reached. Return the final result.
            case ControlKont(v) if cc == HaltKontAddr => StepResult(Set.empty, Set.empty, Some(v), Set.empty, store, kstore)
            // Continue with a given value, given the continuation frame in this state. Pops this frame of the stack.
            case ControlKont(v) =>
                val init: StepResult = StepResult(Set.empty, Set.empty, Option.empty, Set.empty, store, kstore)
                kstore.lookup(cc)
                  .foldLeft(init)((acc1, konts) => // Lookup all associated continuation frames.
                      konts.foldLeft(acc1) {
                          case (acc2, Kont(frame, cc_)) => // For each frame, generate the next actions and accumulate everything (starting from acc1).
                              next(sem.stepKont(v, frame, acc2.store, time), acc2.store, acc2.kstore, cc_, results).merge(acc2) // Note that the frame is popped of the stack by passing cc_.
                      })
            // Handle an error. This results in no successor states.
            case ControlError(_) => StepResult(Set.empty, Set.empty, None, Set.empty, store, kstore)
            // An unknown control component has been reached. Should not happen so throw an error.
            case e => throw new Exception(s"Unsupported control sequence: $e.\n")
        }
    }

    /**
      * Contains bookkeeping information for the inner loop of the algorithm.
      *
      * @param created The threads created by the current thread.
      * @param effects The effects generated during the evaluation of the current thread.
      * @param result  The result value of the thread (initially bottom).
      * @param edges   A list of graph edges.
      */
    case class InnerLoopState(stid: TID, work: Set[State], store: VStore, kstore: KStore, results: RetVals, deps: Deps,
                              visited: Set[State] = Set.empty, result: V = lattice.bottom, created: Created = Set.empty,
                              effects: Effects = Set.empty, edges: UnlabeledEdges = Map.empty) extends SmartHash

    def extractDependencies(stid: TID, deps: Deps, curState: State, effs: Set[Effect]): Deps

    def extract(target: RestartTarget, deps: Deps, effs: Set[Effect]): Deps = profile("extractDependencies") {
        effs.foldLeft(deps) {
            case (deps, JoinEff(tid: TID@unchecked)) => deps.copy(joined = deps.joined + (tid -> (deps.joined(tid) + target)))
            case (deps, ReadAddrEff(addr: A@unchecked)) => deps.copy(read = deps.read + (addr -> (deps.read(addr) + target)))
            case (deps, WriteAddrEff(addr: A@unchecked)) => deps.copy(written = deps.written + (addr -> (deps.written(addr) + target)), localWrites = deps.localWrites + addr)
            case (deps, _) => deps
        }
    }

    /**
      * Performs the analysis for a single thread.
      *
      * @param iState An InnerLoopState containing initial bookkeeping information. This argument can (should) be omitted.
      * @return A set of a new global store and an innerloopstate containing extra bookkeeping information for the outer loop.
      */
    @scala.annotation.tailrec
    final def innerLoop(timeout: Timeout.T, iState: InnerLoopState): InnerLoopState = {
        if (timeout.reached || iState.work.isEmpty) return iState
        intraRuns += 1
        intraIterations += 1
        innerLoop(
            timeout,
            //  profile("innerLoop") {
            iState.work.foldLeft(iState.copy(work = Set())) {
                case (iStateAcc, curState) =>
                    if (iStateAcc.visited.contains(curState))
                        iStateAcc // If the state has been explored already, do not take a step.
                    else {
                        // If the state has not been explored yet, take a step.
                        val StepResult(succs, crea, res, effs, sto, ksto) = curState.step(iStateAcc.store, iStateAcc.kstore, iStateAcc.results)
                        val deps = extractDependencies(iStateAcc.stid, iStateAcc.deps, curState, effs)
                        val vis =
                            if (sto.asInstanceOf[DeltaStore[A, V]].updated.nonEmpty || ksto.asInstanceOf[DeltaStore[KAddr, Set[Kont]]].updated.nonEmpty)
                                Set.empty[State] // Updates to the global stores -> clear visited sets.
                            else
                                iStateAcc.visited + curState // Immediately clear the visited set upon a store change.

                        InnerLoopState(iStateAcc.stid, iStateAcc.work ++ succs,
                                       sto.asInstanceOf[DeltaStore[A, V]].clearUpdated,
                                       ksto.asInstanceOf[DeltaStore[KAddr, Set[Kont]]].clearUpdated,
                                       iStateAcc.results,
                                       deps,
                                       vis,
                                       lattice.join(iStateAcc.result, res.getOrElse(lattice.bottom)),
                                       iStateAcc.created ++ crea,
                                       iStateAcc.effects ++ effs,
                                       iStateAcc.edges + (curState -> succs))
                    }
            }
            // }
        )
    }

    /**
      * Contains bookkeeping information for the outer loop of the algorithm.
      *
      * @param threads A map of thread identifiers to sets of initial thread states.
      * @param deps    The collection of dependencies inferred throughout the analysis.
      * @param results A map of tids to result values.
      * @param store   The store.
      * @param edges   The edges of the graph.
      */
    case class OuterLoopState(threads: Threads, work: Set[State], deps: Deps, results: RetVals, store: VStore, kstore: KStore, edges: Edges) extends SmartHash

    /** Converts a set of targets to a set of states (the initial states for these targets,
      * i.e. the states from which the analysis for the respective targets should start. */
    def convertToStates(targets: Set[RestartTarget], threads: Threads): Set[State]

    /** Retrieves the TIDs corresponding to the targets. */
    def getTID(target: RestartTarget): TID

    /**
      * Finds the targets that must be reanalysed based on a set of dependencies and an old and updated store.
      *
      * @param tid      The TID of the thread that was analysed, thereby converting oldStore to newStore. Used to exclude states for reanalysis.
      * @param deps     A collection of dependencies. Should contain the newest information.
      * @param oldStore The store <i>before</i> the thread TID was analysed.
      * @param newStore The store <i>after</i> the threads TID was analysed.
      * @return A set of targets that were impacted by updates to the store. The analysis should be restarted from these targets onwards.
      */
    @toCheck("Should we really use localWrites instead of deps.written? Logically yes, but have not seen a difference on other benchmarks.")
    def targetsImpactedByWrite(tid: TID, deps: Deps, oldStore: VStore, newStore: VStore): Set[RestartTarget] = profile("statesAddedFromEffects") {
        /* For all written addresses in the previous intra-module analysis: */
        deps.localWrites.foldLeft(Set[RestartTarget]())((acc, addr) =>
            if (oldStore.lookup(addr) == newStore.lookup(addr))
            /* if the address was updated but didn't actually change, do nothing; */
                acc
            else
            /* else, if its value changed, add all states from other threads that read the address. */
                acc ++ (deps.read(addr) ++ deps.written(addr)).filter(getTID(_) != tid) // This filter allows for more precision as otherwise more states are reanalysed using an enlarged store.

        )
    }

    var intraIterations = 0

    /**
      * Performs the fixed-point computation for the entire program. Uses the innerLoop method to analyse single threads and uses the results and dependencies returned from this
      * method to steer the computation.
      *
      * @param oState    An OuterLoopState to contain track of the dependencies between threads and the current global store.
      * @param iteration The current iteration of the outer loop. This argument can be omitted.
      * @return A Map of TIDs to graphs.
      */
    @scala.annotation.tailrec
    final def outerLoop(timeout: Timeout.T, oState: OuterLoopState, iteration: Int = 1): OuterLoopState = {
        if (timeout.reached || oState.work.isEmpty) return oState
        interRuns += 1
        outerLoop(
            timeout,
            //  profile("outerLoop") {
            oState.work.groupBy(_.tid) // TODO find best worklist order! (Can result in better performance for inc).
              .foldLeft(oState.copy(work = Set())) {
                  case (oStateAcc, (stid, curStates)) =>
                      //val stid: TID = curState.tid
                      intraIterations = 0
                      // The inner loop immediately updates the (k)store, result map and dependency maps.
                      val iState = innerLoop(timeout, InnerLoopState(stid, curStates, oStateAcc.store, oStateAcc.kstore, oStateAcc.results, oState.deps))
                      // println(s"[$iteration] Intra iterations for $stid: $intraIterations")
                      // todoCreated contains the initial states of threads that have never been explored. threads is updated accordingly to newThreads to register these new states.
                      val (todoCreated, newThreads): (Set[State], Threads) =
                      iState.created.foldLeft((Set[State](), oStateAcc.threads)) {
                          case ((createdAcc, threadsAcc), curState) =>
                              if (threadsAcc(curState.tid).contains(curState))
                                  (createdAcc, threadsAcc) // There already is an identical thread, so do nothing.
                              else
                                  (createdAcc + curState, threadsAcc + (curState.tid -> (threadsAcc(curState.tid) + curState)))
                      }
                      // Module dependencies have been updated and are available in iState.deps. TODO is iState.store the correct store to pass here for mod?
                      val todoWritten: Set[State] = convertToStates(targetsImpactedByWrite(stid, iState.deps, oStateAcc.store, iState.store), newThreads) // Could be oStateAcc.threads but would not be beneficial for precision.

                      // Join the old and new return value. If the return value changes, all other threads joining in this thread need to be reanalysed.
                      val retVal: V = lattice.join(oStateAcc.results(stid), iState.result)
                      val todoJoined: Set[State] =
                          if (oStateAcc.results(stid) == retVal) Set()
                          else convertToStates(iState.deps.joined(stid), newThreads) // Could be oStateAcc.threads but would not be beneficial for precision.
                      OuterLoopState(newThreads,
                                     oStateAcc.work ++ todoCreated ++ todoWritten ++ todoJoined,
                                     iState.deps.copy(localWrites = Set()),
                                     oStateAcc.results + (stid -> retVal),
                                     iState.store,
                                     iState.kstore,
                                     oStateAcc.edges ++ iState.edges.mapValues(set => set.map((BaseTransition(iteration.toString), _))))
              }
            //    },
            , iteration + 1)
    }

    var intraRuns: Long = 0
    var interRuns: Long = 0

    /**
      * Analyses a given program in a thread-modular way. Returns a state graph representing the collecting semantics of the program.
      *
      * @param program The program to be analysed.
      * @param timeout A timeout after which the analysis should stop to prevent looping.
      * @param ev      A implicit graph.
      * @tparam G The type of the graph to be returned.
      * @return Returns a state graph representing the collecting semantics of the program that was analysed.
      */
    def run[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
        intraRuns = 0
        interRuns = 0
        clearProfiler()

        /** Filters out unreachable graph components that may result from invalidating edges. */
        @scala.annotation.tailrec
        def findConnectedStates(work: Set[State], edges: Edges, visited: Set[State] = Set.empty, acc: GraphEdges = List.empty): GraphEdges = {
            if (timeout.reached || work.isEmpty) return acc
            if (visited.contains(work.head)) findConnectedStates(work.tail, edges, visited, acc)
            else {
                val head = work.head
                val next = edges(head)
                // Prepend the edges and work upfront the respective lists (assume next to be much shorter than work/acc).
                findConnectedStates(next.map(_._2) ++ work.tail, edges, visited + head, next.map(t => (head, t._1, t._2)).toList ++ acc)
            }
        }

        val cc: KAddr              = HaltKontAddr
        val env: Environment[A]    = Environment.initial[A](sem.initialEnv)
        val control: Control       = ControlEval(program, env)
        val kstore: KStore         = Store.initial[KAddr, Set[Kont]](t, Set.empty)
        val time: T                = timestamp.initial("")
        val tid: TID               = allocator.allocate(program, NoPosition, time)
        val state: State           = State(tid, control, cc, time)
        val threads: Threads       = Map(tid -> Set(state)).withDefaultValue(Set.empty)
        val vstore: VStore         = Store.initial[A, V](t, sem.initialStore)(lattice)
        //val t0 = System.nanoTime
        val oState: OuterLoopState = OuterLoopState(threads, // Threads
                                                    Set(state), // Worklist
                                                    Deps(Map.empty.withDefaultValue(Set.empty), // Join dependencies
                                                         Map.empty.withDefaultValue(Set.empty), // Read dependencies
                                                         Map.empty.withDefaultValue(Set.empty)), // Write dependencies
                                                         Map.empty.withDefaultValue(lattice.bottom), // Return values
                                                         vstore, // Store
                                                         kstore, // Continuation store
                                                         Map.empty // Graph edges
        )

        val result: OuterLoopState = outerLoop(timeout, oState)
        //val t1 = System.nanoTime

        //println(s"Total execution: ${(t1 - t0) / Math.pow(10, 9)}")
        //dumpProfile()
        theStore = result.store

        // After running the result, possibly unreachable edges may need to be filtered out.
        //val t2 = System.nanoTime
        val res = Graph[G, State, Transition].empty.addEdges(findConnectedStates(result.threads.values.flatten.toSet, result.edges))
        //val t3 = System.nanoTime
        //println(s"Total graph construction: ${(t3 - t2) / Math.pow(10, 9)}")
        res
    }

    def runWithLabels[G](program: Exp, timeout: Timeout.T)(implicit ev: Graph[G, State, Transition]): G = {
        intraRuns = 0
        interRuns = 0
        clearProfiler()

        /** Filters out unreachable graph components that may result from invalidating edges. */
        @scala.annotation.tailrec
        def findConnectedStates(timeout: Timeout.T, tid: TID, work: List[State], edges: Edges, visited: Set[State] = Set.empty,
                                acc: GraphEdges = List.empty, labels: Map[Int, Int] = Map().withDefaultValue(0)): (GraphEdges, Map[Int, Int]) = {
            if (timeout.reached || work.isEmpty) return (acc, labels)
            if (visited.contains(work.head)) findConnectedStates(timeout, tid, work.tail, edges, visited, acc, labels)
            else {
                val head = work.head
                val next = edges(head)
                val lbs = next.foldLeft(labels) { (lbs, e) =>
                    val i = e._1.l.toInt
                    lbs + (i -> (lbs(i) + 1))
            }
                // Prepend the edges and work upfront the respective lists (assume next to be much shorter than work/acc).
                findConnectedStates(timeout, tid, next.map(_._2).toList ++ work.tail, edges, visited + head,
                                    next.map(t => (head, t._1, t._2)).toList ++ acc, lbs)
            }
        }

        val cc: KAddr = HaltKontAddr
        val env: Environment[A]    = Environment.initial[A](sem.initialEnv)
        val control: Control       = ControlEval(program, env)
        val kstore: KStore         = Store.initial[KAddr, Set[Kont]](t, Set.empty)
        val time: T                = timestamp.initial("")
        val tid: TID               = allocator.allocate(program, NoPosition, time)
        val state: State           = State(tid, control, cc, time)
        val threads: Threads       = Map(tid -> Set(state)).withDefaultValue(Set.empty)
        val vstore: VStore         = Store.initial[A, V](t, sem.initialStore)(lattice)
        val t0 = System.nanoTime
        val oState: OuterLoopState = OuterLoopState(threads, // Threads
                                                    Set(state), // Worklist
                                                    Deps(Map.empty.withDefaultValue(Set.empty), // Join dependencies
                                                         Map.empty.withDefaultValue(Set.empty), // Read dependencies
                                                         Map.empty.withDefaultValue(Set.empty)), // Write dependencies
                                                    Map.empty.withDefaultValue(lattice.bottom), // Return values
                                                    vstore, // Store
                                                    kstore, // Continuation store
                                                    Map.empty) // Graph edges

        val result: OuterLoopState = outerLoop(timeout, oState)
        val t1 = System.nanoTime

        println(s"Total execution: ${(t1 - t0) / Math.pow(10, 9)}")
        dumpProfile()
        theStore = result.store

        // After running the result, possibly unreachable edges may need to be filtered out.
        val t2 = System.nanoTime
        val (edg, labels): (GraphEdges, List[(TID, Map[Int, Int])]) =
            result.threads.keySet
              .foldLeft((List[(State, Transition, State)](), List[(TID, Map[Int, Int])]())) {
                  case ((edg, lab), td) =>
                      val (e, l): (GraphEdges, Map[Int, Int]) =
                          findConnectedStates(timeout, td, result.threads(td).toList, result.edges)
                      (edg ++ e, (td, l) :: lab)
              }
        theLabels = labels
          .foldLeft((List[(Int, Map[Int, Int])](), 0)) {
              case ((acc, n), (_, mp)) => ((n, mp) :: acc, n + 1)
          }
          ._1
        val res = Graph[G, State, Transition].empty.addEdges(edg)
        val t3 = System.nanoTime
        println(s"Total graph construction: ${(t3 - t2) / Math.pow(10, 9)}")
        res
    }

    case class ProfileEntry(calls: Int, time: Long) {
        /* could also add statistics on time (mean, max, min) */
        def +(p: ProfileEntry): ProfileEntry = ProfileEntry(calls = calls + p.calls, time = time + p.time)
    }

    var profileData: Map[String, ProfileEntry] = Map().withDefaultValue(ProfileEntry(0, 0))

    def clearProfiler() = {
        profileData = Map().withDefaultValue(ProfileEntry(0, 0))
    }

    def profile[W](name: String)(what: => W) = {
        val t0 = System.nanoTime
        val res = what
        val t1 = System.nanoTime
        profileData = profileData + (name -> (profileData(name) + ProfileEntry(1, (t1 - t0))))
        res
    }

    def dumpProfile() = {
        profileData.foreach({ case (name, ProfileEntry(calls, time)) =>
            println(s"$name: $calls calls, took ${time.toDouble / Math.pow(10, 9)}s in total")
        })
        println(s"Intra runs: $intraRuns")
        println(s"Inter runs: $interRuns")
    }
}

class ModAtomAnalysis[Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])
                                                                       (implicit val timestamp2: Timestamp[T, Exp], implicit val lattice2: Lattice[V])
  extends AtomAnalysis[Exp, A, V, T, TID](t, sem, allocator) {
    type RestartTarget = TID

    def extractDependencies(stid: TID, deps: Deps, curState: State, effs: Set[Effect]): Deps = extract(stid, deps, effs)
    def convertToStates(tids: Set[TID], threads: Threads): Set[State] = tids.flatMap(threads)
    def getTID(tid: TID): TID = tid
}

class IncAtomAnalysis[Exp, A <: Address, V, T, TID <: ThreadIdentifier](t: StoreType, sem: Semantics[Exp, A, V, T, Exp], allocator: TIDAllocator[TID, T, Exp])
                                                                       (implicit val timestamp2: Timestamp[T, Exp], implicit val lattice2: Lattice[V])
  extends AtomAnalysis[Exp, A, V, T, TID](t, sem, allocator) {
    type RestartTarget = State

    def extractDependencies(stid: TID, deps: Deps, curState: State, effs: Set[Effect]): Deps = extract(curState, deps, effs)
    def convertToStates(states: Set[State], threads: Threads): Set[State] = states
    def getTID(state: State): TID = state.tid
}
