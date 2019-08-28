package scalaam.core

import scalaam.core.Annotations._

/** A trait for thread identifiers. */
trait ThreadIdentifier extends SmartHash

/** Type for no threadIdentifiers. Can be used for sequential programs. */
case object NoTID extends ThreadIdentifier

/** Allocator used to allocate thread identifiers of type TID. */
trait TIDAllocator[TID <: ThreadIdentifier, T, C] {
  implicit val timestamp: Timestamp[T, C]

  /** Allocate a TID. */
  def allocate[E](exp: E, t: T): TID
}

object ConcreteTID {

  var id: Int = 0

  trait threadID extends ThreadIdentifier

  case class TID[T, C](exp: C, t: T, n: Int) extends threadID {

    /** Prints this tid. As the tid contains the full expression, its hashcode is used to get a shorter but (normally) unique name. */
    override def toString: String = n.toString //exp.toString //s"*${exp.toString.hashCode}@$t*"
  }

  case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C])
      extends TIDAllocator[threadID, T, C] {
    def allocate[E](exp: E, t: T): threadID = {
      val cId = id
      id = id + 1
      TID(exp, t, cId)
    }
  }

}

object ExpTimeTID {

  trait threadID extends ThreadIdentifier

  case class TID[T, C](exp: C, t: T) extends threadID {

    /** Prints this tid. As the tid contains the full expression, its hashcode is used to get a shorter but (normally) unique name. */
    override def toString: String =
      exp.hashCode().toString //exp.toString //s"*${exp.toString.hashCode}@$t*"
  }

  case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C])
      extends TIDAllocator[threadID, T, C] {
    def allocate[E](exp: E, t: T): threadID = TID(exp, t)

  }
}

/**
  * A mapping from TIDS to contexts.<br><br>
  * Based on: https://github.com/acieroid/scala-am/blob/744a13a5b957c73a9d0aed6e10f7dae382c9b2e3/src/main/scala/machine/concurrent/ConcurrentAAM.scala#L94
  *
  * @tparam TID     A type of thread identifiers.
  * @tparam Context A type of contexts.
  * @tparam V       A type of values.
  * @param busy     A map of thread identifiers to sets of contexts.
  * @param finished A map of thread identifiers to values.
  * @param errored  A map of thread identifiers to errors.
  * @param lat      An implicit lattice parameter.
  */
case class TMap[TID, Context, V](
    busy: Map[TID, Set[Context]],
    finished: Map[TID, V],
    errored: Map[TID, Set[Error]]
)(implicit val lat: Lattice[V]) {
  def get(tid: TID): Set[Context] = busy.getOrElse(tid, Set())

  def getResult(tid: TID): V = finished.getOrElse(tid, lat.bottom)

  def getError(tid: TID): Set[Error] = errored.getOrElse(tid, Set())

  def set(tid: TID, newContext: Context): TMap[TID, Context, V] = {
    val associatedContexts = get(tid)
    if (associatedContexts.size == 1)
      TMap(busy + (tid -> Set(newContext)), finished, errored)
    else
      TMap(busy + (tid -> (associatedContexts + newContext)), finished, errored)
  }

  def add(tid: TID, newContext: Context): TMap[TID, Context, V] =
    TMap(busy + (tid -> (get(tid) + newContext)), finished, errored)

  @unsound("Multiple contexts may be associated to TID.")
  def finish(tid: TID, v: V): TMap[TID, Context, V] = {
    if (get(tid).isEmpty) {
      throw new Exception(
        s"Invalid state: trying to terminate non-existing process $tid with value $v."
      )
    }
    TMap(busy - tid, finished + (tid -> lat.join(getResult(tid), v)), errored)
  }

  @unsound
  def fail(tid: TID, e: Error): TMap[TID, Context, V] = {
    if (get(tid).isEmpty) {
      throw new Exception(
        s"Invalid state: trying to terminate non-existing process $tid with error $e."
      )
    }
    TMap(busy - tid, finished, errored + (tid -> (getError(tid) + e)))
  }

  def hasFinished(tid: TID): Boolean = {
    if (finished.contains(tid) || errored.contains(tid))
      return true
    if (get(tid).isEmpty)
      throw new Exception(
        s"Invalid state: trying to get information about non-existing process: $tid."
      )
    false
  }

  def threadsBusy(): Set[TID] = busy.keys.toSet

  def allDone(): Boolean = busy.isEmpty

  override def toString: String = {
    "Threads: (" + busy.keys.foldLeft("")(
      (acc, key) => acc + "[" + key.toString + " -> " + get(key).toString + "]"
    ) + ")\n" //+
    //"Results: (" + finished.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + getResult(key).toString + "]") + ")\n" +
    //"Errors:  (" + errored.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + getError(key).toString + "]") + ")"
  }
}

case class BlockableTMap[TID, Context, V](
    runnable: Map[TID, Set[Context]],
    blocked: Map[TID, Set[(TID, Context)]] = Map[TID, Set[(TID, Context)]](),
    blockLog: Set[TID] = Set[TID](),
    finished: Map[TID, V] = Map[TID, V](),
    errored: Map[TID, Set[Error]] = Map[TID, Set[Error]]()
)(implicit val lat: Lattice[V]) {

  def getRunnable(tid: TID): Set[Context] = runnable.getOrElse(tid, Set())

  def getBlocked(tid: TID): Set[(TID, Context)] = blocked.getOrElse(tid, Set())

  def getResult(tid: TID): V = finished.getOrElse(tid, lat.bottom)

  def getError(tid: TID): Set[Error] = errored.getOrElse(tid, Set())

  def newThread(tid: TID, newContext: Context): BlockableTMap[TID, Context, V] = {
    if (getRunnable(tid).nonEmpty || getBlocked(tid).nonEmpty || getError(tid).nonEmpty)
      throw new Exception(s"Attempting to reuse existing TID $tid - Execution is not concrete.")
    BlockableTMap(
      runnable + (tid -> (getRunnable(tid) + newContext)),
      blocked,
      blockLog,
      finished,
      errored
    )
  }

  @maybeUnsound("Verify that it is sound to remove oldContext.")
  def updateThread(
      tid: TID,
      oldContext: Context,
      newContext: Context
  ): BlockableTMap[TID, Context, V] = {
    val associatedContexts = getRunnable(tid)
    if (associatedContexts.isEmpty)
      throw new Exception(s"Updating non-runnable TID: $tid.") // Can be omitted: indicates machine error.
    else if (associatedContexts.size == 1)
      this.copy(runnable = runnable + (tid -> Set(newContext)))
    else {
      this.copy(runnable = runnable + (tid -> (associatedContexts - oldContext + newContext))) // TODO: verify that it is sound to remove oldContext.
    }
  }

  @unsound
  def finishThread(tid: TID, context: Context, result: V): BlockableTMap[TID, Context, V] = {
    val associatedContexts = getRunnable(tid)
    val canRun             = getBlocked(tid).map { case (tid, ctx) => tid -> (getRunnable(tid) + ctx) }
    if (associatedContexts.isEmpty) throw new Exception(s"Finishing non-runnable TID: $tid.") // Can be omitted: indicates machine error.
    // else if (associatedContexts.size == 1)
    this.copy(
      runnable = runnable - tid ++ canRun,
      finished = finished + (tid -> lat.join(getResult(tid), result)),
      blocked = blocked - tid,
      blockLog = blockLog - tid
    )
    // else
    //     this.copy(runnable = runnable + (tid -> (associatedContexts - context)) ++ canRun, finished = finished + (tid -> lat.join(getResult(tid), result)), blocked = blocked - tid, blockLog = blockLog - tid)
  }

  @unsound
  def failThread(tid: TID, context: Context, error: Error): BlockableTMap[TID, Context, V] = {
    val associatedContexts = getRunnable(tid)
    if (associatedContexts.isEmpty) throw new Exception(s"Failing non-runnable TID: $tid.") // Can be omitted: indicates machine error.
    // else if (associatedContexts.size == 1)
    this.copy(runnable = runnable - tid, errored = errored + (tid -> (getError(tid) + error)))
    // else
    //     this.copy(runnable = runnable + (tid -> (associatedContexts - context)), errored = errored + (tid -> (getError(tid) + error)))
  }

  @unsound
  def blockThread(tid: TID, context: Context, waitFor: TID): BlockableTMap[TID, Context, V] = {
    val associatedContexts = getRunnable(tid)
    if (associatedContexts.isEmpty) throw new Exception(s"Blocking non-runnable TID: $tid.") // Can be omitted: indicates machine error.
    // else if (associatedContexts.size == 1)
    this.copy(
      runnable = runnable - tid,
      blocked = blocked + (waitFor -> (getBlocked(tid) + ((tid, context)))),
      blockLog = blockLog + tid
    )
    // else
    //     this.copy(runnable = runnable + (tid -> (associatedContexts - context)), blocked = blocked + (waitFor -> (getBlocked(tid) + ((tid, context)))), blockLog = blockLog + tid)
  }

  def threadFinished(tid: TID): Boolean = {
    if (finished.contains(tid) || errored.contains(tid))
      return true
    if (getRunnable(tid).isEmpty && !blockLog.contains(tid))
      throw new Exception(s"Trying to get information about non-existing TID: $tid.")
    false
  }

  def threadsRunnable = runnable.keySet

  def allDone: Boolean = runnable.isEmpty && blocked.isEmpty
  def canRun: Boolean  = runnable.isEmpty

  override def toString: String =
    runnable.keys.foldLeft("")(
      (acc, tid) =>
        acc ++ s"($tid -> " ++ getRunnable(tid)
          .foldLeft("")((acc2, context) => acc2 ++ " " ++ context.toString) ++ ")"
    )
}
