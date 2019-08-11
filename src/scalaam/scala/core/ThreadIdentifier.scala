package scalaam.core

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
    
    trait threadID extends ThreadIdentifier
    
    /** Prints this tid. As the tid contains the full expression, its hashcode is used to get a shorter but (normally) unique name. */
    case class TID[T, C](exp: C, t: T) extends threadID {
        override def toString: String = exp.hashCode().toString//exp.toString //s"*${exp.toString.hashCode}@$t*"
    }
    
    case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends TIDAllocator[threadID, T, C] {
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
case class TMap[TID, Context, V](busy: Map[TID, Set[Context]], finished: Map[TID, V], errored: Map[TID, Set[Error]])(implicit val lat: Lattice[V]) {
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
    
    def finish(tid: TID, v: V): TMap[TID, Context, V] = {
        if (get(tid).isEmpty) {
            throw new Exception(s"Invalid state: trying to terminate non-existing process $tid with value $v.")
        }
        TMap(busy - tid, finished + (tid -> lat.join(getResult(tid), v)), errored)
    }
    
    def fail(tid: TID, e: Error): TMap[TID, Context, V] = {
        if (get(tid).isEmpty) {
            throw new Exception(s"Invalid state: trying to terminate non-existing process $tid with error $e.")
        }
        TMap(busy - tid, finished, errored + (tid -> (getError(tid) + e)))
    }
    
    def hasFinished(tid: TID): Boolean = {
        if(finished.contains(tid) || errored.contains(tid))
            return true
        if(get(tid).isEmpty)
            throw new Exception(s"Invalid state: trying to get information about non-existing process: $tid.")
        false
    }
    
    def threadsBusy(): Set[TID] = busy.keys.toSet
    
    def allDone(): Boolean = busy.isEmpty
    
    override def toString: String = {
        "Threads: (" + busy.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + get(key).toString + "]") + ")\n" //+
            //"Results: (" + finished.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + getResult(key).toString + "]") + ")\n" +
            //"Errors:  (" + errored.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + getError(key).toString + "]") + ")"
    }
}