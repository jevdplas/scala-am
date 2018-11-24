package scalaam.core

/** A trait for thread identifiers. */
trait ThreadIdentifier extends SmartHash

/** Type for no threadIdentifiers. Can be used for sequential programs. */
case object NoTID extends ThreadIdentifier

/** Allocator used to allocate thread identifiers of type TID. */
trait TIDAllocator[TID <: ThreadIdentifier, T, C] {
    implicit val timestamp: Timestamp[T, C]
    
    /** Allocate a TID. */
    def allocate(exp: C, t: T): TID
}

object ConcreteTID {
    
    trait threadID extends ThreadIdentifier
    
    case class TID[T, C](exp: C, t: T) extends threadID {
        override def toString(): String = s"[$exp~$t]"
    }
    
    case class Alloc[T, C]()(implicit val timestamp: Timestamp[T, C]) extends TIDAllocator[threadID, T, C] {
        def allocate(exp: C, t: T): threadID = TID(exp, t)
    }
    
}

/**
  * A mapping from TIDS to contexts.<br><br>
  * Based on: https://github.com/acieroid/scala-am/blob/744a13a5b957c73a9d0aed6e10f7dae382c9b2e3/src/main/scala/machine/concurrent/ConcurrentAAM.scala#L94
  *
  * @tparam TID     A type of thread identifiers.
  * @tparam Context A type of contexts.
  * @tparam V       A type of values.
  * @param busy     A map of thread identifiers to contexts.
  * @param finished A map of thred identifiers to values.
  * @param lat      An implicit lattice parameter.
  */
case class TMap[TID, Context, V](busy: Map[TID, Set[Context]], finished: Map[TID, V])(implicit val lat: Lattice[V]) {
    def get(tid: TID): Set[Context] = busy(tid)
    
    def getResult(tid: TID): V = finished.getOrElse(tid, lat.bottom)
    
    def set(tid: TID, newContext: Context): TMap[TID, Context, V] = {
        val associatedContexts = get(tid)
        if (associatedContexts.size == 1)
            TMap(busy + (tid -> Set(newContext)), finished)
        else
            TMap(busy + (tid -> (associatedContexts + newContext)), finished)
    }
    
    def add(tid: TID, newContext: Context): TMap[TID, Context, V] =
        TMap(busy + (tid -> (get(tid) + newContext)), finished)
    
//    def remove(tid: TID): TMap[TID, Context, V] =
//        TMap(busy - tid, finished)
    
    def finish(tid: TID, v: V): TMap[TID, Context, V] = {
        TMap(busy - tid, finished + (tid -> lat.join(finished(tid), v)))
    }
    
    def finished(tid: TID): Boolean = finished.contains(tid)
    
    def threadsBusy(): Set[TID] = busy.keys.toSet
    def allDone(): Boolean = busy.isEmpty
    
    override def toString: String = {
       "Threads: (" +     busy.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + get(key).toString       + "]") + ")\n" +
       "Results: (" + finished.keys.foldLeft("")((acc, key) => acc + "[" + key.toString + " -> " + getResult(key).toString + "]") + ")"
    }
}