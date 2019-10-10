package scalaam.core

import scalaam.core.Annotations.toCheck
import scalaam.core.StoreType.StoreType

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash {

  //def content: Map[A, V]

  /** Gets all the keys of the store */
  def keys: Iterable[A]

  /** Restrict the store to only certain keys */
  def restrictTo(keys: Set[A]): Store[A, V]

  /** Checks if a predicate is true for all elements of the store */
  def forall(p: ((A, V)) => Boolean): Boolean

  /** Looks up a value in the store */
  def lookup(a: A): Option[V]

  def lookupDefault(a: A, default: V): V = lookup(a) match {
    case Some(a) => a
    case None    => default
  }
  def lookupMF(a: A): MayFail[V, Error] = lookup(a) match {
    case Some(a) => MayFail.success(a)
    case None    => MayFail.failure(UnboundAddress(a))
  }

  /** Add a new entry in the store */
  def extend(a: A, v: V): Store[A, V]

  /** Update an entry in the store */
  def update(a: A, v: V): Store[A, V] = extend(a, v)

  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: A, v: V): Store[A, V] = extend(a, v)

  /** Joins two stores together */
  def join(that: Store[A, V]): Store[A, V]

  /** Checks whether this store subsumes another store */
  def subsumes(that: Store[A, V]): Boolean
}

/** Basic store with no fancy feature, just a map from addresses to values */
case class BasicStore[A <: Address, V](val content: Map[A, V])(implicit val lat: Lattice[V])
    extends Store[A, V] {

  override def toString              = content.view.filterKeys(_.printable).mkString("\n")
  def keys                           = content.keys
  def restrictTo(keys: Set[A])       = BasicStore(content.view.filterKeys(a => keys.contains(a)).toMap)
  def forall(p: ((A, V)) => Boolean) = content.forall({ case (a, v) => p((a, v)) })
  def lookup(a: A)                   = content.get(a)
  def extend(a: A, v: V) = content.get(a) match {
    case None     => new BasicStore[A, V](content + (a -> v))
    case Some(v2) => new BasicStore[A, V](content + (a -> lat.join(v, v2)))
  }

  def update(a: A, v: V) = extend(a, v)

  def updateOrExtend(a: A, v: V) = extend(a, v)

  def join(that: Store[A, V]) =
    if (that.isInstanceOf[BasicStore[A, V]]) {
      keys.foldLeft(that)((acc, k) => lookup(k).fold(acc)(v => acc.extend(k, v)))
    } else {
      throw new RuntimeException(
        s"Trying to join incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}"
      )
    }

  def subsumes(that: Store[A, V]) =
    that.forall(
      (binding: (A, V)) => content.get(binding._1).exists(v => lat.subsumes(v, binding._2))
    )
}

case class ConcreteStore[A <: Address, V](content: Map[A, V])(implicit val lat: Lattice[V])
    extends Store[A, V] {

  /* Copied from BasicStore. */

  override def toString = content.filterKeys(_.printable).mkString("\n")

  def keys = content.keys

  def forall(p: ((A, V)) => Boolean) = content.forall({ case (a, v) => p((a, v)) })

  def lookup(a: A) = content.get(a)

  def lookupMF(a: A) = content.get(a) match {
    case Some(v) => MayFail.success(v)
    case None    => MayFail.failure(UnboundAddress(a))
  }

  def subsumes(that: Store[A, V]) =
    that.forall(
      (binding: (A, V)) => content.get(binding._1).exists(v => lat.subsumes(v, binding._2))
    )

  /* Specific to ConcreteStore. */

  @toCheck("Disabled if.")
  override def extend(a: A, v: V): ConcreteStore[A, V] = {
    /*if (content.get(a).isDefined) {
      println(s"store.extend imprecision")
    }*/
    ConcreteStore(content + (a -> v))
  }

  override def update(a: A, v: V): ConcreteStore[A, V] = ConcreteStore(content + (a -> v))

  override def updateOrExtend(a: A, v: V): ConcreteStore[A, V] = content.get(a) match {
    case None    => extend(a, v)
    case Some(_) => update(a, v)
  }

  override def join(that: Store[A, V]) = throw new Exception("Attempting to join concrete stores.")

}

trait Count

case object COne extends Count

case object CInf extends Count

/** Counting store */
// Based on https://github.com/acieroid/scala-am/blob/e435e01b9aa7f7e784dfbbb7b49599ef8464e05a/src/main/scala/core/Store.scala#L116
case class CountingStore[A <: Address, V](content: Map[A, (Count, V)])(implicit lat: Lattice[V])
    extends Store[A, V] {

  def keys = content.keys

  def forall(p: ((A, V)) => Boolean): Boolean = content.forall({ case (a, (_, v)) => p((a, v)) })

  def lookup(a: A): Option[V] = content.get(a).map(_._2)

  def lookupMF(a: A): MayFail[V, Error] = content.get(a) match {
    case Some((_, a)) => MayFail.success(a)
    case None         => MayFail.failure(UnboundAddress(a))
  }

  def extend(a: A, v: V): CountingStore[A, V] = content.get(a) match {
    case None           => this.copy(content = content + (a -> ((COne, v))))
    case Some((_, old)) => this.copy(content = content + (a -> ((CInf, lat.join(old, v)))))
  }

  def update(a: A, v: V): CountingStore[A, V] = content.get(a) match {
    case None            => throw new RuntimeException(s"Storeupdate at non-existent index: $a -> $v.")
    case Some((COne, _)) => this.copy(content = content + (a -> ((COne, v))))
    case _               => extend(a, v)
  }

  def updateOrExtend(a: A, v: V): CountingStore[A, V] = content.get(a) match {
    case None => extend(a, v)
    case _    => update(a, v)
  }

  def join(that: Store[A, V]): CountingStore[A, V] =
    if (that.isInstanceOf[CountingStore[A, V]]) {
      that.keys.foldLeft(this)((acc, k) => that.lookup(k).fold(acc)(v => acc.extend(k, v)))
    } else {
      throw new RuntimeException(
        s"Trying to join incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}"
      )
    }

  def subsumes(that: Store[A, V]): Boolean = {
    that.forall({
      case (a, v) =>
        lookup(a) match {
          case None     => false
          case Some(vv) => lat.subsumes(vv, v)
        }
    })
  }
}

/** This is a delta store. It behaves just like a store, but stores every element that has changed into `d`. Note that the view on `content` is always up to date */
case class DeltaStore[A <: Address, V](val content: Map[A, V], val updated: Set[A])(
    implicit val lat: Lattice[V]
) extends Store[A, V] {
  override def toString = content.filterKeys(_.printable).mkString("\n")

  def keys                           = content.keys
  def forall(p: ((A, V)) => Boolean) = content.forall({ case (a, v) => p((a, v)) })
  def lookup(a: A)                   = content.get(a)
  def lookupMF(a: A) = content.get(a) match {
    case Some(a) => MayFail.success(a)
    case None    => MayFail.failure(UnboundAddress(a))
  }
  def extend(a: A, v: V) = content.get(a) match {
    case None                            => new DeltaStore[A, V](content + (a -> v), updated + a)
    case Some(v2) if lat.subsumes(v2, v) => this
    case Some(v2)                        => new DeltaStore[A, V](content + (a -> (lat.join(v, v2))), updated + a)
  }
  def update(a: A, v: V)         = extend(a, v)
  def updateOrExtend(a: A, v: V) = extend(a, v)

  def join(that: Store[A, V]) =
    if (that.isInstanceOf[DeltaStore[A, V]])
      updated.foldLeft(that)((acc, k) => lookup(k).fold(acc)(v => acc.extend(k, v)))
    else
      throw new RuntimeException(s"Trying to join incompatible stores: ${this.getClass.getSimpleName} and ${that.getClass.getSimpleName}")

  def clearUpdated: DeltaStore[A, V] = new DeltaStore(content, Set.empty[A])

  def subsumes(that: Store[A, V]) =
    that.forall(
      (binding: (A, V)) => content.get(binding._1).exists(v => lat.subsumes(v, binding._2))
    )
}

/** An enumeration of the store types. */
object StoreType extends Enumeration {
  type StoreType = Value
  val BasicStore, ConcreteStore, CountingStore, DeltaStore = Value
}

object Store {
  def empty[A <: Address, V: Lattice](t: StoreType): Store[A, V] = t match {
    case StoreType.BasicStore    => BasicStore(Map())
    case StoreType.ConcreteStore => ConcreteStore(Map())
    case StoreType.CountingStore => new CountingStore(Map())
    case StoreType.DeltaStore    => new DeltaStore(Map(), Set())
  }

  def initial[A <: Address, V: Lattice](t: StoreType, values: Iterable[(A, V)]): Store[A, V] =
    t match {
      case StoreType.BasicStore    => BasicStore(values.toMap)
      case StoreType.ConcreteStore => ConcreteStore(values.toMap)
      case StoreType.CountingStore =>
        values.foldLeft(empty[A, V](StoreType.CountingStore))({
          case (acc, (a, v)) => acc.updateOrExtend(a, v)
        })
      case StoreType.DeltaStore => new DeltaStore(values.toMap, Set())
    }
}

object Profiler {
  val methodsCount: scala.collection.mutable.Map[String, Int] =
    scala.collection.mutable.Map().withDefaultValue(0)
  val methodsTotalTime: scala.collection.mutable.Map[String, Long] =
    scala.collection.mutable.Map().withDefaultValue(0)
  def profileCall[T](name: String)(f: => T): T = {
    val t0  = System.nanoTime()
    val res = f
    val t1  = System.nanoTime()
    methodsCount += (name     -> (methodsCount(name) + 1))
    methodsTotalTime += (name -> (methodsTotalTime(name) + (t1 - t0)))
    res
  }
  def printResults(): Unit = {
    // TODO: sort results per total time?
    methodsCount.keySet.foreach(
      k =>
        println(
          s"$k has been called ${methodsCount(k)} times for a total time of ${methodsTotalTime(k) / 1000000}ms"
        )
    )
  }
}

/*
case class ProfiledStore[A <: Address, V](val store: Store[A, V]) extends Store[A, V] {
  import Profiler.profileCall
  override def toString = profileCall("store.toString") { store.toString }
  def content           = profileCall("store.content") { store.content }
  def keys              = profileCall("store.keys") { store.keys }
  def restrictTo(keys: Set[A]) = profileCall("store.restrictTo") {
    ProfiledStore(store.restrictTo(keys))
  }
  def forall(p: ((A, V)) => Boolean) = profileCall("store.forall") { store.forall(p) }
  def lookup(a: A)                   = profileCall("store.lookup") { store.lookup(a) }
  def extend(a: A, v: V)             = profileCall("store.extend") { ProfiledStore(store.extend(a, v)) }
  def join(that: Store[A, V]) = profileCall("store.join") {
    that match {
      case ProfiledStore(store2) => ProfiledStore(store.join(store2))
      case _                     => ProfiledStore(store.join(that))
    }
  }
  def subsumes(that: Store[A, V]) = profileCall("store.subsumes") { store.subsumes(that) }
}
 */
