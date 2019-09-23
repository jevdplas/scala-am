package scalaam.lattice

import scalaam.core.{Lattice, ConcreteVal}
import scalaam.core.ConcreteVal._
import scalaam.util.Show

object Concrete {
  sealed trait L[+X] {
    def foldMap[Y: Lattice](f: X => Y): Y = this match {
      case Top => Lattice[Y].top
      case Values(content) =>
        content.foldLeft(Lattice[Y].bottom)((acc, v) => Lattice[Y].join(acc, f(v)))
    }
    def map[Y](f: X => Y): L[Y] = this match {
      case Top             => Top
      case Values(content) => Values(content.map(f))
    }
    def guardBot[Y: Lattice](body: => Y): Y = this match {
      case Values(content) if content.isEmpty => Lattice[Y].bottom
      case _                                  => body
    }
  }
  case object Top                       extends L[Nothing]
  case class Values[X](content: Set[X]) extends L[X]

  abstract class BaseInstance[A: Show](typeName: String) extends Lattice[L[A]] {
    def show(x: L[A]): String = x match {
      case Top                                  => typeName
      case Values(content) if content.size == 1 => Show[A].show(content.head)
      case Values(content)                      => "{" + content.map(Show[A].show).mkString(",") + "}"
    }
    val top: L[A]    = Top
    val bottom: L[A] = Values[A](Set.empty)
    def join(x: L[A], y: => L[A]): L[A] = x match {
      case Top => Top
      case Values(content1) =>
        y match {
          case Top              => Top
          case Values(content2) => Values(content1.union(content2))
        }
    }
    def subsumes(x: L[A], y: => L[A]) = x match {
      case Top => true
      case Values(content1) =>
        y match {
          case Top              => false
          case Values(content2) => content2.subsetOf(content1)
        }
    }
    def eql[B2: BoolLattice](x: L[A], y: L[A]): B2 = y.guardBot {
      x.foldMap(a => y.foldMap(b => BoolLattice[B2].inject(a == b)))
    }
  }

  type S   = L[String]
  type Sym = L[String]
  type B   = L[Boolean]
  type I   = L[Int]
  type R   = L[Double]
  type C   = L[Char]
  /* TODO[easy]: the bool lattice implementation could be specialized (see the old "ConcreteBoolEfficient" implementation). Whether this results in a speed improvement should be evaluated */

  object L {
    import scalaam.lattice._

    implicit class FoldMapOps[X](content: Set[X]) {
      def foldMap[Y: Lattice](f: X => Y): Y =
        content.foldLeft(Lattice[Y].bottom)((acc, v) => Lattice[Y].join(acc, f(v)))
    }

    implicit val stringShow: Show[String] = new Show[String] {
      def show(s: String): String = "\"" + s + "\""
    }
    implicit val stringConcrete: StringLattice[S] = new BaseInstance[String]("Str")
    with StringLattice[S] {
      def inject(x: String): S             = Values(Set(x))
      def length[I2: IntLattice](s: S): I2 = s.foldMap(s => IntLattice[I2].inject(s.length))
      def append(s1: S, s2: S): S = (s1, s2) match {
        case (Values(bot), _) if bot.isEmpty => Values(bot)
        case (_, Values(bot)) if bot.isEmpty => Values(bot)
        case (Top, _) | (_, Top)             => Top
        case (Values(content1), Values(content2)) =>
          Values(content1.foldMap(s1 => content2.map(s2 => s1 + s2)))
      }
      def lt[B2: BoolLattice](s1: S, s2: S): B2 = (s1, s2) match {
        case (Values(bot), _) if bot.isEmpty => BoolLattice[B2].bottom
        case (_, Values(bot)) if bot.isEmpty => BoolLattice[B2].bottom
        case (Top, _) | (_, Top)             => BoolLattice[B2].top
        case (Values(content1), Values(content2)) =>
          content1.foldMap(s1 => content2.foldMap(s2 => BoolLattice[B2].inject(s1 < s2)))
      }
      def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = s.foldMap(s => SymbolLattice[Sym2].inject(s))
      override def concreteValues(x: S): Set[ConcreteVal] = x match {
        case Top             => ???
        case Values(content) => content.map(x => ConcreteString(x))
      }
    }
    implicit val boolShow: Show[Boolean] = new Show[Boolean] {
      def show(b: Boolean): String =
        if (b) {
          "#t"
        } else {
          "#f"
        }
    }
    implicit val boolConcrete: BoolLattice[B] = new BaseInstance[Boolean]("Bool")
    with BoolLattice[B] {
      def inject(x: Boolean): B = Values(Set(x))
      def isTrue(b: B): Boolean = b match {
        case Top             => true
        case Values(content) => content.contains(true)
      }
      def isFalse(b: B): Boolean = b match {
        case Top             => true
        case Values(content) => content.contains(false)
      }
      def not(b: B): B = b.map(x => !x)
      override def concreteValues(x: B): Set[ConcreteVal] = x match {
        case Top             => ???
        case Values(content) => content.map(x => ConcreteBool(x))
      }
    }

    implicit val intShow: Show[Int] = new Show[Int] {
      def show(i: Int): String = s"$i"
    }
    implicit val intConcrete: IntLattice[I] = new BaseInstance[Int]("Int") with IntLattice[I] {
      def inject(x: Int): I = Values(Set(x))
      def toReal[R2: RealLattice](n: I): R2 = n match {
        case Top             => RealLattice[R2].top
        case Values(content) => content.foldMap((n: Int) => RealLattice[R2].inject(n.toDouble))
      }
      def random(n: I): I           = n.map(n => MathOps.random(n))
      def plus(n1: I, n2: I): I     = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 + n2)) }
      def minus(n1: I, n2: I): I    = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 - n2)) }
      def times(n1: I, n2: I): I    = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 * n2)) }
      def quotient(n1: I, n2: I): I = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 / n2)) }
      def div[R2: RealLattice](n1: I, n2: I): R2 = n2.guardBot {
        n1.foldMap(n1 => n2.foldMap(n2 => RealLattice[R2].inject(n1 / n2.toDouble)))
      }
      def modulo(n1: I, n2: I): I = n2.guardBot {
        n1.foldMap(n1 => n2.map(n2 => MathOps.modulo(n1, n2)))
      }
      def remainder(n1: I, n2: I): I = n2.guardBot {
        n1.foldMap(n1 => n2.map(n2 => MathOps.remainder(n1, n2)))
      }
      def lt[B2: BoolLattice](n1: I, n2: I): B2 = n2.guardBot {
        n1.foldMap(n1 => n2.foldMap(n2 => BoolLattice[B2].inject(n1 < n2)))
      }
      def toString[S2: StringLattice](n: I): S2 =
        n.foldMap(n => StringLattice[S2].inject(n.toString))

      override def concreteValues(x: I): Set[ConcreteVal] = x match {
        case Top             => ???
        case Values(content) => content.map(x => ConcreteNumber(x))
      }
    }

    implicit val doubleShow: Show[Double] = new Show[Double] {
      def show(d: Double): String = s"$d"
    }
    implicit val realConcrete: RealLattice[R] = new BaseInstance[Double]("Real")
    with RealLattice[R] {
      def inject(x: Double): R            = Values(Set(x))
      def toInt[I2: IntLattice](n: R): I2 = n.foldMap(n => IntLattice[I2].inject(n.toInt))
      def ceiling(n: R): R                = n.map(_.ceil)
      def floor(n: R): R                  = n.map(_.floor)
      def round(n: R): R                  = n.map(n => MathOps.round(n))
      def log(n: R): R                    = n.map(n => scala.math.log(n))
      def random(n: R): R                 = n.map(n => MathOps.random(n))
      def sin(n: R): R                    = n.map(n => scala.math.sin(n))
      def asin(n: R): R                   = n.map(n => scala.math.asin(n))
      def cos(n: R): R                    = n.map(n => scala.math.cos(n))
      def acos(n: R): R                   = n.map(n => scala.math.acos(n))
      def tan(n: R): R =
        n.map(n => scala.math.sin(n) / scala.math.cos(n)) /* scala.math.tan isn't precise enough */
      def atan(n: R): R          = n.map(n => scala.math.atan(n))
      def sqrt(n: R): R          = n.map(n => scala.math.sqrt(n))
      def plus(n1: R, n2: R): R  = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 + n2)) }
      def minus(n1: R, n2: R): R = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 - n2)) }
      def times(n1: R, n2: R): R = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 * n2)) }
      def div(n1: R, n2: R): R   = n2.guardBot { n1.foldMap(n1 => n2.map(n2 => n1 / n2)) }
      def lt[B2: BoolLattice](n1: R, n2: R): B2 = n2.guardBot {
        n1.foldMap(n1 => n2.foldMap(n2 => BoolLattice[B2].inject(n1 < n2)))
      }
      def toString[S2: StringLattice](n: R): S2 =
        n.foldMap(n => StringLattice[S2].inject(n.toString))
      override def concreteValues(x: R): Set[ConcreteVal] = x match {
        case Top             => ???
        case Values(content) => content.map(x => ConcreteReal(x))
      }
    }
    implicit val charShow: Show[Char] = new Show[Char] {
      def show(c: Char): String = s"#\\$c"
    }
    implicit val charConcrete: CharLattice[C] = new BaseInstance[Char]("Char") with CharLattice[C] {
      def inject(x: Char): C = Values(Set(x))
      override def concreteValues(x: C): Set[ConcreteVal] = x match {
        case Top             => ???
        case Values(content) => content.map(x => ConcreteChar(x))
      }
    }
    implicit val symConcrete: SymbolLattice[Sym] = new BaseInstance[String]("Sym")
    with SymbolLattice[Sym] {
      def inject(x: String): Sym                  = Values(Set(x))
      def toString[S2: StringLattice](s: Sym): S2 = s.foldMap(s => StringLattice[S2].inject(s))
      override def concreteValues(x: Sym): Set[ConcreteVal] = x match {
        case Top             => ???
        case Values(content) => content.map(x => ConcreteSymbol(x))
      }
    }
  }
}
