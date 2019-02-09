package nest

import scalaz.Scalaz.unfold
import reflect.runtime.universe.TypeTag

case object EmptyNest extends Nest[Nothing, Nothing]
private[nest] final case class NestCons[+A, +B](pair: Pair[A, B], pairs: List[Pair[A, B]]) extends Nest[A, B]

final case class NestWithoutRightB[A, B](nest: Nest[A, B], a: A) {
  def >>(b: B): Nest[A, B] = nest match {
    case EmptyNest      => Nest(a, b)
    case NestCons(h, t) => NestCons(AB(a, b), h :: t)
  }
}

final case class NestWithoutRightA[A, B](nest: Nest[A, B], b: B) {
  def >>(a: A): Nest[A, B] = nest match {
    case EmptyNest      => NestCons(BA(b, a), Nil)
    case NestCons(h, t) => NestCons(AB(a, b), h :: t)
  }
}

private[nest] trait Pair[+A, +B]{
  def toTuple: Either[(A, B), (B, A)] = this match {
    case AB(a, b) => Left (a, b)
    case BA(b, a) => Right(b, a)
  }
}
private object Pair {
  // Not returning Pair because of type erasure
  def apply[A, B](a: A, b: B): AB[A, B] = AB(a, b)
  def apply[A, B](b: B, a: A): BA[A, B] = BA(b, a)
}
private[nest] case class AB[A, B] (a: A, b: B) extends Pair[A, B]
object AB {
  def apply[A, B](pair: (A, B)): AB[A, B] = AB(pair._1, pair._2)
}
private[nest] case class BA[A, B] (b: B, a: A) extends Pair[A, B]
object BA {
  def apply[A, B](pair: (B, A)): BA[A, B] = BA(pair._1, pair._2)
}

object Nest {
  def apply[A, B](pair: (A, B)): Nest[A, B] = Nest(pair._1, pair._2)
  def apply[A, B](a: A, b: B): Nest[A, B] = NestCons(Pair(a, b), Nil)
  def apply[A, B](pair: AB[A, B]): Nest[A, B] = Nest(pair.a, pair.b)
  def apply[A, B](pair: BA[A, B]): Nest[A, B] = NestCons(pair, Nil)
  def apply[A, B](l: List[Pair[A, B]]) = l match {
    case Nil    => EmptyNest
    case h :: t => NestCons(h, t)
  }

  private[nest] def toPair[A, B](eab: Either[(A, B), (B, A)]): Pair[A, B] =
    eab.fold(p => AB(p._1, p._2), p => BA(p._1, p._2))

}

trait Nest[+A, +B] {
  lazy val depth: Int = this match {
    case EmptyNest      => 0
    case NestCons(h, t) => t.size + 1
  }

  lazy val size: Int = depth

  // TODO flatmap impl (or ap impl)
  //def flatMap[C, D](f: Either[(A, B), (B, A)] => Nest[C, D]): Nest[C, D] = ???

  def map[C, D](f: Either[(A, B), (B, A)] => Either[(C, D), (D, C)]): Nest[C, D] = this match {
    case en@EmptyNest    => en
    case NestCons(pair, pairs) => Nest[C, D]((pair :: pairs).map {
      case AB(a, b) => Nest.toPair(f(Left (a, b)))
      case BA(b, a) => Nest.toPair(f(Right(b, a)))
    })


  }

  def reverse: Nest[A, B] = this match {
    case EmptyNest      => EmptyNest
    case NestCons(h, t) => Nest((h :: t).reverse)
  }

  def drop(n: Int): Nest[A, B] = this match {
    case EmptyNest      => EmptyNest
    case NestCons(h, t) => Nest((h :: t).drop(n))
  }

  def take(n: Int): Nest[A, B] = this match {
    case EmptyNest      => EmptyNest
    case NestCons(h, t) => Nest((h :: t).take(n))
  }

  def prepend[C >: A, D >: B](nest: Nest[C, D]): Nest[C, D] = this match {
    case EmptyNest                   => EmptyNest
    case nc@NestCons(hAfter, tAfter) => nest match {
      case EmptyNest                  => nc
      case NestCons(hBefore, tBefore) => Nest[C, D]((hBefore :: tBefore) ::: (hAfter :: tAfter))
    }
  }

//TODO force construction through fun syntax so I don't have to expose the AB, BA Pair types?
//  def prepend[C >: A, D >: B](c: C, d: D): Nest[C, D] = prepend(Nest(c, d))
//  def prepend[C >: A, D >: B](d: D, c: C): Nest[C, D] = prepend(Nest(BA(d, c)))
  def append[C >: A, D >: B](nest: Nest[C, D]): Nest[C, D] = nest.prepend(this)
//  def append[C >: A, D >: B : TypeTag](pair: (C, D)): Nest[C, D] = append(Nest(pair))
//  def append[C >: A, D >: B](pair: (D, C)): Nest[C, D] = append(Nest(BA(pair)))
  def pluck(index: Int): Nest[A, B] = this.take(index) append this.drop(index + 1)

  def lift(index: Int): Option[Either[(A, B), (B, A)]] = this.drop(index) match {
    case EmptyNest      => None
    case NestCons(h, _) => Some(h.toTuple)
  }

  //TODO get rid of this either with an implicit class with type [A <: C, B <: C] (either.merge works this way)
  def toStream: Stream[Either[A, B]] = {
    unfold[(Nest[A, B], Stream[Either[A, B]]), Either[A, B]]((this, Stream())) {
      case (EmptyNest, Stream.Empty)           => None
      case (EmptyNest, ab #:: abs)             => Some((ab, (EmptyNest, abs)))
      case (NestCons(AB(a, b), pairs), tail) => Some((Left(a), (Nest(pairs), Right(b) #:: tail)))
      case (NestCons(BA(b, a), pairs), tail) => Some((Left(a), (Nest(pairs), Right(b) #:: tail)))
    }
  }

  def toList: List[Either[A, B]] = toStream.toList

  // TODO depends on Dotty union types:
  // def toStream[C >: A | B : TypeTag] = toStream.map{_.fold[C](identity, identity)}
  // def toList[C >: A | B : TypeTag] = toStream[C].toList

}

object UnionTypes {
  // Union types hack from from http://milessabin.com/blog/2011/06/09/scala-union-types-curry-howard/
  // issues with using these for subtyping:
  // scala> implicitly[Int <:< ¬¬[Int]]
  //  <console>:26: error: Cannot prove that Int <:< UnionTypes.¬¬[Int].
  //    implicitly[Int <:< ¬¬[Int]]
  //      ^
  //
  // scala> implicitly[¬¬[Int] <:< Int]
  //   <console>:26: error: Cannot prove that UnionTypes.¬¬[Int] <:< Int.
  //   implicitly[¬¬[Int] <:< Int]
  //     ^
  import scala.reflect.runtime.universe._

  type  ¬ [A]    = A => Nothing
  type ¬¬ [A]    = ¬[¬[A]]
  type  ∨ [T, U] = ¬[¬[T] with ¬[U]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }

  def size[T : (Int |∨| String)#λ](t: T): Int =
    t match {
      case i: Int => i
      case s: String => s.length
    }
}