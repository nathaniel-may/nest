package nest

import scalaz.Scalaz.unfold

private[nest] final case class NestWrap[+A, +B](pairs: List[Pair[A, B]]) extends Nest[A, B]

final case class :/>[+A, +B](n: Nest[A, B], a: A) {
  def </:[C >: A, D >: B](d: D): Nest[C, D] = </>(a, n, d)
}
final case class :\>[+A, +B](n: Nest[A, B], b: B) {
  def <\:[C >: A, D >: B](c: C): Nest[C, D] = <\>(b, n, c)
}

object </: {
  def unapply[A, B](n: Nest[A, B]): Option[(B, :/>[A, B])] = n match {
    case Nest.empty        => None
    case </>(a, nest, b) => Some((b, :/>(nest, a)))
    case <\>(_, _, _)    => None
  }
}

object <\: {
  def unapply[A, B](n: Nest[A, B]): Option[(A, :\>[A, B])] = n match {
    case Nest.empty        => None
    case </>(_, _, _)    => None
    case <\>(b, nest, a) => Some((a, :\>(nest, b)))
  }
}

object </> {
  def apply[A, B](a: A, nest: Nest[A, B], b: B): Nest[A, B] = nest.wrapWith(AB(a, b) :: _)
  def unapply[A, B](n: Nest[A, B]): Option[(A, Nest[A, B], B)] = n match {
    case Nest.empty                  => None
    case NestWrap(BA(_, _) :: _)     => None
    case NestWrap(AB(a, b) :: pairs) => Some(a, NestWrap(pairs), b)
  }
}

object <\> {
  def apply[A, B](b: B, nest: Nest[A, B], a: A): Nest[A, B] = nest.wrapWith(BA(b, a) :: _)
  def unapply[A, B](n: Nest[A, B]): Option[(B, Nest[A, B], A)] = n match {
    case Nest.empty                  => None
    case NestWrap(AB(_, _) :: _)     => None
    case NestWrap(BA(b, a) :: pairs) => Some(b, NestWrap(pairs), a)
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
  val empty = NestWrap(Nil)

  def apply[A, B](pair: (A, B)): Nest[A, B] = Nest(pair._1, pair._2)
  def apply[A, B](a: A, b: B): Nest[A, B] = NestWrap(List(AB(a, b)))
  def apply[A, B](pair: AB[A, B]): Nest[A, B] = NestWrap(List(pair))
  def apply[A, B](pair: BA[A, B]): Nest[A, B] = NestWrap[A, B](List(pair))

  private[nest] def toPair[A, B](eab: Either[(A, B), (B, A)]): Pair[A, B] =
    eab.fold(p => AB(p._1, p._2), p => BA(p._1, p._2))

}

//TODO deal with syntax and sealing
trait Nest[+A, +B] { //TODO I switched these...? c and d
  def :/>[C >: A, D >: B](c: C): :/>[C, D] = new :/>[C, D](this, c)
  def :\>[C >: A, D >: B](d: D): :\>[C, D] = new :\>[C, D](this, d)

  private[nest] def use[C](f: List[Pair[A, B]] => C): C = this match {
    case NestWrap(pairs) => f(pairs)
  }

  private[nest] def wrapWith[C >: A, D >: B](f: List[Pair[A, B]] => List[Pair[C, D]]): Nest[C, D] = this match {
    case NestWrap(pairs) => NestWrap(f(pairs))
  }


  // TODO can improve efficiency
  lazy val depth: Int = this match {
    case NestWrap(pairs) => pairs.size
  }

  lazy val size: Int = depth

  // TODO flatmap impl (or ap impl)
  //def flatMap[C, D](f: Either[(A, B), (B, A)] => Nest[C, D]): Nest[C, D] = ???

  // TODO replace with bimap???
  def map[C, D](f: Either[(A, B), (B, A)] => Either[(C, D), (D, C)]): Nest[C, D] = this match {
    case NestWrap(pairs) => NestWrap[C, D](pairs.map {
      case AB(a, b) => Nest.toPair(f(Left (a, b)))
      case BA(b, a) => Nest.toPair(f(Right(b, a)))
    })
  }

  def reverse: Nest[A, B] = wrapWith[A, B](_.reverse)
  def drop(n: Int): Nest[A, B] = wrapWith(_.drop(n))
  def take(n: Int): Nest[A, B] = wrapWith(_.take(n))

  def prepend[C >: A, D >: B](nest: Nest[C, D]): Nest[C, D] = (nest, this) match {
    case (NestWrap(before), NestWrap(after)) => NestWrap(before ::: after)
    case (_, n @ NestWrap(_))                => n
    case (n @ NestWrap(_), _)                => n
    case (_, _)                              => this
  }

  def append[C >: A, D >: B](nest: Nest[C, D]): Nest[C, D] = nest.prepend(this)
  def pluck(index: Int): Nest[A, B] = this.take(index) append this.drop(index + 1)
  def isEmpty: Boolean = use(_.isEmpty)

  def lift(index: Int): Option[Either[(A, B), (B, A)]] =
    this.drop(index).use(_.headOption).map(_.toTuple)

  //TODO get rid of this either with an implicit class with type [A <: C, B <: C] (either.merge works this way)
  def toStream: Stream[Either[A, B]] = {
    unfold[(Nest[A, B], Stream[Either[A, B]]), Either[A, B]]((this, Stream())) {
      case (Nest.empty, Stream.Empty)           => None
      case (Nest.empty, ab #:: abs)             => Some((ab, (Nest.empty, abs)))
      case (NestWrap(AB(a, b) :: pairs), tail) => Some((Left(a), (NestWrap(pairs), Right(b) #:: tail)))
      case (NestWrap(BA(b, a) :: pairs), tail) => Some((Left(a), (NestWrap(pairs), Right(b) #:: tail)))
    }
  }

  def toList: List[Either[A, B]] = toStream.toList

  // TODO depends on Dotty union types:
  // def toStream[C >: A | B : TypeTag] = toStream.map{_.fold[C](identity, identity)}
  // def toList[C >: A | B : TypeTag] = toStream[C].toList

}