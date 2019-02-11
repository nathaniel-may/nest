package nest

import scalaz.Scalaz.unfold

final case class :/>[+A, +B](n: Nest[A, B], b: B) {
  def </:[C >: A, D >: B](c: C): Nest[C, D] = </>(c, n, b)
}
final case class :\>[+A, +B](n: Nest[A, B], a: A) {
  def <\:[C >: A, D >: B](d: D): Nest[C, D] = <\>(d, n, a)
}

object </: {
  def unapply[A, B](n: Nest[A, B]): Option[(A, :/>[A, B])] = n match {
    case Nest.empty        => None
    case </>(a, nest, b) => Some((a, :/>(nest, b)))
    case <\>(_, _, _)    => None
  }
}

object <\: {
  def unapply[A, B](n: Nest[A, B]): Option[(B, :\>[A, B])] = n match {
    case Nest.empty        => None
    case </>(_, _, _)    => None
    case <\>(b, nest, a) => Some((b, :\>(nest, a)))
  }
}

object </> {
  def apply[A, B](a: A, nest: Nest[A, B], b: B): Nest[A, B] = nest.wrapWith(AB(a, b) :: _)
  def unapply[A, B](n: Nest[A, B]): Option[(A, Nest[A, B], B)] = n match {
    case Nest(Nil)               => None
    case Nest(BA(_, _) :: _)     => None
    case Nest(AB(a, b) :: pairs) => Some(a, Nest(pairs), b)
  }
}

object <\> {
  def apply[A, B](b: B, nest: Nest[A, B], a: A): Nest[A, B] = nest.wrapWith(BA(b, a) :: _)
  def unapply[A, B](n: Nest[A, B]): Option[(B, Nest[A, B], A)] = n match {
    case Nest(Nil)               => None
    case Nest(AB(_, _) :: _)     => None
    case Nest(BA(b, a) :: pairs) => Some(b, Nest(pairs), a)
  }
}

private[nest] sealed trait Pair[+A, +B]{
  def toTuple: Either[(A, B), (B, A)] = this match {
    case AB(a, b) => Left (a, b)
    case BA(b, a) => Right(b, a)
  }
}

private[nest] case class AB[A, B] (a: A, b: B) extends Pair[A, B]
private[nest] case class BA[A, B] (b: B, a: A) extends Pair[A, B]

object Nest {
  val empty = Nest(Nil)

  private[nest] def toPair[A, B](eab: Either[(A, B), (B, A)]): Pair[A, B] =
    eab.fold(p => AB(p._1, p._2), p => BA(p._1, p._2))

}

final case class Nest[+A, +B] private[nest] (pairs: List[Pair[A, B]]) {
  def :/>[C >: A, D >: B](d: D): :/>[C, D] = new :/>[C, D](this, d)
  def :\>[C >: A, D >: B](c: C): :\>[C, D] = new :\>[C, D](this, c)

  private[nest] def use[C](f: List[Pair[A, B]] => C): C = f(pairs)

  private[nest] def wrapWith[C >: A, D >: B](f: List[Pair[A, B]] => List[Pair[C, D]]): Nest[C, D] =
    Nest(f(pairs))

  // TODO can improve efficiency
  lazy val depth: Int = pairs.size

  lazy val size: Int = depth

  // TODO this isn't really map. replace with something else or remove. bimap???
  def map[C, D](f: Either[(A, B), (B, A)] => Either[(C, D), (D, C)]): Nest[C, D] =
    Nest[C, D](pairs.map {
      case AB(a, b) => Nest.toPair(f(Left (a, b)))
      case BA(b, a) => Nest.toPair(f(Right(b, a)))
    })

  def reverse: Nest[A, B] = wrapWith[A, B](_.reverse)
  def drop(n: Int): Nest[A, B] = wrapWith(_.drop(n))
  def take(n: Int): Nest[A, B] = wrapWith(_.take(n))

  def prepend[C >: A, D >: B](nest: Nest[C, D]): Nest[C, D] = (nest, this) match {
    case (Nest(before), Nest(after)) => Nest(before ::: after)
    case (_, n @ Nest(_))                => n
    case (n @ Nest(_), _)                => n
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
      case (Nest.empty, Stream.Empty)         => None
      case (Nest.empty, ab #:: abs)           => Some((ab, (Nest.empty, abs)))
      case (Nest(AB(a, b) :: ps), abs) => Some((Left(a),  (Nest(ps), Right(b) #:: abs)))
      case (Nest(BA(b, a) :: ps), abs) => Some((Right(b), (Nest(ps), Left(a)  #:: abs)))
    }
  }

  def toList: List[Either[A, B]] = toStream.toList

  // TODO depends on Dotty union types:
  // def toStream[C >: A | B : TypeTag] = toStream.map{_.fold[C](identity, identity)}
  // def toList[C >: A | B : TypeTag] = toStream[C].toList

}