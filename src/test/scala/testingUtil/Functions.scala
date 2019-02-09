package testingUtil

import nest._
import syntax._

object Functions {

  // TODO is this private constructor still being called despite being private???
  final case class Even[A] private (wrapped: Vector[A])
  object Even {
    def apply[A](v: Vector[A], makeEvenWith: => A): Even[A] =
      if (v.size % 2 != 0) new Even(v :+ makeEvenWith) else new Even(v)
  }


  def toNest[A](v: Even[A]): Nest[A, A] = {
    v.wrapped
      .zip(v.wrapped.reverse)
      .take(v.wrapped.size/2)
      .reverse
      .foldLeft[Nest[A, A]](EmptyNest) {
        case (nest, (l, r)) => l << nest >> r
      }
  }

}
