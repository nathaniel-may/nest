package testingUtil

import nest.{ABPair, Nest}

object Functions {

  // TODO is this private constructor being called ANYWAY???
  final case class Even[A] private (wrapped: Vector[A])
  object Even {
    def apply[A](v: Vector[A], makeEvenWith: => A): Even[A] =
      if (v.size % 2 != 0) new Even(v :+ makeEvenWith) else new Even(v)
  }


  def toNest[A](v: Even[A]): Nest[A, A] = {
    Nest(v.wrapped
      .zip(v.wrapped.reverse)
      .take(v.wrapped.size/2)
      .map { case (a, b) => ABPair(a, b) }
      .reverse
      .toList :_*)
  }

}
