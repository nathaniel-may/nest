package nest

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
      .foldLeft[Nest[A, A]](Nest.empty) {
        case (nest, (l, r)) => </>(l, nest, r) // TODO fix with better syntax
      }
  }

  def inOrder[A, B](pairs: List[nest.Pair[A, B]]): List[Either[A, B]] = {
    val there = pairs.map(_.toTuple match {
      case Left ((a, _)) => Left(a)
      case Right((b, _)) => Right(b)}  )

    val back = pairs.map(_.toTuple match {
      case Left ((_, b)) => Right(b)
      case Right((_, a)) => Left(a)}  )

    there ::: back.reverse
  }

}
