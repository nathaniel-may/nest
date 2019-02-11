package nest

object Functions {

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
