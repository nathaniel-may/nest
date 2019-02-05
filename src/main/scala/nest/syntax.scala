

package object syntax {
  import nest._

  final implicit class NestSyntax[A](a: A) {
    private[syntax] final case class UnfinishedABNest[C, D](nest: Nest[C, D], c: C) extends Pair[C, D] {
      def >>(d: D): Nest[C, D] = NestCons(ABPair(c, d), nest)
    }

    private[syntax] final case class UnfinishedBANest[C, D](nest: Nest[C, D], d: D) extends Pair[C, D] {
      def >>(c: C): Nest[C, D] = NestCons(BAPair(d, c), nest)
    }

    def <<[B](nest: Nest[A, B]): UnfinishedABNest[A, B] = UnfinishedABNest(nest, a)
    def <<[B](nest: Nest[B, A]): UnfinishedBANest[B, A] = UnfinishedBANest(nest, a)
    def <<   (nest: Nest[A, A]): UnfinishedABNest[A, A] = UnfinishedABNest(nest, a)
  }

  final case class <<[A, B](leftOuter: A, inner: Nest[A, B]) extends Nest[A, B]
  final case class >>[A, B](inner: Nest[A, B], rightOuter: B) extends Nest[A, B]

}