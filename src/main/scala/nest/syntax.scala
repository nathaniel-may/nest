

package object syntax {
  import nest._
  import reflect.runtime.universe.TypeTag

  final implicit class NestSyntax[A : TypeTag](a: A) {
    private[syntax] final case class UnfinishedABNest[C >: A, D](nest: Nest[C, D], c: C) extends Pair[C, D] {
      def >>(d: D): Nest[C, D] = NestCons(ABPair(c, d), nest)
    }

    private[syntax] final case class UnfinishedBANest[C, D >: A](nest: Nest[C, D], d: D) extends Pair[C, D] {
      def >>(c: C): Nest[C, D] = NestCons(BAPair(d, c), nest)
    }

    def <<[B : TypeTag](nest: Nest[A, B]): UnfinishedABNest[A, B] = UnfinishedABNest(nest, a)
    def <<[B : TypeTag](nest: Nest[B, A]): UnfinishedBANest[B, A] = UnfinishedBANest(nest, a)
    // TODO def <<             (nest: Nest[A, A]): UnfinishedABNest[A, A] = UnfinishedABNest(nest, a)
    def <<[B](nest: Nest[Nothing, Nothing]): UnfinishedABNest[A, B] = UnfinishedABNest(nest, a)

  }

  final case class <<[A, B](leftOuter: A, inner: Nest[A, B]) extends Nest[A, B]
  final case class >>[A, B](inner: Nest[A, B], rightOuter: B) extends Nest[A, B]

}