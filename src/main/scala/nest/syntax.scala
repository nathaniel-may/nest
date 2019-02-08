

package object syntax {
  import nest._
  import reflect.runtime.universe.TypeTag

  final implicit class NestSyntax[A](a: A) {

    def <<[B : TypeTag](nest: Nest[A, B]): NestWithoutRightB[A, B] = NestWithoutRightB(nest, a)
    def <<[B : TypeTag](nest: Nest[B, A]): NestWithoutRightA[B, A] = NestWithoutRightA(nest, a)
    def <<(nest: Nest[A, A]): NestWithoutRightB[A, A] = NestWithoutRightB(nest, a)
    // TODO type erasure.... def <<[B](nest: Nest[Nothing, Nothing]): NestWithoutRightB[A, B] = NestWithoutRightB(nest, a)
  }

  final case class <<[A, B](left: A, inner: Nest[A, B])  extends Nest[A, B]
  final case class >>[A, B](inner: Nest[A, B], right: B) extends Nest[A, B]

}