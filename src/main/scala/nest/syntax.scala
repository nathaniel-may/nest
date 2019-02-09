import nest.Nest

package object syntax {
  import nest._

  final implicit class NestSyntax[A](a: A) {
    def <</[B](nest: Nest[A, B]): NestWithoutRightB[A, B] = NestWithoutRightB(nest, a)
    def <<\[B](nest: Nest[B, A]): NestWithoutRightA[B, A] = NestWithoutRightA(nest, a)
  }

  final case class <</[A, B](left: A, inner: Nest[A, B])               extends Nest[A, B]
  final case class />>[A, B](inner: NestWithoutRightB[A, B], right: B) extends Nest[A, B]
  final case class <<\[A, B](left: B, inner: Nest[A, B])               extends Nest[A, B]
  final case class \>>[A, B](inner: NestWithoutRightA[A, B], right: A) extends Nest[A, B]

}