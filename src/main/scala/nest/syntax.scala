import nest.Nest

package object syntax {
  import nest._

//  final implicit class NestSyntax[A](a: A) {
//    def <</[B](nest: Nest[A, B]): PartialNestA[A, B] = PartialNestA(nest, a)
//    def <<\[B](nest: Nest[B, A]): PartialNestB[B, A] = PartialNestB(nest, a)
//  }
//
//  final case class <</[A, B](left: A, inner: Nest[A, B])               extends Nest[A, B]
//  final case class />>[A, B](inner: PartialNestA[A, B], right: B) extends Nest[A, B]
//  final case class <<\[A, B](left: B, inner: Nest[A, B])               extends Nest[A, B]
//  final case class \>>[A, B](inner: PartialNestB[A, B], right: A) extends Nest[A, B]

}