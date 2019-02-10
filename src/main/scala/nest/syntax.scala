import nest.Nest

package object syntax {
  import nest._

  final implicit class NestSyntax[X](x: X) {
    def <</[B](partial: (Nest[X, B], B)): Nest[X, B] = <</>>(x, partial._1, partial._2)
    def <<\[A](partial: (Nest[A, X], A)): Nest[A, X] = <<\>>(x, partial._1, partial._2)
  }
//
//  final case class <</[A, B](left: A, inner: Nest[A, B])               extends Nest[A, B]
//  final case class />>[A, B](inner: PartialNestA[A, B], right: B) extends Nest[A, B]
//  final case class <<\[A, B](left: B, inner: Nest[A, B])               extends Nest[A, B]
//  final case class \>>[A, B](inner: PartialNestB[A, B], right: A) extends Nest[A, B]

}