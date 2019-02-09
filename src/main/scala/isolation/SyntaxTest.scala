package isolation

object SyntaxTest {
  import nest.{Nest, <<\>>, <</>>, NestWithoutRightA, NestWithoutRightB}

  final implicit class NestSyntax[A](a: A) {
    def <</[B](nest: Nest[A, B]): NestWithoutRightB[A, B] = NestWithoutRightB(nest, a)
    def <<\[B](nest: Nest[B, A]): NestWithoutRightA[B, A] = NestWithoutRightA(nest, a)
  }

  final class />>[A, B](val inner: NestWithoutRightB[A, B])
  final object />> {
    def apply[A, B](n: Nest[A, B], a: A): />>[A, B] = {
      new />>(NestWithoutRightB(n, a))
    }

    def unapply[A, B](n: Nest[A, B]): Option[(Nest[A, B], B)] = n match {
      case Nest.empty => None
      case <</>>(_, nest, b) => Some((nest, b))
    }
  }

  final class \>>[A, B](val inner: NestWithoutRightA[A, B])
  final object \>> {
    def apply[A, B](n: Nest[A, B], b: B): \>>[A, B] =
      new \>>(NestWithoutRightA(n, b))

    def unapply[A, B](n: Nest[A, B]): Option[(Nest[A, B], A)] = n match {
      case Nest.empty => None
      case <<\>>(_, nest, a) => Some((nest, a))
    }
  }

  object <</ {
    def unapply[A, B](n: Nest[A, B]): Option[(B, />>[A, B])] = n match {
      case Nest.empty        => None
      case <</>>(a, nest, b) => Some((b, />>(nest, a)))
    }
  }

  object <<\ {
    def unapply[A, B](n: Nest[A, B]): Option[(A, \>>[A, B])] = n match {
      case Nest.empty        => None
      case <<\>>(b, nest, a) => Some((a, \>>(nest, b)))
    }
  }

  //TODO: "pattern is incompatible with expected type. found Nest[A, B] required SyntaxTest./>>[Int, Int] "
//  val output1: List[Int] = Nest(1, 2) match {
//    case Nest.empty       => List()
//    case a <</ nest />> b => (a <</ nest />> b).toList
//    case b <<\ nest \>> a => (b <<\ nest \>> a).toList
//  }

  val x: NestWithoutRightB[Int, Int] = 0 <</ Nest(1,2)
  val y: Nest[Int, Int] = x />> 4

  def main(args: Array[String]): Unit = {
    //println(s"output: $output1")
  }

}