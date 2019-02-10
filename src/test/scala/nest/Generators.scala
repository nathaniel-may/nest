package nest

import nest.Functions._
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  def evenGen[A](implicit evv: Arbitrary[Vector[A]], eva: Arbitrary[A]): Gen[Even[A]] = for {
    v <- evv.arbitrary
    a <- eva.arbitrary
  } yield Even(v, a)

  def evenGen[A](minSize: Int)(implicit eva: Arbitrary[A]): Gen[Even[A]] = for {
    seq <- Gen.pick(minSize + 1, eva.arbitrary, eva.arbitrary)
  } yield seq match {
    case Nil    => Even(Vector(), seq.head)
    case h :: t => Even(t.toVector, h)
  }

  def symetricalNestGen[A](implicit evv: Arbitrary[Vector[A]], eva: Arbitrary[A]): Gen[Nest[A, A]] = for {
    v <- evenGen[A]
  } yield toNest(v)

  def pairGen[A, B](implicit eva: Arbitrary[A], evb: Arbitrary[B]): Gen[nest.Pair[A, B]] = for {
    a <- eva.arbitrary
    b <- evb.arbitrary
    p <- Gen.oneOf(nest.AB(a, b), nest.BA(b, a))
  } yield p

  def pairsGen[A, B](implicit eva: Arbitrary[A], evb: Arbitrary[B]): Gen[List[nest.Pair[A, B]]] = Gen.sized { size =>
    Gen.listOfN(size, pairGen[A, B])
  }

  def nestGen[A, B](implicit eva: Arbitrary[A], evb: Arbitrary[B]): Gen[Nest[A, B]] = Gen.sized { size =>
    for {
      pairs <- Gen.resize(size, pairsGen[A, B])
      nest  =  Nest(pairs)
    } yield nest
  }

}