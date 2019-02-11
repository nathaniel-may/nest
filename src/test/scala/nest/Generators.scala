package nest

import org.scalacheck.{Arbitrary, Gen}

object Generators {

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