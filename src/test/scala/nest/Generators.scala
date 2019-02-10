package testingUtil

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import testingUtil.Functions._

import nest.Nest

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

  def nestGen[A, B](implicit eva: Arbitrary[A], evb: Arbitrary[A]): Gen[Nest[A, A]] = Gen.sized { size =>
    for {
      va <- evenGen(size)[A]
      vb <- evenGen(size)[B]
    } yield toNest(v)
  }

  def nestGen[A, B]: Gen[Nest[A, B]] = ???

}