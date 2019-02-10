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

  def symetricalNestGen[A](implicit evv: Arbitrary[Vector[A]], eva: Arbitrary[A]): Gen[Nest[A, A]] = for {
    v <- evenGen[A]
  } yield toNest(v)

}