package testingUtil

import org.scalacheck.{Arbitrary, Gen}, Gen.pick
import org.scalacheck.Arbitrary._
import testingUtil.Functions._

import nest.Nest

object Generators {
  //val xx = Gen.pick(5, evenGen)

  def evenGen[A](implicit eva: Arbitrary[A]): Gen[Even[A]] = Gen.sized { size =>
    for {
      seq <- pick[A](size * 2, eva.arbitrary, eva.arbitrary)
      v   =  seq.toVector
      a   <- eva.arbitrary
    } yield Even(v, a)
  }

  def symetricalNestGen[A](implicit evv: Arbitrary[Vector[A]], eva: Arbitrary[A]): Gen[Nest[A, A]] = for {
    v <- evenGen[A]
  } yield toNest(v)

  def NestGen[A, B](implicit evva: Arbitrary[Vector[A]], evvb: Arbitrary[Vector[B]], eva: Arbitrary[A], evb: Arbitrary[B]): Gen[Nest[A, B]] = for {
    va <- evenGen[Either[A, B]]
  } yield toNest(v)

}
