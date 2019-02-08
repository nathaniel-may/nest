package testingUtil

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import testingUtil.Functions.Even

object Generators {

  def evenGen[A](implicit evv: Arbitrary[Vector[A]], eva: Arbitrary[A]): Gen[Even[A]] = for {
    v <- evv.arbitrary
    a <- eva.arbitrary
  } yield Even(v, a)

}
