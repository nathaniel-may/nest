package nest

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}

// Project
import syntax._
import testingUtil.Functions._
import testingUtil.Generators._

object NestProperties extends Properties("Nest"){

    property("toList works") = forAll(evenGen[Int]) {
      (e: Even[Int]) =>
        toNest(e).toList.map(_.fold(identity, identity)) ==
        e.wrapped.toList
    }

    property("toStream works") = forAll(evenGen[Int]) {
      (e: Even[Int]) =>
        toNest(e).toStream.map(_.fold(identity, identity)) ==
          e.wrapped.toStream
    }

    property("matching works") = forAll { //TODO improve this test so all cases are hit
      (a: Int, b: Int) => {
        val n0 = Nest((a, b))
        n0 match {
          case EmptyNest               => false
          case x << EmptyNest >> y     => x == a && y == b
          case _ << (_ << _ >> _) >> _ => false
          case _ << _ >> _             => false
        }
      }
    }

  property("constructor syntax works") = forAll { //TODO improve this test so all cases are hit
    (a: String, b: Boolean) => { // TODO when a is of type Int Long v Int implicit conversion becomes a problem
      val n0 = Nest((a, b))
      (n0 match {
        case EmptyNest           => EmptyNest
        case x << EmptyNest >> y => Nest(x, y)//TODO can't get this to work x << EmptyNest >> y
        case x << nest >> y      => x << nest >> y
        //TODO add this case :   val n3:   Nest[Boolean, String] = "hey" << (true << n0 >> "hi") >> false
      }) == n0
    }
  }
}
