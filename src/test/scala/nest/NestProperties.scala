package nest

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}

// Project
import syntax._

object NestProperties extends Properties("Nest"){

    property("toList works") = forAll {
      v: Vector[Int] => {
        val evenV = if (v.size % 2 != 0) v :+ 0 else v
        Nest(evenV
          .zip(evenV.reverse)
          .take(evenV.size/2)
          .map { case (a, b) => ABPair(a, b) }
          .reverse
          .toList :_*).toList.map(_.fold(identity, identity)) == evenV.toList //TODO this fold syntax sucks. Find a better way to do this when A == B
      }
    }

  //    property("toStream works") = forAll {
  //
  //    }

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
        case x << EmptyNest >> y => x << EmptyNest >> y
        case x << nest >> y      => x << nest >> y
        //TODO add this case :   val n3:   Nest[Boolean, String] = "hey" << (true << n0 >> "hi") >> false
      }) == n0
    }
  }
}
