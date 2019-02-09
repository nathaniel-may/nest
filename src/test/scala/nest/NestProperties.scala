package nest

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}

// Project
import nest.Nest
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

//  property("matching works") = forAll(symetricalNestGen[Int]) {
//    //TODO pattern matching syntax is clashing with constructor syntax
//    (n: Nest[Int, Boolean]) =>(n match {
//      case Nest.empty                     => List()
//      case a <</ Nest.empty />> b         => List(a, b)
//      case a <</ nest />> b               => a :: nest.toList.map(_.merge) ::: List(b)
//      case a <</ (b <</ nest />> c) />> d => List(a, b) ::: nest.toList.map(_.merge) ::: List(c, d)
//      case x <<\ Nest.empty \>> y         => List(x, y)
//      case a <<\ nest \>> c               => a :: nest.toList.map(_.merge) ::: List(c)
//      case a <<\ (b <<\ nest \>> c) \>> d => List(a, b) ::: nest.toList.map(_.merge) ::: List(c, d)
//      case a <<\ (b <</ nest />> c) \>> d => List(a, b) ::: nest.toList.map(_.merge) ::: List(c, d)
//      case a <</ (b <<\ nest \>> c) />> d => List(a, b) ::: nest.toList.map(_.merge) ::: List(c, d)
//    }) == n.toList.map(_.merge)
//  }
//
//  property("constructor syntax works") = forAll { //TODO improve this test so all cases are hit
//    (a: String, b: Boolean) => { // TODO when a is of type Int Long v Int implicit conversion becomes a problem
//      val n0 = Nest((a, b))
//      (n0 match {
//        case EmptyNest           => EmptyNest
//        case x << EmptyNest >> y => Nest(x, y)//TODO can't get this to work ---> x << EmptyNest >> y
//        case x << nest >> y      => x << nest >> y
//        //TODO add this case :   val n3:   Nest[Boolean, String] = "hey" << (true << n0 >> "hi") >> false
//      }) == n0
//    }
//  }
}
