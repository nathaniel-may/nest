package nest

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}

// Project
import nest._
import syntax._
import Functions._
import Generators._

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

  //TODO it's backwards
  property("matching works") = forAll(nestGen[Int, Boolean]) {
    (n: Nest[Int, Boolean]) =>
        val list: List[Either[Int, Boolean]] = n match {
        case Nest.empty                     => List()
        case a </: Nest.empty :/> b         => List(Right(a), Left(b))
        case a </: nest :/> b               => Right(a) :: nest.toList ::: List(Left(b))
        case a </: (b </: nest :/> c) :/> d => List(Right(a), Right(b)) ::: nest.toList ::: List(Left(c), Left(d))
        case b <\: Nest.empty :\> a         => List(Right(a), Left(b))
        case b <\: nest :\> a               => Right(a) :: nest.toList ::: List(Left(b))
        case d <\: (c <\: nest :\> b) :\> a => List(Right(a), Right(b)) ::: nest.toList ::: List(Left(c), Left(d))
        case d <\: (b </: nest :/> c) :\> a => List(Right(a), Right(b)) ::: nest.toList ::: List(Left(c), Left(d))
        case a </: (b <\: nest :\> c) :/> d => List(Right(a), Left(b)) ::: nest.toList ::: List(Right(c), Left(d))
      }
      println(s" list: $list")
      println(s"nlist: ${n.toList}")
      list == n.toList
  }

//  property("constructor syntax works") = forAll {
//
//  }
}
