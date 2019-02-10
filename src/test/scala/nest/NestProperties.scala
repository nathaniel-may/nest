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

  property("toList works") = forAll(pairsGen[Int, Boolean]) {
    (pairs: List[Pair[Int, Boolean]]) =>
      NestWrap(pairs).toList == inOrder(pairs)
  }

  property("toStream works") = forAll(pairsGen[Int, Boolean]) {
    (pairs: List[Pair[Int, Boolean]]) =>
      NestWrap(pairs).toStream == inOrder(pairs).toStream
  }

  property("matching works") = forAll(nestGen[Int, Boolean]) {
    (n: Nest[Int, Boolean]) =>
        val list: List[Either[Int, Boolean]] = n match {
        case Nest.empty                     => List()
        case a </: Nest.empty :/> b         => List(Left(a), Right(b))
        case a </: nest :/> b               => Left(a) :: nest.toList ::: List(Right(b))
        case a </: (b </: nest :/> c) :/> d => List(Left(a), Left(b)) ::: nest.toList ::: List(Right(c), Right(d))
        case b <\: Nest.empty :\> a         => List(Left(a), Right(b))
        case b <\: nest :\> a               => Left(a) :: nest.toList ::: List(Right(b))
        case d <\: (c <\: nest :\> b) :\> a => List(Left(a), Left(b)) ::: nest.toList ::: List(Right(c), Right(d))
        case d <\: (b </: nest :/> c) :\> a => List(Left(a), Left(b)) ::: nest.toList ::: List(Right(c), Right(d))
        case a </: (b <\: nest :\> c) :/> d => List(Left(a), Right(b)) ::: nest.toList ::: List(Left(c), Right(d))
      }
      list == n.toList
  }

//  property("constructor syntax works") = forAll {
//
//  }
}
