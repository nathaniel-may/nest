package nest

// Scalacheck
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties, Shrink}

// Project
import Functions._
import Generators._

object NestProperties extends Properties("Nest"){

  property("toList works") = forAll(pairsGen[Int, Boolean]) {
    pairs: List[Pair[Int, Boolean]] =>
      Nest(pairs).toList == inOrder(pairs)
  }

  property("toStream works") = forAll(pairsGen[Int, Boolean]) {
    pairs: List[Pair[Int, Boolean]] =>
      Nest(pairs).toStream == inOrder(pairs).toStream
  }

  property("matching works") = forAll(nestGen[Int, Boolean]) {
    n: Nest[Int, Boolean] =>
        val list: List[Either[Int, Boolean]] = n match {
        case Nest.empty                     => List()
        case a </: Nest.empty :/> b         => List(Left(a), Right(b))
        case a </: nest :/> b               => Left(a) :: nest.toList ::: List(Right(b))
        case a </: (b </: nest :/> c) :/> d => List(Left(a), Left(b)) ::: nest.toList ::: List(Right(c), Right(d))
        case a <\: Nest.empty :\> b         => List(Right(a), Left(b))
        case a <\: nest :\> b               => Right(a) :: nest.toList ::: List(Left(b))
        case a <\: (b <\: nest :\> c) :\> d => List(Right(a), Right(b)) ::: nest.toList ::: List(Left(c), Left(d))
        case a <\: (b </: nest :/> c) :\> d => List(Right(a), Left(b)) ::: nest.toList ::: List(Right(c), Left(d))
        case a </: (b <\: nest :\> c) :/> d => List(Left(a), Right(b)) ::: nest.toList ::: List(Left(c), Right(d))
      }
      list == n.toList
  }

  property("constructor syntax works") = forAll(nestGen[Int, Boolean]) {
    n: Nest[Int, Boolean] => (n match {
      case Nest.empty                     => Nest.empty
      case a </: Nest.empty :/> b         => a </: Nest.empty :/> b
      case a </: nest :/> b               => a </: nest :/> b
      case a </: (b </: nest :/> c) :/> d => a </: (b </: nest :/> c) :/> d
      case a <\: Nest.empty :\> b         => a <\: Nest.empty :\> b
      case a <\: nest :\> b               => a <\: nest :\> b
      case a <\: (b <\: nest :\> c) :\> d => a <\: (b <\: nest :\> c) :\> d
      case a <\: (b </: nest :/> c) :\> d => a <\: (b </: nest :/> c) :\> d
      case a </: (b <\: nest :\> c) :/> d => a </: (b <\: nest :\> c) :/> d
    }) == n
  }

  property("bad matching fails") = forAll(nestGen[Int, Boolean]) {
    n: Nest[Int, Boolean] => n match {
      case nest @ Nest.empty => nest match {
        case </>(_, _, _) => false
        case <\>(_, _, _) => false
        case </:(_,_)     => false
        case <\:(_,_)     => false
        case _            => true
      }
      case nest @ </>(_, _, _) => nest match {
        case <\>(_, _, _) => false
        case <\:(_,_)     => false
        case _            => true
      }
      case nest @ <\>(_, _, _) => nest match {
        case </>(_, _, _) => false
        case </:(_,_)     => false
        case _            => true
      }
    }
  }

  property("depth is accurate") = forAll(nestGen[Int, Boolean]) {
    n: Nest[Int, Boolean] => n.depth == n.pairs.size && n.size == n.pairs.size
  }

  property("map works") = forAll(nestGen[Int, Boolean]) {
    n: Nest[Int, Boolean] =>
      val f: Either[(Int, Boolean), (Boolean, Int)] => Either[(Int, String), (String, Int)] = {
        case Left ((i, b)) => Left ((i + 1,      b.toString))
        case Right((b, i)) => Right((b.toString, i + 1))
      }

      val ff: Either[Int, Boolean] => Either[Int, String] = {
        case Left (i) => Left (i + 1)
        case Right(b) => Right(b.toString)
      }

      n.map(f).toList == n.toList.map(ff)
  }

  property("list wrappers work. i.e. reverse, drop, take, prepend, append") = forAll(nestGen[Int, Boolean], implicitly[Arbitrary[Int]].arbitrary) {
    (n: Nest[Int, Boolean], i: Int) =>
      n.reverse.pairs == n.pairs.reverse &&
      n.drop(i).pairs == n.pairs.drop(i) &&
      n.take(i).pairs == n.pairs.take(i)
  }

  property("prepend works") = forAll(nestGen[Int, Boolean], nestGen[Int, Boolean]) {
    (n0: Nest[Int, Boolean], n1: Nest[Int, Boolean]) =>
      n0.prepend(n1).toList == n1.toList.take(n1.size) ::: n0.toList ::: n1.toList.drop(n1.size)
  }

  property("append works") = forAll(nestGen[Int, Boolean], nestGen[Int, Boolean]) {
    (n0: Nest[Int, Boolean], n1: Nest[Int, Boolean]) =>
      n0.append(n1).toList == n0.toList.take(n0.size) ::: n1.toList ::: n0.toList.drop(n0.size)
  }

}
