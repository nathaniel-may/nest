package isolation

object SyntaxTest {
  import nest._
  import syntax._

  val output: List[Int] = Nest(1, 2) match {
    case Nest.empty       => List()
    case a </: nest :/> b => (a </: nest :/> b).toList.map(_.merge)
    case b <\: nest :\> a => (b <\: nest :\> a).toList.map(_.merge)
  }

  def main(args: Array[String]): Unit = {
    println(s"output: $output")
  }

}