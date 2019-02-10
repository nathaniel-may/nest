package isolation

object SyntaxTest {
  import nest._

  val output: List[Int] = Nest(1, 2) match {
    case Nest.empty       => List()
    case a <</ nest />> b => (a <</ nest />> b).toList
    case b <<\ nest \>> a => (b <<\ nest \>> a).toList
  }

  val x: <</[Int, Int] = Nest(1,2) />> 2
  val z: Nest[Int, Int] = <</(0, x)
  val y: Nest[Int, Int] = 0 <</ Nest(1,2) />> 4 //(0 <</ Nest(1,2)) />> 4

  def main(args: Array[String]): Unit = {
    println(s"output: $output")
  }

}