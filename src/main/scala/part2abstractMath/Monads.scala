package part2abstractMath

/**
 * User: pat
 * Date: 31/12/21
 * Time: 8:56
 */
object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1 how do you create all combination of (number, char)?
  val combination1Point1: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationLstFor: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  //options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')
  // TODO 1.2 how do you create all combination of (number, char)?
  val combinationOption: Option[(Int, Char)] = numberOption.flatMap(n => charOption.map(c => (n,c)))
  val combinationOptionFor: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)


  def main(args: Array[String]): Unit = {
    println(combination1Point1)
    println(combinationOption)
  }

}
