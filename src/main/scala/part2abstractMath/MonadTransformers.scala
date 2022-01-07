package part2abstractMath

/**
 * User: pat
 * Date: 7/1/22
 * Time: 14:20
 */
object MonadTransformers {

  // option transformer
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
  }



}
