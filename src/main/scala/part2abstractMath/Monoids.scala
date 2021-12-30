package part2abstractMath

/**
 * User: pat
 * Date: 30/12/21
 * Time: 8:18
 */
object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import |+| extension method

  val numbers: List[Int] = (1 to 1000).toList

  // |+| is always associative

  val sumLeft: Int = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //def combineFold[T: Semigroup](list: List[T]): T = list.foldLeft(/** WHAT?*//)(_ |+| _)

  // MONOID
  import cats.Monoid
  val intMonoid: Monoid[Int] = Monoid[Int]

  val combinedNumbers: Int = intMonoid.combine(23, 999)
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // brng the implicit Monoid[String] in scope
  val emptyStr = Monoid[String].empty

  import cats.instances.option._
  val emptyOption = Monoid[Option[Int]].empty // None
  val combinedOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // 2

  // extension methods for Monoids
  val combinedOptionFancy = Option(2) |+| Option(9)


  // TODO1: solve combine fold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(sumLeft == sumRight)
  }

}
