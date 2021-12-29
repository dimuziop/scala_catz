package part2abstractMath

/**
 * User: pat
 * Date: 29/12/21
 * Time: 14:33
 */
object Semigroups {
  // semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._

  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombination: String = naturalStringSemigroup.combine("Hello ", "There") // concatenation

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)


  // TODO 1: support a new type
  // hint: use the same patter we use with Eq
  case class Expense(id: Long, amount: BigDecimal)

  implicit val expenseSemigroup: Semigroup[Expense] =
    Semigroup.instance[Expense] {
      (a, b) => Expense(Math.max(a.id, b.id), a.amount + b.amount)
    }

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val intSum: Int = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val aStringConcat: String = "Me Like " |+| "Semigroups"
  val aExpenseComb: Expense = Expense(5, 35) |+| Expense(8, 45)

  // TODO 2 : implement reduceThings2
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(_ |+| _)

  def reduceThings3[T: Semigroup](list: List[T]): T = // look the implicit syntax here
    list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    val strings = List("I'm ", "starting", " to", " like", " semigroups")
    val expenses = List(Expense(1,58), Expense(2,75), Expense(3, 57))
    // specific API
    println(reduceInts(numbers))
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers)) // compiler inject the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler inject the implicit Semigroup[String]
    import cats.instances.option._ // compiler will produce an implicit Semigroup[Option[Int]]
    // combine will produce another option with summed elements
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions))
    println(reduceThings(expenses))

  }

}
