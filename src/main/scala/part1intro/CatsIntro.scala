package part1intro

/**
 * User: pat
 * Date: 27/12/21
 * Time: 17:28
 */
object CatsIntro {

  // Eq | type class

  val aComparison: Boolean = 2 == "a string"

  // part 1 - type class import

  import cats.Eq
  // part 2 - import TC instances for the types you need
  import cats.instances.int._

  // part 3 - use the TC API
  val intEquality: Eq[Int] = Eq[Int]

  val aTypeSafeComparison: Boolean = intEquality.eqv(2, 3)

  // val anUnsafeComparison = intEquality.eqv(2, "a tring") -- doesn't compile

  // part 4 - use extension methods (if applicable)

  import cats.syntax.eq._

  val anotherTypeSafeComp: Boolean = 2 === 3
  val negComp: Boolean = 2 =!= 3
  //val invalidComp: Boolean = 2 === "some string"

  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e. g lists

  import cats.instances.list._ // we bring Eq[List[Int]] in scope

  val aListComparison: Boolean = List(2) === List(3)

  // part 6 - create a TC instance for custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) => car1.price == car2.price }

  val compareTwoToyCars: Boolean = ToyCar("Model1", 5) === ToyCar("Model2", 5) // true

}
