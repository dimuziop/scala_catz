package part1intro

/**
 * User: patricio
 * Date: 1/7/21
 * Time: 07:33
 */
object CatsIntro {

  // Eq
  val aComparison = 2 == "a string"

  // part 1 - type class import

  import cats.Eq
  // part 2 - import TC instances for the class you need
  import cats.instances.int._

  // part3 - use the TC api
  val intEquality = Eq[Int]
  val aTypeSafeComparision = intEquality.eqv(2, 3)
  //val aTypeUnSafeComparision = intEquality.eqv(2,"A string") -- do not compile

  // part 4 - use extension methods (if applicable)

  import cats.syntax.eq._

  val anotherTypeSafeComp = 2 === 3
  val neqTypeSafeComp = 2 =!= 3
  // val invalidComparison = 2 === "not compile" -- doesnt compile

  // WARNING: extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e,g Lists

  import cats.instances.list._ // we bring Eq[List[Int]] in scope

  val aListComparison = List(2) === List(3)

  // part 6 - create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCaeEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) => car1.price == car2.price }
  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lambo", 29.99)
  def main(args: Array[String]): Unit = {
    println(compareTwoToyCars)
  }

}
