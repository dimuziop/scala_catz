package part1recap

/**
 * User: patricio
 * Date: 29/6/21
 * Time: 06:52
 */
object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  /*val impersonableString = ImpersonableString("Peter")
  impersonableString.greet ------> */ val greeting = "Peter".greet

  // importing implicit conversios in scope

  import scala.concurrent.duration._

  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount = 10
  val incremented2 = increment(2) // implicit argument is passed by the compiler

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)

  // more complex
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}"}
         |""".stripMargin.trim
  }

  val personsJson = listToJson(List(Person("Mike"), Person("Esther")))
  // implicit argument is used to PROVE TH EXISTENCE of a type

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String = s"""
                                               |{"${value.productElementName(0)}": "${value.productElement(0)}"}
                                               |""".stripMargin.trim
  }

  case class Cat(name: String)

  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))
  // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // can be used for implicit conversions (DISCOURAGED)




  def main(args: Array[String]): Unit = {

  println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
  println(oneArgCaseClassSerializer[Person].toJson(Person("No_Garfield")))
  }
}
