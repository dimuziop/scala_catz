package part3dataManipulation

import cats.kernel.Semigroup

import scala.annotation.tailrec
import scala.util.Try

/**
 * User: pat
 * Date: 14/1/22
 * Time: 10:47
 */
object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value

  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")


  def isPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }

  // TODO: use Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val prime = if (isPrime(n)) Right[List[String], Int](n) else Left[List[String], Int](List("Must be a prime"))
    val nonNegative =
      if (n >= 0) prime.fold(errList => Left(errList), _ => Right(n))
      else prime.fold(errList => Left(errList :+ "Must be positive"), _ => Right(n))
    val up100 =
      if (n <= 100) nonNegative.fold(errList => Left(errList), _ => Right(n))
      else nonNegative.fold(errList => Left(errList :+ "Must be less than 100"), _ => Right(n))
    val even =
      if (n % 2 != 0) up100.fold(errList => Left(errList), _ => Right(n))
      else up100.fold(errList => Left(errList :+ "Must be even"), _ => Right(n))

    even
  }
  def testNumberDaniel(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isToBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (isPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && isPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n ,List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(isPrime(n), n, List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing Present"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("somthing".toInt))

  //backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2
  object FormValidation {
    //import cats.instances.string._
    implicit val combineIntMax: Semigroup[String] = Semigroup.instance[String]{(prev, post) => s"$prev | $post"}

    type FormValidation[T] = Validated[List[String], T]

    def validateName(name: String): FormValidation[String] =
      Validated.cond(!name.isBlank, name, List("Name mustn't be blank"))

    def validateEmail(email: String): FormValidation[String] =
      Validated.cond(email.contains('@'), email, List("Invalid email"))

    def validatePassword(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password must be longer than 9"))

    def validateKeyPresence[T](map: Map[String, T], key: String): Validated[List[String], T] =
      Validated.fromOption(map.get(key), List(s"$key must be present"))


    /*
      fields are
      - name
      - email
      - password
      rules are
      - name, email and password MUST be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters
     */

    def validateForm(form: Map[String, String]): FormValidation[String] =
      validateKeyPresence(form, "name")
        .andThen(validateName)
        .combine(validateKeyPresence(form, "email"))
        .andThen(validateEmail)
        .combine(validateKeyPresence(form, "password"))
        .andThen(validateEmail).map(_ => "User Registration Complete")
  }


  def main(args: Array[String]): Unit = {
    println(FormValidation.validateForm(Map("name" -> "Oscar", "email" -> "oscar@delahoya.com", "password" -> "12345678910")))
    println(FormValidation.validateForm(Map("name" -> "Oscar", "email" -> "oscardelahoya.com", "password" -> "178910")))
    println(FormValidation.validateForm(Map("email" -> "oscar@delahoya.com", "password" -> "12345678910")))
    println(testNumber(-5))
    println(testNumber(100))
    println(testNumber(101))
    println(testNumber(99))
    println(testNumber(17))
    println(testNumber(21))
    println(testNumber(13))
    println(validateNumber(13))
  }

}
