package part3dataManipulation

import scala.annotation.tailrec

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
      if (n % 2 == 0) up100.fold(errList => Left(errList), _ => Right(n))
      else up100.fold(errList => Left(errList :+ "Must be even"), _ => Right(n))

    even
  }


  def main(args: Array[String]): Unit = {
    println(testNumber(0))
    println(testNumber(-5))
    println(testNumber(100))
    println(testNumber(101))
    println(testNumber(99))
    println(testNumber(17))
    println(testNumber(21))
    println(testNumber(13))
  }

}
