package part3dataManipulation

import cats.Monoid

/**
 * User: pat
 * Date: 18/1/22
 * Time: 8:32
 */
object Applicatives {

  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._

  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Monads extend Applicatives
  // Applicatives extend Functors
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)
  val validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]


  // TODO: though experiment
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] =
    applicative.map(wa)(a => applicative.map(wb)(b => (a, b)))

  def main(args: Array[String]): Unit = {

  }

}