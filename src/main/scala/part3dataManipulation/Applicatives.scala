package part3dataManipulation

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
  //def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A,B)] = applicative.map(wa)(a => (b: B) => (a,b))
    applicative.ap(functionWrapper)(wb)
  }
  //  ap(a => applicative.map(wb)(b => (a, b)))(wa)
  //applicative.map(wa)(a => applicative.map(wb)(b => (a, b)))

  // Applicatives have this ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicative extends Semigroupal

  def main(args: Array[String]): Unit = {

  }

}
