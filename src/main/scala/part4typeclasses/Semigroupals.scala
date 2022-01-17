package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

/**
 * User: pat
 * Date: 17/1/22
 * Time: 16:10
 */
object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aToupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled: Option[(Int, String)] = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

  import cats.instances.list._ // Monad[List]
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))
  val zippedLists = List(1,2).zip(List("a", "b"))

  // TODO: implement product with monads
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  // MONADS EXTENDS SEMIGROUPALS

  // example: Validated
  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "something else wrong")),
    Left(List("This can't be right"))
  )

  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(zippedLists)
    println(productWithMonads(List(1, 2), List("a", "b")))
    println(invalidsCombination)
    println(eitherCombination)
  }

}
