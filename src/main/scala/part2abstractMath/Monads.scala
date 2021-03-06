package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

/**
 * User: pat
 * Date: 31/12/21
 * Time: 8:56
 */
object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1 how do you create all combination of (number, char)?
  val combination1Point1: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationLstFor: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  //options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')
  // TODO 1.2 how do you create all combination of (number, char)?
  val combinationOption: Option[(Int, Char)] = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationOptionFor: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('a')

  // TODO 1.3 how do you create all combination of (number, char)?
  val combinationFuture: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationFutureFor: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
  Pattern
   - wrapping value into a M value
   - the flatMap mechanism

   MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    //TODO: implement this
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  // Cats monad

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformation: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._

  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(12)
  val aTransformedList: List[Int] = listMonad.flatMap(aList)(x => List(x, x + 1))

  // TODO 2: use a Monad[Future]

  import cats.instances.future._

  val futureMonad: Monad[Future] = Monad[Future] //require an implicit execution context
  val aFuture: Future[Int] = futureMonad.pure(5)
  val aTransformedFuture: Future[Int] = futureMonad.flatMap(aFuture)(x => Future(x * 25))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatMap
  import cats.syntax.applicative._
  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] will be use => Some(1)

  import cats.syntax.flatMap._ // flatmap isHere
  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monads extends Functors
  val oneOptionMapped: Option[Int] = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor._ // map is here
  val oneOptionMapped2: Option[Int] = oneOption.map(_ + 2)

  // for-comprehesions
  val composedOptionFor: Option[Int] = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions

  def getPairsForShorter[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    ma.flatMap(a => mb.map(b => (a, b)))

  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)


  def main(args: Array[String]): Unit = {
    println(combination1Point1)
    println(combinationOption)
    println(combinationFuture)
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)
    println(getPairsForShorter(numbersList, charsList))
  }

}
