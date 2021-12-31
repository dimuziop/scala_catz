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
  val combinationOption: Option[(Int, Char)] = numberOption.flatMap(n => charOption.map(c => (n,c)))
  val combinationOptionFor: Option[(Int, Char)] = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // future
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('a')
  // TODO 1.3 how do you create all combination of (number, char)?
  val combinationFuture: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map(c => (n,c)))
  val combinationFutureFor: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  def main(args: Array[String]): Unit = {
    println(combination1Point1)
    println(combinationOption)
    println(combinationFuture)
  }

}
