package part3dataManipulation

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/**
 * User: pat
 * Date: 10/1/22
 * Time: 12:30
 */
object Writers {

  import cats.data.Writer

  // 1 - define them at the start

  val aWriter: Writer[List[String], Int] = Writer(List("Initial value"), 45)
  // 2 - manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "Found something Interesting") // value stay the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "Found something Interesting", _ + 1)

  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "Found Something Interesting", value + 1)
  }

  import cats.instances.vector._

  // flatMap
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs

  import cats.instances.list._ // an implicit monoid to set 0 value

  val anEmptyWriter = aWriter.reset

  // 3 - dump either the value or the log
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // TODO 1: rewrite a function which "prints" things and writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    val writer = Writer(Vector("Starting"), n)

    @tailrec
    def innerFunction(writter: Writer[Vector[String], Int], int: Int): Writer[Vector[String], Int] = {
      if (int == 0) writter
      else {
        writter.map(_ - 1)
        innerFunction(writter, int - 1)
      }
    }

    innerFunction(writer, n)

  }

  def countAndLogSolution(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting!"), 0)
    else countAndLogSolution(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }
  // Benefit #1: we work as pure FP

  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSumFP(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(s"Now at $n"), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- naiveSumFP(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {

    println(compositeWriter.run)
    println(countAndSay(5))
    countAndLogSolution(5).written.foreach(println)
    println(naiveSumFP(5).run)

    Future(naiveSumFP(100)).foreach(println)
    Future(naiveSumFP(100)).foreach(println)

  }

}
