package part3dataManipulation

/**
 * User: pat
 * Date: 11/1/22
 * Time: 7:32
 */
object Evaluation {

  /*
    Cats makes the distinction between
    - evaluating an expression eagerly
    - evaluating lazily and every time you request it
    - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    64345
  }

  val redoEval: Eval[Int] = Eval.always {
    println("Computing again")
    4234
  }

  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later")
    54321
  }

  val composedEvaluation: Eval[Int] = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val anotherComposedEvaluation: Eval[Int] = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  // TODO 1:
  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  def main(args: Array[String]): Unit = {
    /*println(instantEval.value)
    println(redoEval.value)
    println(redoEval.value)
    println(delayedEval.value)
    println(delayedEval.value)
    println(composedEvaluation.value)
    println(composedEvaluation.value) */
    println(evalEx1.value)
    println(evalEx1.value)

  }

}
