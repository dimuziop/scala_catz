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

  /* remember */
  val dontRecompute: Eval[Int] = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step 1 ....")
      "Put the guitar on your lap"
    }
    .map {
      step1 =>
        println("Step 2 ....")
        s"$step1 the put your left hand on the neck"
    }.memoize
    .map {
      steps12 =>
        println("Step 3, more complicated")
        s"$steps12 then with right hand strike the strings"
    }

  // TODO 2: implement defer such the defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  // TODO 3: rewrite the method with Eval
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.later(list)
    else Eval.later(reverseEval(list.tail).value :+ list.head)

  def main(args: Array[String]): Unit = {
    /*println(instantEval.value)
    println(redoEval.value)
    println(redoEval.value)
    println(delayedEval.value)
    println(delayedEval.value)
    println(composedEvaluation.value)
    println(composedEvaluation.value)
    println(evalEx1.value)
    println(evalEx1.value)
    println(dontRecompute.value)
    println(dontRecompute.value)
    println(tutorial.value)
    println(tutorial.value)
    defer(Eval.now {
      println("Now!")
      42
    })
    println(defer(Eval.now {
      println("Now!")
      42
    }).value)*/
    println(reverseEval(List(1,2,3,5)).value)
  }

}
