package part3dataManipulation

/**
 * User: pat
 * Date: 13/1/22
 * Time: 9:38
 */
object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))

  val (eleven,counted10) = countAndSay.run(10).value

  // state= "iterative" computations

  /*
  BAD CODE [Iterative]
   */
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied by 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}"))

  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap{ firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }

  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // function composition is clunky
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeFunc = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }






  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(compositeTransformation2.run(10).value)
    println()
  }

}
