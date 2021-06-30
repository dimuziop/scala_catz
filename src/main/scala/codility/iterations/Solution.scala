package codility.iterations

import scala.annotation.tailrec

/**
 * User: patricio
 * Date: 29/6/21
 * Time: 08:13
 */
object Solution {

  implicit class IntToBin(int: Int) {
    @tailrec
    final def toBin(acc: String = "" ): String = {
      if (int < 2) return int + acc
      (int / 2).toBin(int % 2 + acc)
    }
  }

  def solution(n: Int): Int = {
    getBinaryGap(validateStrictRange(n, 1, 2147483647).toBin())
  }

  def getBinaryGap(str: String): Int = {
    val zeros = str.trim.split("1").filter(x => x != "")
    if (str.last == '0') return getLongerGap(zeros.slice(0, zeros.length - 1))
    getLongerGap(zeros)
  }

  @tailrec
  def getLongerGap(zeros: Array[String], longer: Int = 0): Int = {
    if (zeros.isEmpty) return longer
    val length = if (longer < zeros.head.length) zeros.head.length else longer
    if (zeros.tail.length == 0) return length
    getLongerGap(zeros.tail, length)
  }


  def validateStrictRange(n: Int, min: Int, max: Int): Int = if (n >= min && n <= max) n else throw new Exception("Out of range")

}


/*def main(args: Array[String]): Unit = {


  /*def f(arr:List[Int]):List[Int] = {
    arr.drop(1).sliding(1,2).flatten.toList
  }*/

  /*def f(delim:Int,arr:List[Int]):List[Int] = {
    validateStrictRange(arr.length, 1,100)
    arr.map(validateStrictMinusHtoH).filter(v => validateStrictMinusHtoH(v) < delim)
  }*/

  def validateStrictMinusHtoH(v: Int) = validateStrictRange(v, -100,100)
  def validateStrictRange(n: Int, min: Int, max: Int): Int = if (n >= min && n <= max) n else throw new Exception("Out of range")
  }
  }

  object Solution extends App {

    def f(num:Int) : List[Int] = {

      val list: List[Int] = List.range(0,validateStrictRange(num, 1, 100))

      print(list + "\n")
      return list
    }

    def validateStrictRange(n: Int, min: Int, max: Int): Int = if (n >= min && n <= max) n else throw new Exception("Out of range")

    println(f(readInt))
  }*/
