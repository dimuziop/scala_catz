package codility.arrays.oddocurrencies

/**
 * User: patricio
 * Date: 29/6/21
 * Time: 08:17
 */
class Solution {

  def solution(a: Array[Int]): Int = {
    validateStrictRange(validateOddInteger(a.length), 1, 1000000)
    validateOneValueOccurOnce(
      a.map(value => validateStrictRange(value, 1, 1000000000))
        .groupBy(identity)
        .collect { case (x, Array(_)) => x }.toArray
    ).head
  }

  def validateOneValueOccurOnce(value: Array[Int]): Array[Int] = if (value.length != 1) throw new Exception("Single elements should occurs once") else value

  def validateStrictRange(n: Int, min: Int, max: Int): Int = if (n >= min && n <= max) n else throw new Exception("Out of range")

  def validateOddInteger(value: Int): Int = if (value % 2 == 0) throw new Exception("Even Integer") else value
}
