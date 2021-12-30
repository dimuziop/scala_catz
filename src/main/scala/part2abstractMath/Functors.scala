package part2abstractMath

import scala.util.Try

/**
 * User: pat
 * Date: 30/12/21
 * Time: 15:51
 */
object Functors {

  val aModifiedList: List[Int] = List(1, 2, 3).map(_ + 1)
  val aModifiedOption: Option[Int] = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry: Try[Int] = Try(2).map(_ + 1) // Success(3)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._

  val listFunctor: Functor[List] = Functor[List]

  def incrementedNumbers: List[Int] = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._

  val optionFunctor: Functor[Option] = Functor[Option]

  def incrementedOption: Option[Int] = optionFunctor.map(Option(2))(_ + 1)

  import cats.instances.try_._

  val tryFunctor: Functor[Try] = Functor[Try]

  def incrementedTry: Try[Int] = tryFunctor.map(Try(2))(_ + 1)

  //generalizing the API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10X[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree

  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]

  object Tree {
    /* Smart constructors */
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] =
      Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int]= Tree.branch(40, Tree.branch(5, Tree.leaf(5), Tree.leaf(5)), Tree.leaf(43))
  tree.map(_ + 1)

  // TODO 2 write a shorted do10x method using extension methods

  def main(args: Array[String]): Unit = {
    println(do10X(List(1, 2, 3)))
    println(do10X(Option(4)))
    println(do10X(Try(5)))
    //println(do10X(Branch(30, Leaf(10), Leaf(50)))) || invariance error
    println(do10X[Tree](Branch(30, Leaf(10), Leaf(50))))
    println(do10X(Tree.branch(30, Tree.leaf(10), Tree.leaf(50))))
  }

}
