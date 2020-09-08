import scala.annotation.tailrec
import scala.collection.immutable.{SortedSet, TreeMap}
import io.StdIn.readInt

object FibApp {
  def main(args: Array[String]): Unit = {
    print("Enter the positive number for encoding: ")

    val n = readInt()
    val indexes = encode(n)

    println(s"Fibonacci notation: ${fibonacciNotation(indexes)}")
    println(s"Fibonacci coding: ${fibonacciCoding(indexes)}")
  }

  @tailrec def fibIter(n: Int, last: Int = 0, current: Int = 1): Int = n match {
    case 0 => last
    case 1 => current
    case _ => fibIter(n - 1, current, last + current)
  }

  def fib(n: Int): Int = fibIter(n)

  def fibMap(n: Int): TreeMap[Int, Int] = (0 to n + 2)
    .view
    .map(fib)
    .takeWhile(_ <= n)
    .zipWithIndex
    .map(_.swap)
    .to(TreeMap)

  def encode(n: Int): Set[Int] = {
    val fibonacciMap = fibMap(n)

    @tailrec def encodeIter(n: Int, accum: Set[Int]): Set[Int] = {
      if (0 >= n) accum
      else {
        fibonacciMap.filter(_._2 <= n).lastOption match {
          case None => encodeIter(0, accum)
          case Some(pair) => encodeIter(n - pair._2, accum + pair._1)
        }
      }
    }

    encodeIter(n, SortedSet())
  }

  def representation(indexes: Set[Int]): Seq[Int] =
    (2 to indexes.max)
      .map(indexes.contains _ andThen (if (_) 1 else 0))

  def fibonacciCoding(indexes: Set[Int]): String =
    (representation(indexes) :+ 1).mkString("")

  def fibonacciNotation(indexes: Set[Int]): String =
    representation(indexes).reverse.mkString("")
}
