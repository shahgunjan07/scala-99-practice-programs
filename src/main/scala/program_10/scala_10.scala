package program_10

import scala.collection.mutable

/**
 * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
 *   Example:
 *
 *   scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 *   res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */

object Solution_1_scala_10 {

  /**
   *  Implementation using foldLeft method
   */
  def groupByChar(input : mutable.Seq[Char]) : mutable.Seq[Tuple2[Int, Char]] = {
    input.foldLeft(mutable.Seq.empty[mutable.Seq[Char]]) {
      case (acc , ch ) if acc.isEmpty => acc :+ mutable.Seq(ch)
      case (acc , ch ) if acc.last.last.equals(ch) => acc.updated(acc.length-1,(acc.last :+ ch))
      case (acc , ch ) => acc :+ mutable.Seq(ch)
    }.map(subList => Tuple2(subList.length, subList.head))
  }

}

object Solution_2_scala_10 {

  /**
   *  Implementation using foldLeft method
   */
  def groupByCharWithTailRec[A](input : List[Char], acc : List[List[Char]] = List.empty) : List[Tuple2[Int,Char]] = {
    if(input.isEmpty) acc.map(subList => Tuple2(subList.length, subList.head)).toList
    else {
      val (packed, unpacked): (List[Char], List[Char]) = input.span(_ == input.head)
      groupByCharWithTailRec(unpacked,acc ++ List(packed))
    }
  }
}

object scala_10 extends App {

  val input1: mutable.Seq[Char] = mutable.Seq('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val input2: List[Char] = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

  println("Result after packing duplicate characters using foldLeft : "+Solution_1_scala_10.groupByChar(input1))
  println("Result after packing duplicate characters using tail recursion: "+Solution_2_scala_10.groupByCharWithTailRec(input2))

}
