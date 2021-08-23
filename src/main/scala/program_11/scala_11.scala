package program_11

import scala.collection.mutable

/**
 * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
 * Example:
 *
 * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */

object Solution_1_scala_11 {

  /**
   *  Implementation using foldLeft method
   */
  def groupByChar(input : mutable.Seq[Char]) : mutable.Seq[Any] = {
    input.foldLeft(mutable.Seq.empty[mutable.Seq[Char]]) {
      case (acc , ch ) if acc.isEmpty => acc :+ mutable.Seq(ch)
      case (acc , ch ) if acc.last.last.equals(ch) => acc.updated(acc.length-1,(acc.last :+ ch))
      case (acc , ch ) => acc :+ mutable.Seq(ch)
    }.map(subList => if(subList.length>1) Tuple2(subList.length, subList.head) else subList.head)
  }

}

object Solution_2_scala_11 {

  /**
   *  Implementation using foldLeft method
   */
  def groupByCharWithTailRec[A](input : List[Char], acc : List[List[Char]] = List.empty) : List[Any] = {
    if(input.isEmpty) acc.map(subList => if(subList.length>1) Tuple2(subList.length, subList.head) else subList.head).toList
    else {
      val (packed, unpacked): (List[Char], List[Char]) = input.span(_ == input.head)
      groupByCharWithTailRec(unpacked,acc ++ List(packed))
    }
  }
}

object scala_11 extends App {

  val input1: mutable.Seq[Char] = mutable.Seq('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val input2: List[Char] = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

  println("Result after packing duplicate characters using foldLeft : "+Solution_1_scala_11.groupByChar(input1))
  println("Result after packing duplicate characters using tail recursion: "+Solution_2_scala_11.groupByCharWithTailRec(input2))

}

