package program__9

import scala.collection.mutable

/**
 * Pack consecutive duplicates of list elements into sublists.
 * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 */

object Solution_1_scala_9 {


  /**
   *  Implementation using foldLeft method
   */
  def groupByChar(input : mutable.Seq[Char]) : mutable.Seq[mutable.Seq[Char]] = {
    input.foldLeft(mutable.Seq.empty[mutable.Seq[Char]]) {
      case (acc , ch ) if acc.isEmpty => acc :+ mutable.Seq(ch)
      case (acc , ch ) if acc.last.last.equals(ch) => acc.updated(acc.length-1,(acc.last :+ ch))
      case (acc , ch ) => acc :+ mutable.Seq(ch)
    }
  }


}

object Solution_2_scala_9 {

  /**
   *  Implementation using foldLeft method
   */
  def groupByCharWithTailRec[A](input : List[Char], acc : List[List[Char]] = List.empty) : List[List[Char]] = {
    if(input.isEmpty) acc
    else {
      val (packed, unpacked): (List[Char], List[Char]) = input.span(_ == input.head)
      groupByCharWithTailRec(unpacked,acc ++ List(packed))
    }
  }

}

object scala_9 extends App {

  val input1: mutable.Seq[Char] = mutable.Seq('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  val input2: List[Char] = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

  println("Result after packing duplicate characters using foldLeft : "+Solution_1_scala_9.groupByChar(input1))
  println("Result after packing duplicate characters using tail recursion: "+Solution_2_scala_9.groupByCharWithTailRec(input2))

}
