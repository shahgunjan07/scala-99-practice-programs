package program__8

import program__7.Solution_1_scala_7.flatten

/**
 * Eliminate consecutive duplicates of list elements
 * If a list contains repeated elements they should be replaced with a single copy of the element.
 * The order of the elements should not be changed.
 */

object Solution_1_scala_8 {

  def removeDuplicates[A](input : List[A]) : List[A] = {

      def removeDuplicates(result : List[A], currentList : List[A]): List[A] = {
        currentList match {
          case head :: tail => removeDuplicates(result ::: List(head), tail.dropWhile(_ == head))
          case Nil => result
      }
    }
    removeDuplicates(List(), input)
  }
}




object scala_8 extends App {
  /**
   * Note : For working of this solution the data must needs to be in sorted format
   */
  val data1 = List("a", "a", "a", "b", "c", "c", "d", "e", "e")

  println(s" Result after flatenning list using tail recursion : ${Solution_1_scala_8.removeDuplicates(data1)}")

}

