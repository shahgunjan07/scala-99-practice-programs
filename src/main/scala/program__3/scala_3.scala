package program__3

import program__3.Solution_1_scala_3.findNthElement

import java.util.NoSuchElementException

/** Requirement : Find Kth element of a list **/

object Solution_1_scala_3 {

  def findNthElement[A]( list: List[A], n : Int) : A = {
    (n, list) match {
      case (0, head :: tail) => head
      case (0, Nil) => throw new NoSuchElementException("Invalid Index")
      case (n, head :: tail) => findNthElement(tail, n-1)
    }
  }
}


object scala_3 extends App {
  val data = List("a", "b", "c", "d", "e", "f", "g")


  val input1 = 8
  val input2 = 5

  println(s"Element from ${input1} element in the list is : ${findNthElement(data, input1-1)}")
  //println(s"Element from ${input2} element in the list is : ${findNthElement(data, input2)}")

}