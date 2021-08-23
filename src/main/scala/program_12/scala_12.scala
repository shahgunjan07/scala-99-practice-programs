package program_12

import scala.collection.mutable.Seq
/**
 * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
 * Example:
 *
 *
 * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
 * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 */

object Solution_1_scala_12 {
  def decodeToList(input : List[(Int, Char)]) : Seq[Char] = {
    input.foldLeft(Seq.empty[Char]) {
      case (acc, element) if element._1 == 1 => acc :+ element._2
      case (acc, element) => add(element._2, acc, element._1)
    }
  }

  def add(ch : Char, acc : Seq[Char], repeatCount : Int) : Seq[Char] = {
    repeatCount match {
      case 0 => acc
      // TODO : What is another way to add elements to list here ? Using for loop creates problem.
      case _ => add(ch, acc :+ ch, repeatCount-1)
    }
  }
}
object scala_12  extends  App {
  val input  = List((4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e'))
  println("List after decoding the content : "+Solution_1_scala_12.decodeToList(input))
}
