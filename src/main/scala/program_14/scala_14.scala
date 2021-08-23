package program_14

/***
 * Duplicate the elements of a list.
 * Example:
 *
 *
 * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */
object Solution_1_scala_14 {

  def duplicateElements(input : List[Char],acc : Seq[Char] = Seq.empty) : Seq[Char] = {
    input match {
      case Nil => acc
      case head :: tail => duplicateElements(tail, add(head, acc))
    }
  }

  def add(ch : Char, acc : Seq[Char], secondCall : Boolean = false): Seq[Char] = {
    secondCall match {
      case true => acc :+ ch
      case false => add(ch, acc :+ ch, true)
    }
  }
}

object scala_14 extends App {
  val input  = List('a','b','c','d','e','f','g')
  println("List after decoding the content : "+Solution_1_scala_14.duplicateElements(input))
}
