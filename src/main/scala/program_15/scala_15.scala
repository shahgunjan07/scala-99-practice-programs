package program_15


/***
 * Duplicate the elements of a list a given number of times.
 * Example:
 *
 * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 */
object Solution_1_scala_15 {

  def duplicateN(input : List[Char], n : Int, acc : Seq[Char] = Seq.empty) : Seq[Char] = {
    input match {
      case Nil => acc
      case head :: tail => duplicateN(tail, n, add(head, acc, n))
    }
  }

  def add(ch : Char, acc : Seq[Char], n : Int): Seq[Char] = {
    n match {
      case 0 => acc
      case _ => add(ch, acc :+ ch, n-1)
    }
  }
}

object scala_15 extends App {
  val input  = List('a','b','c','d','e','f','g')
  println("List after decoding the content : "+Solution_1_scala_15.duplicateN(input,4))
}

