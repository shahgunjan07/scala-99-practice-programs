package program_13


/**
 * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
 * Example:
 *
 *
 * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
 * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object Solution_1_scala_13 {

  def lengthEncoding(input : List[Char], acc :Seq[Tuple2[Int, Char]] = Seq.empty): Seq[Tuple2[Int, Char]] ={
    if(input.isEmpty) acc
    else {
      val (grouped, ungrouped) = input.span(_ == input.head)
      lengthEncoding(ungrouped, acc :+ (grouped.length, grouped.head))
    }
  }
}

object scala_13 extends  App {
  val input  = List('a','a','a','a','b','c','c','a','a','d','e','e','e','e')
  println("List after decoding the content : "+Solution_1_scala_13.lengthEncoding(input))
}
