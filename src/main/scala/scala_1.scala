import Solution_1.reduceToIndice
import Solution_2.executeIfConditionMatch
import scala.collection.mutable.Buffer

/** Requirement : Find last element of a list **/
object Solution_1 {

  def reduceToIndice(input : Buffer[String], indice : Int) : Int = {
    input.length match {
      case 0 => indice
      case _ => reduceToIndice(input.tail, indice+1)
    }
  }
}

object Solution_2 {
  def executeIfConditionMatch(condition : => Boolean) (blockForNoMatch : => Unit)(blockForMatch : => Unit): Unit = {
    if(!condition) {
      blockForNoMatch
      executeIfConditionMatch(condition) (blockForNoMatch)(blockForMatch)
    } else {
      blockForMatch
    }
  }
}
object scala_1 extends App {
  val data = List("a", "b", "c", "d", "e", "f", "g").toBuffer[String]

  /**
   * Solution 1 using built in tail operator
   */
  val lastIndice = reduceToIndice(data, -1)
  println("Last element of the list is  : "+ data(lastIndice))

  /**
   * Solution 2 using currying
   */
  executeIfConditionMatch(data.length == 1){data -= data(0)} {
    println(s"Last element of the list is  : ${data.head}")
  }

}