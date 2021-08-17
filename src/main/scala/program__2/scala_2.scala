package program__2

import program__2.Solution_1_scala_2.executeIfConditionMatch

/** Requirement : Find last but one element of a list **/
/** Requirement : Find second last element of a list **/

object Solution_1_scala_2 {

  def executeIfConditionMatch(condition : => Boolean) (blockForNoMatch : => Unit)(blockForMatch : => Unit): Unit = {
    if(!condition) {
      blockForNoMatch
      executeIfConditionMatch(condition) (blockForNoMatch)(blockForMatch)
    } else {
      blockForMatch
    }
  }
}


object scala_2 extends App {
  val data = List("a", "b", "c", "d", "e", "f", "g").toBuffer[String]


  /**
   * Solution 2 using currying
   */
  executeIfConditionMatch(data.length == 2){data -= data(0)} {
    println(s"Second Last element of the list is  : ${data(0)}")
  }


}