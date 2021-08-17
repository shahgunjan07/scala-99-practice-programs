package program__7

import program__7.Solution_1_scala_7.flatten

/**
 * Requirement : Flatten a nested list structure
 *    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
 *    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 */

object Solution_1_scala_7 {

  def flatten[A](input : List[A]) : List[A] = {

    def flatten[A](accumulator : List[A], input : List[A]) : List[A] = {
      input match {
        case head :: tail => head match {
          case data : List[A] => flatten(flatten(accumulator, data), tail)
          case data : A => flatten(accumulator ::: List(data), tail)
        }
        case Nil => accumulator
      }
    }
    flatten(List(),input)
  }

}

object Solution_2_scala_7 {

  def flatten[A](input : List[A]) : List[A] = {
    input flatMap  {
      case elememt : List[A] => flatten(elememt)
      case e =>   List(e)
    }
  }


}


  object scala_7 extends App {
    val data1 = List("a", List("b"), List(List("c")),"d","e","f","g",List(List(List(List("h",List("i"), "j")))))

    println(s" Result after flatenning list using tail recursion : ${Solution_1_scala_7.flatten(data1)}")
    println(s" Result after flatenning list using built in flatMap method : ${Solution_2_scala_7.flatten(data1)}")

  }

