import scala.collection.mutable

/** Requirement : Reverse a given list ***/

  object Solution_1_scala_5 {

    /**
     * In this program, we will left shift each value from right to left
     * For example :
     * 1,2,3
     * 1,3,2
     * 3,1,2
     * 3,2,1
     */
    def reverse[A](list: List[A]) : List[A] = {
      val bufferedList = list.toBuffer
      for(i <-  0 to bufferedList.length-1) {
        for(j <- bufferedList.length-1 to 0 by -1 if j > i) {
          val leftValue = bufferedList(j-1)
          val rightValue = bufferedList(j)
          bufferedList(j-1) = rightValue
          bufferedList(j) = leftValue
        }
      }

      bufferedList.toList
    }
  }

  object Solution_2_scala_5 {
    /**
     * Following method performs list reversal using tail recursion
     */
    def reverse[A](input : List[A]) : List[A] = {
      def tailRec[A](input : List[A], result : List[A]) : List[A] = {
        input match {
          case Nil => result
          case head :: tail => tailRec(tail, head :: result)
        }
      }
      tailRec(input, List())
    }
  }

  object Solution_3_scala_5 {
    /**
     * Following method performs list reversal using  recursion
     */
    def reverse[A](input : List[A]) : List[A] = {
        input match {
          case Nil => Nil
          case head :: tail => reverse(tail) ::: List(head)
        }
    }
  }

  object Solution_4_scala_5 {
    /**
     * Following method performs list reversal using  pure function
     */
    def reverse[A](input : List[A]) : List[A] = {
      input.foldLeft(List[A]()) {(h,r) => r :: h}
    }
  }

  object scala_5 extends App {
    val data = List("a", "b", "c", "d", "e", "f", "g")

    val input1 = 8
    val input2 = 5

    println(s"Reverse List is using for loop logic  : ${Solution_1_scala_5.reverse(data)}")
    println(s"Reverse List is using tail recursion  : ${Solution_2_scala_5.reverse(data)}")
    println(s"Reverse List is using normal recursion  : ${Solution_3_scala_5.reverse(data)}")
    println(s"Reverse List is using pure function  : ${Solution_3_scala_5.reverse(data)}")

  }

