package program__6

/** Requirement : Find weather the given list is Palindrome ***/


  object Solution_1_scala_6 {

    /**
     * In this program, we will left shift each value from right to left
     * For example :
     * 1,2,3
     * 1,3,2
     * 3,1,2
     * 3,2,1
     */

    // Method to reverse a list
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

    def pallindromeCheck[A](input : List[A] )  : Boolean = {
      val reverseInput = reverse(input)
      if (input.mkString("").equals(reverseInput.mkString(""))) true else false
    }
  }

  object Solution_2_scala_6 {
    /**
     * Following method performs list reversal using tail recursion
     */

    // Method to reverse a list
    def reverse[A](input : List[A]) : List[A] = {
      def tailRec[A](input : List[A], result : List[A]) : List[A] = {
        input match {
          case Nil => result
          case head :: tail => tailRec(tail, head :: result)
        }
      }
      tailRec(input, List())
    }

    def pallindromeCheck[A](input : List[A] )  : Boolean = {
      val reverseInput = reverse(input)
      if (input.mkString("").equals(reverseInput.mkString(""))) true else false
    }
  }


  object scala_6 extends App {
    val data1 = List("a", "b", "c", "d", "e", "f", "g")
    val data2 = List(1, 4, 4, 1)
    val data3 = List("A", "B", "B", "A")

    val input1 = 8
    val input2 = 5

    println(s" Is data1 list palindrome  : ${Solution_1_scala_6.pallindromeCheck(data1)}")
    println(s" Is data1 list palindrome   : ${Solution_2_scala_6.pallindromeCheck(data1)}")

    println(s" Is data2 list palindrome  : ${Solution_1_scala_6.pallindromeCheck(data2)}")
    println(s" Is data2 list palindrome   : ${Solution_2_scala_6.pallindromeCheck(data2)}")

    println(s" Is data3 list palindrome  : ${Solution_1_scala_6.pallindromeCheck(data3)}")
    println(s" Is data3 list palindrome   : ${Solution_2_scala_6.pallindromeCheck(data3)}")

  }

