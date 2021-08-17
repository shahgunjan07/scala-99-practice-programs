package program__6

/** Requirement : Find weather the given list is Palindrome ***/


  // Method to reverse a list
  object Solution_1_scala_6_v2 {
    /**
     * Following method performs list reversal using tail recursion
     */

    // Method to reverse a list
    def reverse[A](input : List[A]) : List[A] = {
      input.foldLeft(List[A]()) {(l, r) => r :: l }
    }

    def isEven(number : Int) : Boolean = if(number%2 == 0) true else false

  /**
   * To avoid iterating over a full size list, the best way to check for Palindrome is :
   * Compare first half list with reversed elements of second half list
   */
    def pallindromeCheck[A](input : List[A] )  : Boolean = {
      input.size match {
        case 1 => true
        case 0 => throw  new IllegalArgumentException("Invalid input")
        case _ => {

          /**
           * Note : Here, slice method takes two index to split the array.
           *  first index : It starts from zero. So important to substract -1 from the calculated positions
           *  second index : It starts from 1. So no need to substract 1
           *
           *  Aslo, note that, slice method will take care of index which may cause Array out of bound error
           */
          val subListIndex1: Int = input.size/2
          val subListIndex2 : Int = if(isEven(input.size))  (input.size/2)+1 else input.size-(input.size/2)+1
          input.slice(0,subListIndex1).equals(reverse(input.slice(subListIndex2-1, input.length)))
        }
      }

    }
  }


  object scala_6_v2 extends App {
    val data1 = List("a", "b", "c", "d", "e", "f", "g")
    val data2 = List(1, 4, 4, 1)
    val data3 = List("A", "B", "B", "A")

    val input1 = 8
    val input2 = 5

    println(s" Is data1 list palindrome  : ${Solution_1_scala_6_v2.pallindromeCheck(data1)}")

    println(s" Is data2 list palindrome  : ${Solution_1_scala_6_v2.pallindromeCheck(data2)}")

    println(s" Is data3 list palindrome  : ${Solution_1_scala_6_v2.pallindromeCheck(data3)}")

  }

