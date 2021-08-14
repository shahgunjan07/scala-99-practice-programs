
  /** Requirement : Find total number of elements of a list ***/

  object Solution_1_scala_4 {

    def countIteration[A](list: List[A], n : Int) : Int = {
      list match {
        case head :: tail => countIteration(tail, n+1)
        case Nil => n
        case head :: Nil => n+1
      }
    }
  }


  object scala_4 extends App {
    val data = List("a", "b", "c", "d", "e", "f", "g")

    val input1 = 8
    val input2 = 5

    println(s"Total number of elements of list are : ${Solution_1_scala_4.countIteration(data, 0)}")

  }

