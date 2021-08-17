package InterviewCollection

object FindSecondLasrgeNumber extends App {

  val list1 = List(8,40,20,60,2,8,9,100)
  val list2 = List(8)
  val list3 = List(10,8)

  /**
   * span method partitions the list in two directions left and right. Left side will have the elements who satisfies the condition
   * and right side holds the elements who doesn't satisfy the condition.
   *
   * accumulator :List(8)
   * accumulator :List(8, 40)
   * accumulator :List(8, 20, 40)
   * accumulator :List(8, 20, 40, 60)
   * accumulator :List(2, 8, 20, 40, 60)
   * accumulator :List(2, 8, 8, 20, 40, 60)
   * accumulator :List(2, 8, 8, 9, 20, 40, 60)
   *
   */
  def insertElementInSortedList(list: List[Int], element: Int): List[Int] = {
    val (lowerThan, greaterThan) = list.span(_ < element)
    lowerThan ++ List(element) ++ greaterThan
  }

  def getSortedList(list: List[Int]): List[Int] =
    list.foldLeft(List.empty[Int]){
      case (acc, element) => insertElementInSortedList(acc, element)
    }

  def secondLargestElement(list: List[Int]): Int = list match {
    case h :: Nil => h
    case _ => getSortedList(list).init.last
  }


  secondLargestElement(list1)
  //secondLargestElement(list2)
  //secondLargestElement(list3)
}
