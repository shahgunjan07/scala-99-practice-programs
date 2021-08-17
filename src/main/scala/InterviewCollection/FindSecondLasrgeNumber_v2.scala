package InterviewCollection

object FindSecondLasrgeNumber_v2 extends App {

  val list1 = List(8,40,20,60,2,8,9,100)
  val list2 = List(8)
  val list3 = List(10,8)

  def secondLargestElementWithFold(list: List[Int]): (Int, Int) = list.foldLeft((0, 0)) {
    case ((second, first), element) if(element > first)                          => (first, element)
    case ((second, first), element) if(element > second && element != first)     => (element, first)
    case ((second, first), element)                                              => (second, first)
  }

  def getSecondLargest(list: List[Int]): Int = {
    val (secondLargest, largest) = secondLargestElementWithFold(list)
    secondLargest
  }

}
