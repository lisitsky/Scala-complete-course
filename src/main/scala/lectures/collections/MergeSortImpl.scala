package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = data match {
    case Nil => Nil
    case a@Seq(_) => a
    case _ =>
      val mid: Int = data.length / 2
      val (first, last) = data splitAt mid
      val firstSorted = mergeSort(first)
      val lastSorted = mergeSort(last)
      merge(firstSorted, lastSorted)
  }

  private def merge2(left: Seq[Int], right: Seq[Int]): Seq[Int] = {
    if (left.isEmpty) {right}
    else if (right.isEmpty) {left}
    else {
      if (left.head < right.head) {
        Seq(left.head) ++ merge(left.tail, right)
      } else {
        Seq(right.head) ++ merge(left, right.tail)
      }
    }
  }


  private def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = (left, right) match {
    case (Nil, _) => right
    case (_, Nil) => left
    case (_, _) if left.head < right.head => Seq(left.head) ++ merge(left.tail, right)
    case (_, _) => Seq(right.head) ++ merge(left, right.tail)

  }


//  val sorted = mergeSort(Seq(10, 9, 3, 8, 5, 2, -9, 11, 0, 9, 3))
//  println(Seq(10, 9, 3, 8, 5, 2, -9, 11, 0, 9, 3))
//  println(mergeSort(10, 9, 3, 8, 5, 2, -9, 11, 0, 9, 3))
//  println(unpackLists(toLists(List(10, 9, 3, 8, 5, 2, -9, 11, 0, 9, 3))))
//
//  println(sorted)


  /// OTHER STUFF ///

  // данный вариант не работает из-за бесконечной рекурсии - надо придумать как останавливаться на листах 0 и 1 длины
//  private def toLists2(data: List[Int]): List[List[Int]] = data.grouped(data.length / 2 + data.length % 2).toList map toLists2 reduce {(acc, x) => acc ++ x}
//
//    работает в таком варианте
//  private def toLists3(data: List[Int]): List[List[Int]] = {
//    data.grouped(data.length / 2 + data.length % 2).toList map {
//      case Nil => List()
//      case a@List(_) => List(a)
//      case v@_ => toLists3(v)
//    } reduce {(acc, x) => acc ++ x}
//  }


  //  private def toLists(data: Seq[Int]): List[Int] = {
  //    for (d <- data) yield d
  //  }

  //  private def toLists(data: List[Int]): List[List[Int]] = data match {
  //    case Nil => List(List[Int]())
  //    case a@List(v) => List(a)
  //    case _ => {
  //      val mid: Int = data.length / 2
  //      val (first, last) = data splitAt mid
  //      toLists(first) ::: toLists(last)
  //    }
  //  }

  //  private def unpackLists(data: List[List[Int]]): List[Int] = {
  //    for (lst <- data) yield lst.head
  //  }

//  private def mergeLists(data: List[List[Int]]): List[Int] = data match {
//    case l:List[List[Int]] if l.isEmpty => List()
//    case List(oneList) => oneList
//    case _ => {
//      val mid: Int = data.length / 2 + data.length % 2
//      val (first, last) = data splitAt mid
//      val firstMerged = mergeLists(first)
//      val lastMerged = mergeLists(last)
//      merge(firstMerged, lastMerged)
//    }
//  }
//
//  private def mergeLists2(data: List[Int]): List[Int] = {
//    val mid: Int = data.length / 2 + data.length % 2
//    val (first, last) = data splitAt mid
//  }

  //  println(toLists(List(1)))
  //  println(toLists(List(1,2)))
  //  println(toLists(List(1,2,3)))
  //
  //  println(toLists3(List(1)))
  //  println(toLists3(List(1,2)))
  //  println(toLists3(List(1,2,3,4,5,6,7,8)))
  //
  //  val v = List(1,2,3,4,5,6,7,8)
  //  val s = merge(toLists(v)(3), toLists(v)(1))
  //  println(s)
  ////  val q = mergeStart(toLists3(v))
  ////  println(q)
  //
  //  println("=====")
  //  println(mergeLists(List(List(1,4,5), List(2, 1, 8, 7))))
  //  println(merge(List(1,4,5), List(1,7,9)))


}
