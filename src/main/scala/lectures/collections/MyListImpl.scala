package lectures.collections

import scala.annotation.tailrec

/**
  * Представим, что по какой-то причине Вам понадобилась своя обертка над списком целых чисел List[Int]
  *
  * Вы приняли решение, что будет достаточно реализовать 4 метода:
  * * * * * def flatMap(f: (Int => MyList)) -  реализуете на основе соответствующего метода из List
  * * * * * метод map(f: (Int) => Int) - с помощью только что полученного метода flatMap класса MyList
  * * * * * filter(???) - через метод flatMap класса MyList
  * * * * * foldLeft(acc: Int)(???) - через декомпозицию на head и tail
  *
  * Для того, чтобы выполнить задание:
  * * * * * раскомментируйте код
  * * * * * замените знаки вопроса на сигнатуры и тела методов
  * * * * * не используйте var и мутабильные коллекции
  *
  */
object MyListImpl extends App {

  case class MyList(data: List[Int]) {

    def flatMap(f: Int => MyList) =
      MyList(data.flatMap(inp => f(inp).data))

    def map(f: Int => Int): MyList = this.flatMap(x => MyList(List(f(x))))
//    def map(f: Int => Int): MyList = this.flatMap(f andThen List[Int] _ andThen MyList)

    @tailrec
    final def foldLeft(acc: Int)(f: (Int, Int) => Int): Int = data match {
      case Nil => acc
      case head :: tail => MyList(tail).foldLeft(f(acc, head))(f) // хвостовая? не является ли оверхедом переупаковка в MyList?
    }

    def filter(f: Int => Boolean): MyList =
      MyList.this.flatMap(x => if (f(x)) MyList(List(x)) else MyList(List()))
  }

  require(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2).data == List(2, 4, 6, 8, 10, 12))
  require(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data == List(2, 4, 6))
  require(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0){(a, b) => a + b} == 21)
  require(MyList(Nil).foldLeft(0)((acc, i) => acc + i) == 0)

  println( MyList(List(1, 2, 3, 4, 5, 6)).flatMap(x => MyList(List(x*3))) )
  println(MyList(List(1, 2, 3, 4, 5, 6)).map(_ * 2))
  println(MyList(List(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0))
  println(MyList(List(1, 2, 3, 4, 5, 6)).foldLeft(0){(a, b) => a + b})
}