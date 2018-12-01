package lectures.collections

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

  private[collections] class MyList[T, Cont <: Traversable[T]](data1: Traversable[T]) {
    val data: Traversable[T] = data1

    def flatMap(f: T => MyList[T, Cont]): MyList[T, Cont] = new MyList(data.flatMap(inp => f(inp).data))

    def map(f: T => T): MyList[T, Cont] = this.flatMap(x => new MyList[T, Cont](Seq(f(x))))

    final def foldLeft(acc: T)(op: (T, T) => T): T = {
      @tailrec
      def foldLeftStep(xs: Traversable[T], acc: T): T = xs match {
        case Nil => acc
        case head :: tail => foldLeftStep(tail, op(acc, head))
      }
      foldLeftStep(data, acc)
    }

    def filter(f: T => Boolean): MyList[T, Cont] =
      MyList.this.flatMap((x:T) => new MyList(List(x).filter(f)))
  }

  private[collections] class MyListBuffer[T](data1: Traversable[T]) extends MyList[T, ListBuffer[T]](data1)

  private[collections] class MyIndexedList[T](data1: Traversable[T]) extends MyList[T, ArrayBuffer[T]](data1)

}
