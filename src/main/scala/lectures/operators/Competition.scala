package lectures.operators

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[(String, Int)] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом числовое значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать, чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо
  * исправить ошибки компиляции
  * напечатать:
  * * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * * "Продули", если победили приезжие
  * * "Победила дружба" в случае ничьи
  *
  * Для решения задачи раскомментируйте тело объекта Competition
  * В целях упрощения можно поменять тип исходных данных
  */

object Competition extends App {

  val locals: Map[String, Int] = Map("Artem" -> 6, "Sergey" -> 5, "Anton" -> 2, "Vladimir" -> 2, "Alexander" -> 4)
  val foreigners = Map[String, Int]("John" -> 3, "James" -> 1, "Tom" -> 2, "Dick" -> 5, "Eric" -> 6)

  val results = for (l <- locals;
                     f <- foreigners) yield {
    val localName = l._1
    val localValue = l._2
    val foreignName = f._1
    val foreignValue = f._2
    val resultName = l._1 + " vs " + f._1
    val resultValue = l._2 - f._2
    //    println(resultName, resultValue)
    (resultName, resultValue)
  }

  var results2: Map[String, Int] = Map[String, Int]()
  for ((localName, localValue) <- locals) {
    for ((foreignName, foreignValue) <- foreigners)
      results2 += (localName + " vs " + foreignName) -> (localValue - foreignValue)
  }

  var results3 = Map[String, Int]()
  for (
    (localName, localValue) <- locals;
    (foreignName, foreignValue) <- foreigners
  ) {
      results3 += (localName + " vs " + foreignName) -> (localValue - foreignValue)
  }

  var results4 = Map[String, Int]()
  locals foreach ( l =>
    foreigners foreach( f =>
      results4 += (l._1 + " vs " + f._1) -> (l._2 - f._2)
    )
  )


  // Подсчет результатов соревнования различными способами
  // Стандартный императивный способ подсчета результата
  val calcResult1 = (results: Map[_, Int]) => {
    var finalResult = 0
    for (r <- results) {
      if (r._2 > 0) finalResult = finalResult + 1
      else if (r._2 < 0) finalResult = finalResult - 1
    }
    finalResult
  }

  // Подсчет с помощью свертки(?) в функциональном стиле
  val sign: Int => Int =  {
    case x: Int if x > 0 => +1
    case x: Int if x < 0 => -1
    case x: Int if x == 0 => 0
  }

  // fold
  val calcResult2 = (results: Map[_, Int]) => {
    results.foldLeft(0)((acc, x) => acc + sign(x._2))
  }

  // классификация каждого элемента +/- и суммирование
  val calcResult3 = (results: Map[_, Int]) => results.map(x => sign(x._2)).sum

  // суммирование + и - элементов
  val calcResult4 = (results: Map[_, Int]) => {
    val positives = results.count(x => x._2 > 0)
    val negatives = results.count(x => x._2 < 0)
    positives - negatives
  }

  val formatResults = (x: Int) => {
    x match {
      case n if n == 0 => "Победила дружба"
      case n if n >  0 => "Наша взяла"
      case n if n <  0 => "Продули"
    }
  }

  def makeResults(calcFn: Map[_, Int] => Int)(results: Map[_, Int]) = {
    val res = calcFn(results)
    println(res, formatResults(res))
  }


  println(calcResult1(results), formatResults(calcResult1(results)))
  println(calcResult2(results), formatResults(calcResult2(results)))
  println(calcResult3(results), formatResults(calcResult3(results)))
  println(calcResult4(results), formatResults(calcResult4(results)))

  println("====")
  makeResults(calcResult1)(results2)
  makeResults(calcResult4)(results3)
  makeResults(calcResult4)(results4)


}
