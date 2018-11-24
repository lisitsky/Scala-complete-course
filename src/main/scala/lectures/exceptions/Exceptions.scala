package lectures.exceptions

import scala.util.Try

private object PrintGreetings {

  case class Greeting(msg: String)

  private val data = Array(
    Greeting("Hi"),
    Greeting("Hello"),
    Greeting("Good morning"),
    Greeting("Good afternoon"),
    null,
    null
  )

  private[exceptions] def printGreetings(): Unit = {
    for (i <- 0 to 10) {
      Try[String] {
        data(i).msg
      }.map {
        println
      }.recover {
        case e: NullPointerException => println(s"Achtung NPE. $e")
        case e: ArrayIndexOutOfBoundsException => println(s"Achtung Out of Bound. $e")
        case e => println(s"Achtung - unknown $e")
      }
    }
  }
}

object Greeter extends App {
  PrintGreetings.printGreetings()
}
