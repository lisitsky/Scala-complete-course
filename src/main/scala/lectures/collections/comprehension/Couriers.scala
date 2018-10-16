package lectures.collections.comprehension

/**
  * Помогите курьерам разобраться с обслуживанием адресов
  *
  * Каждый день на работу выходит 'courierCount' курьеров
  * Им нужно обслужить 'addressesCount' адресов
  * Каждый курьер может обслужить courier.canServe адресов, но только при условии, что позволит дорожная ситуация.
  * Т.е. если trafficDegree < 5, то курьер обслужит все адреса, которые может, иначе - ни одного
  *
  * Входные данные для приложения содержат 2 строки
  * В первой строке - количество адресов, которые требуется обслужить
  * Во второй - количество курьеров, вышедших на работу.
  *
  * Ваша задача:
  *  Изучить код и переписать его так,
  *  что бы в нем не было ни одного цикла for, ни одной переменной или мутабильной коллекции
  *
  * Для этого используйте функции комбинаторы: filter, withFilter, fold, map, flatMap и т.д.
  *
  */

case class Traffic(degree: Double)

object Courier {
  def couriers(couriersCount: Int): List[Courier] = (1 to couriersCount).map{Courier(_)}.toList
}

case class Courier(index: Int) {
  val canServe = (Math.random() * 10).toInt
}

object Address {
  def addresses(addressesCount: Int): List[Address] = (1 to addressesCount).map{i=>Address(s"$i$i$i")}.toList
}

case class Address(postIndex: String)

object CouriersWithComprehension extends App {

  import Address._
  import Courier._

  val sc = new java.util.Scanner(System.in)
  val addressesCount = sc.nextInt()
  val courierCount = sc.nextInt()
  val addrs = addresses(addressesCount)
  val cours = couriers(courierCount)

  implicit val canServe = None // quick toggle
//  implicit val canServe = Some(8)

  // какие адреса были обслужены
  def serveAddresses(addresses: List[Address], couriers: List[Courier])(implicit canServe: Option[Int]) = {

    val canServer: Courier => Int = canServe match {
      case Some(value) => _ => value
      case None => _.canServe
    }

    couriers.flatMap(courier => {
      val trafficDegree = traffic().degree
      (0 until canServer(courier))
        .filter( _ => trafficDegree < 5)
    })
    .zip(1 to addresses.length)
    .map(v  => addresses(v._1))
  }

  def traffic(): Traffic = new Traffic(Math.random() * 10)

  def printServedAddresses(addresses: List[Address], couriers: List[Courier]) =
    serveAddresses(addresses, couriers).foreach(a => println(a.postIndex))

  printServedAddresses(addrs, cours)

}
