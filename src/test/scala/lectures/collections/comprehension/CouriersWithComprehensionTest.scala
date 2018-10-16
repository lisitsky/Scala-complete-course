package lectures.collections.comprehension

import lectures.collections.comprehension
import org.scalatest.{FunSuite, Matchers}

class CourierComprehensionTest extends FunSuite with Matchers {

  test("testCouriers") {
    Courier.couriers(5) shouldBe List(Courier(1), Courier(2), Courier(3), Courier(4), Courier(5))
    Courier.couriers(0) shouldBe List()
    Courier.couriers(-3) shouldBe List()
  }

  test("testAddresses") {
    Address.addresses(3) shouldBe List(Address("111"), Address("222"), Address("333"))
    Address.addresses(0) shouldBe List()
    Address.addresses(-1) shouldBe List()
  }

  test("testPrintServedAddresses") {

  }

  test("testServeAddresses") {
    val couriers = Courier.couriers(7)
    val addresses = Address.addresses(3)
    implicit var canServe:Option[Int] = Some(3)
    (CouriersWithComprehension.serveAddresses(addresses, couriers):List[Address]) shouldBe List(Address("111"), Address("222"), Address("333"))

    canServe = Some(8)
    (CouriersWithComprehension.serveAddresses(addresses, couriers):List[Address]) shouldBe List(Address("111"), Address("222"), Address("333"))

    canServe = None
    println(CouriersWithComprehension.serveAddresses(addresses, couriers))
//    (CouriersWithComprehension.serveAddresses(addresses, couriers):List[Address]) shouldBe List()
  }

  val couriers = Courier.couriers(7)
  val addresses = Address.addresses(3)
  implicit var canServe = Some(3)
  println(CouriersWithComprehension.serveAddresses(addresses, couriers))



}
