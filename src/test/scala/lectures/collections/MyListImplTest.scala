package lectures.collections

import lectures.collections.MyListImpl.{MyIndexedList, MyList, MyListBuffer}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class MyListImplTest extends FlatSpec with Matchers {
  "MyList" should "work with type parameters" in {
    println("Extra tests")
    new MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).map(p => p * 2).data shouldBe List(2, 4, 6, 8, 10, 12)
    new MyList[Long, ListBuffer[Long]](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data shouldBe  List(2, 4, 6)
    new MyList[Int, List[Int]](List(1, 2, 3, 4, 5, 6)).foldLeft(0)((a, b) => a + b) shouldBe 21
    //  require(MyList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((tpl) => tpl._1 + tpl._2) == 0)
    new MyList[Float, IndexedSeq[Float]](ArrayBuffer.empty[Float]).foldLeft(0)((a,b) => a + b) shouldBe 0
  }

  "MyList" should "work for" in {
    new MyListBuffer[Long](ListBuffer(1, 2, 3, 4, 5, 6)).filter(_ % 2 == 0).data shouldBe List(2, 4, 6)
    new MyIndexedList[Float](ArrayBuffer.empty[Float]).foldLeft(0)((a, b) => a + b) shouldBe 0
  }
}
