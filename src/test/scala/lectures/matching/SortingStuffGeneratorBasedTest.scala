package lectures.matching

import lectures.matching.SortingStuff.{Book, Knife, Stuff, StuffBox, Watches, Boots}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов:
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста:
  * Для "find knife" теста создайте генератор, Option[Knife]. Тест должен показать, что если нож есть в вещах,
  * то метод findMyKnife его отыщет.
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предыдущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator: Gen[Book] = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen: Gen[Book] = bookGenerator.filter(_.isInteresting)
  val knifeGen: Gen[Option[Knife.type]] = Gen.option(Knife)
  val bootsGen: Gen[Boots] = Gen.zip(Gen.choose(min = 37, max = 45), Gen.oneOf("Converse", "Adidas")).map(w => Boots(w._2, w._1))

  // Override configuration if you need
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSize = 10)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)
        goodBooks should have size books.size
        niceWatches should have size watches.size
        junk should have size 0
      }
    }
    "find knife" which {
      "was occasionally disposed" in {
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val knife = knifeGen.sample.flatten
        val objectsToSort = Random.shuffle(books ++ List(knife).flatten).toList
        knife.isDefined shouldBe SortingStuff.findMyKnife(SortingStuff.sortJunk(objectsToSort))


      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in  {
        val ms = generatorDrivenConfig.minSuccessful
        val boots = (1 to ms) flatMap { _ => bootsGen.sample }
        val books = (1 to ms) flatMap { _ => bookGenerator.sample }
        val knife = knifeGen.sample.flatten
        val objectsToSort = Random.shuffle(boots ++ books ++ List(knife).flatten).toList
        val StuffBox(sortedBooks, sortedWatches, sortedBoots, _) = SortingStuff.sortJunk(objectsToSort)
        sortedBooks should have size books.size
        sortedBooks should not contain boots
        sortedWatches should not contain boots
        sortedBoots should have size boots.size
        sortedBoots.sortBy(Boots.unapply) shouldBe boots.sortBy(Boots.unapply)
      }

    }
  }
}
