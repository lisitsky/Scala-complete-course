package lectures.oop

import scala.annotation.tailrec


/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)): Int
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {


  def add(newValue: Int): BSTImpl = {
    this match {
      case b@BSTImpl(`newValue`, _, _) => this
      case b@BSTImpl(v, None, _) if v > newValue => copy(left = Some(BSTImpl(newValue, None, None))) //this.copy(left=Some(BSTImpl(v)))
      case b@BSTImpl(v, Some(leftBranch), _) if v > newValue => copy(left = Some(leftBranch.add(newValue)))
      case b@BSTImpl(v, _, None) if v < newValue => copy(right = Some(BSTImpl(newValue, None, None)))
      case b@BSTImpl(v, _, Some(rightBranch)) if v < newValue => copy(right = Some(rightBranch.add(newValue)))
    }
  }


  def find(value: Int): Option[BST] = this match {
    case BSTImpl(v, _, _) if v == value => Some(this)
    case BSTImpl(v, l, _) if v > value => l.flatMap(_.find(value))
    case BSTImpl(v, _, r) if v < value => r.flatMap(_.find(value))
  }

  def fold(aggregator: Int)(f: (Int, Int) =>(Int)): Int = BSTContainer.fromBST(this).map(_.bst.value).fold(aggregator)(f)
}

// BST Container is a class for BST linearization - each node in tree is put into
// an appropriate container. Position is a number of position: 1 is a tree root, 2 - left, 3 - right subnodes
// Rules:
//  - floor(log2(Position))+1 == level. 1 - root, 2 - first right under the root, 3 - their children and so on
//  - subnode has position == parent's position * 2 (for left/odd) or parent's position * 2 + 1 (for even/right)
//  - odd values - left, even - right subtree
//  - first element in a level has position == 2 ** level
//  - last element in a level has position  == 2 ** (level+1) - 1
//
// representation:
// level                      node
// 1                            1
// 2               2                          3
// 3       4             5             6             7
// 4    8     9       10    11     12     13     14    15
// 5    ....
//
case class BSTContainer(bst: BST, position: Int)

object BSTContainer {
  @tailrec
  private def fromBSTIter(nextNodes: Seq[BSTContainer], visitedNodes: Seq[BSTContainer] = Nil): Seq[BSTContainer] = nextNodes match {
    case Nil => visitedNodes
    case head :: tail =>
      val children = getChildrenList(head.bst, head.position*2)
      //println("children:", children)
      fromBSTIter(tail ++ children, visitedNodes :+ head)
  }

  def fromBST(bst: BST): Seq[BSTContainer] = fromBSTIter(Seq(BSTContainer(bst, 1)))

  private def getChildrenList(node: BST, position: Int): Seq[BSTContainer] =
    node.left.map(x=>List(BSTContainer(x, position+0))).getOrElse(Nil) ++
    node.right.map(x=>List(BSTContainer(x, position+1))).getOrElse(Nil)

  //@tailrec
  def listToString(bstcs: Seq[BSTContainer], width: Int, prev: Option[BSTContainer]=None): String = bstcs match {
    case Nil => ""
    case head :: tail => formatBSTContainer(head, width, prev) + listToString(tail, width, Option(head))
  }

  private def formatBSTContainer(bstc: BSTContainer, width: Int, prev: Option[BSTContainer]): String = {
    val level = getLevel(bstc.position)
    val prevLevel = prev.map(x => getLevel(x.position)).getOrElse(level)
    //println(level)
    val elementsOnLevel = Math.pow(2, level-1).toInt
    val elementWidth = width / elementsOnLevel
    val firstElementPosition = Math.pow(2, level-1).toInt
    val prevElementPosition = if (level == prevLevel && prev.nonEmpty) {prev.get.position} else {firstElementPosition}
    val offset = bstc.position - prevElementPosition
//    println(level, elementsOnLevel, elementWidth, prevElementPosition, offset)
    (if (level != prevLevel) "\n" else "") + " " * offset * elementWidth + (" " * (elementWidth / 2)  + bstc.bst.value.toString + " " * (elementWidth / 2))
  }

  private def getLevel(n: Int):Int = (Math.log(n) / Math.log(2)).floor.toInt + 1
}

object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
//  val nodesCount = sc.nextInt()
  val nodesCount = 5

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = root // generator goes here

  println(root)

  // add marker items
  println(s"Adding items: $markerItem, $markerItem2, $markerItem3")
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)
  //println(s"Result tree: \n${testTree.toS}")
//  val unraveled = bsfStart(testTree)
//  println("bsf: ", unraveled)
//  println(formatBSTCList(unraveled))

  val unraveled = BSTContainer.fromBST(testTree)
//  println("Unr:", unraveled)
  println("Print Tree ")
  println(BSTContainer.listToString(unraveled, 100))
  println("\n")
  println("Folded tree")
  println(testTree.fold(0)(_ + _))


  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  //println(testTree)
}