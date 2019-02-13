import scala.annotation.tailrec

/**
  * An app to model a supermarket checkout
  * Source code
  *
  * @author Keoni D'Souza
  */
object Inventory {

  final case class Item(sku: String, price: Int, offer: Option[Offer])

  final case class Offer(quantity: Int, price: Int)

  val itemA = Item("A", 50, Option(Offer(3, 130)))
  val itemB = Item("B", 30, Option(Offer(2, 45)))
  val itemC = Item("C", 20, None)
  val itemD = Item("D", 15, None)

  val testShop: List[Item] =
    List(itemA, itemB, itemC, itemD, itemA, itemA, itemB, itemD, itemD, itemD)

}

object Checkout extends App {

  import Inventory._

  print(Console.UNDERLINED)
  println("Checkout Kata\n")
  print(Console.RESET)

  /**
    * Calculates subtotal with a recursive definition
    *
    * @param items Products to be purchased
    * @return item subtotal
    */
  def calculateSubtotalWithRecursion(items: List[Item]): Int =
    if (items.isEmpty)
      0
    else {
      val price: Int = items.head.price
      val rest: List[Item] = items.tail
      price + calculateSubtotalWithRecursion(rest)
    }

//  /**
//    * Calculates subtotal with pattern matching
//    *
//    * @param items Products to be purchased
//    * @param stock String SKUs mapping to prices
//    * @return Item subtotal
//    */
//  def calculateSubtotalWithPM(items: List[String],
//                              stock: Map[String, Int]): Int =
//    items match {
//      case Nil =>
//        0
//      case h :: t =>
//        stock(h) + calculateSubtotalWithPM(t, stock)
//    }

}
