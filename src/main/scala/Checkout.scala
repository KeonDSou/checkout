import CheckoutOutdated.calculateItemTotalOutdated
import InventoryOutdated.stock
import com.sun.xml.internal.bind.v2.TODO

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

  /**
    * Calculates subtotal with pattern matching
    *
    * @param items Products to be purchased
    * @return Item subtotal
    */
  def calculateSubtotalWithPM(items: List[Item]): Int =
    items match {
      case Nil =>
        0
      case h :: t =>
        h.price + calculateSubtotalWithPM(t)
    }

  /**
    * Calculates subtotal using tail recursion (wrapper function)
    *
    * @param items Products to be purchased
    * @return Item subtotal
    */
  def calculateSubtotalWithTailRecursion(items: List[Item]): Int = {

    /**
      * Calculates subtotal using tail recursion
      *
      * @param items Products to be purchased
      * @param acc   Accumulator
      * @return Keeps track of the accumulator
      */
    @tailrec
    def tailRecursion(items: List[Item], acc: Int): Int =
      items match {
        case Nil =>
          acc
        case h :: t =>
          tailRecursion(t, acc + h.price)
      }

    tailRecursion(items, 0)
  }

  // TODO Work out what's happening from here (it's not been sorted yet)
//  /**
//    * Calculates subtotal with a fold
//    *
//    * @param items Products to be purchased
//    * @return Item subtotal
//    */
//  def calculateSubtotalWithFold(items: List[Item],
//                                stock: Map[String, Int]): Int =
//    items
//      .map(p => stock.get(p)) // Using .get is a failsafe
//      .collect { case Some(i) => i }
//      .sum
//
//  /**
//    * Calculate total price of shop
//    *
//    * @param items  Products to be purchased
//    * @return Total price of shop
//    */
//  def calculateTotal(items: List[Item]): Int =
//    items // Dave's (better) version
//      .map(t => (t._1, t._2.length))
//      .map {
//        case (item, quantity) if offers.contains(item) =>
//          calculateItemTotal(item, quantity, offers(item))
//
//        case (item, quantity) =>
//          calculateItemTotal(item, quantity)
//      }
//      .sum
//
//  /**
//    * Calculate total price of n item(s) on offer
//    *
//    * @param sku      Singular product type
//    * @param quantity Number of items
//    * @return Total price
//    */
//  def calculateItemTotal(item: Item, quantity: Int): Int = {
//    val promo: Int = quantity / offer._1 * offer._2
//    val fullPrice: Int = quantity % offer._1 * stock(sku)
//    promo + fullPrice
//  }
//
//  /**
//    * Calculate total price of n non-promotional item(s)
//    *
//    * @param item     Singular product type
//    * @param quantity Number of items
//    * @return Total price
//    */
//  def calculateItemTotal(item: Item, quantity: Int): Int = {
//    item.offer match {
//      case None =>
//        item.price * quantity
//      case Some(item.offer) =>
//        val promo: Int = quantity / item. * offer._2
//        val fullPrice: Int = quantity % offer._1 * stock(sku)
//        promo + fullPrice
//    }
//  }
}
