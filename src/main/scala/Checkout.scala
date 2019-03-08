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

  /*********************************************************************************************************************
    *
    * Basket functions
    *
    */
  lazy val basket: List[Item] = List()

  def viewBasket(): List[Item] =
    basket

  def addToBasket(items: List[Item]) = {
    println(basket ::: items)
    basket ::: items
  }

  def removeItemFromBasket(item: Item) =
    addToBasket(testShop)

  /*********************************************************************************************************************
    *
    * Subtotal calculation functions
    *
    */
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

  /**
    * Calculates subtotal with a fold
    *
    * @param items Products to be purchased
    * @return Item subtotal
    */
  //  def calculateSubtotalWithFold(items: List[Item]): Int =
  //    items
  //      .map(_.price)
  //      .sum
  //
  def calculateSubtotalWithFold(items: List[Item]): Int =
    items.foldLeft(0) {
      case (acc, item) => acc + item.price
    }

  // TODO Work out what's happening from here (it's not been sorted yet)

  /**
    * Calculate total price of shop
    *
    * @param items Products to be purchased
    * @return Total price of shop
    */
  def calculateTotal(items: List[Item]) = {
    println("calculateTotal")
  }
}
//    items.filter(p: Item => Some(p.offer))
//    }
//    items.partition(p: Inventory.Item => Some(p.offer))
//    p: Inventory.Item => Boolean
//    val groupedByOffer =
//      items
//        .groupBy(_.offer)
//        .map(println)
////        val gbo = items.contains(Item.offer)
//    groupedByOffer.foreach(println)
//    //    println("items:                                 " + items)
//    println(".groupBy(_.sku):                       " + items.groupBy(_.sku))
//    println(
//      ".mapValues(_.size):                    " + items
//        .groupBy(_.sku)
//        .mapValues(_.size))

// How many of each item are there?
//    val skus: Map[String, List[Item]] = items.groupBy(_.sku)
//    println("skus -> " + skus)
//
//    def calculateItemTotal(skus: Map[String, List[Item]], acc: Int) = {
//      // For each item in the map...
//      skus(sku)
//      println("skus(acc) => " + skus("A"))
//      // Length of item list
//      // Is it on offer?
//      // Calculate price accordingly
//    }
//    calculateItemTotal(skus, 0)
//  }
//  calculateTotal(testShop)
// Use the offer option to work out the total item price
// Add the total item prices together (grand total)

//    val (promoItems: List[Inventory.Item],
//    nonPromoItems: List[Inventory.Item]) = items.collect({
//      case Item(sku, price, offer) =>
//        offer match {
//          case Some(offer: Offer) => (sku, price, offer)
//          case None => (sku, price)
//        }
//    })
//}}
//    println("itemsOnOffer -> " + itemsOnOffer)

//    val products: Map[String, List[Inventory.Item]] = items.groupBy(_.sku)
////    products.collect({case (sku:String, price:Int, offer: Offer) => offer})
//    products.collect({ case (sku: String, item: Inventory.Item) => sku })
////      .mapValues(_.size)
////    friends collect { case (id, p) if (p.age > 22) => p}

//  println(
//    "total: " + calculateTotal(List(itemC, itemB, itemC, itemD, itemB, itemA)))
//    items.flatMap(_.price)

//items // Dave's (better) version
//    .map(t => (t._1, t._2.length))
//    .map {
//      case (item, quantity) if offers.contains(item) =>
//        calculateItemTotal(item, quantity, offers(item))
//
//      case (item, quantity) =>
//        calculateItemTotal(item, quantity)
//    }
//    .sum
//  }

//    /**
//      * Calculate total price of n item(s) on offer
//      *
//      * @param sku      Singular product type
//      * @param quantity Number of items
//      * @return Total price
//      */
//    def calculateItemTotal(item: Item, quantity: Int): Int = {
//      val promo: Int = quantity / offer._1 * offer._2
//      val fullPrice: Int = quantity % offer._1 * stock(sku)
//      promo + fullPrice
//    }
//
//    /**
//      * Calculate total price of n non-promotional item(s)
//      *
//      * @param item     Singular product type
//      * @param quantity Number of items
//      * @return Total price
//      */
//    def calculateItemTotal(item: Item, quantity: Int): Int = {
//      item.price
//
//
//      if(item.)
//      if (Some(item.offer))
//        item.price * quantity
//      if ((item.offer).exists
//      else {
//          val promo: Int = quantity / item. * offer._2
//          val fullPrice: Int = quantity % offer._1 * stock(sku)
//          promo + fullPrice
//      }
//    }
//}
