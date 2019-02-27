import scala.annotation.tailrec

/**
  * An app to model a supermarket checkout
  * Source code (outdated)
  *
  * @author Keoni D'Souza
  */
object InventoryOutdated {

  // String SKUs mapping to prices
  val stock: Map[String, Int] = Map[String, Int](
    // SKU -> price
    "A" -> 50,
    "B" -> 30,
    "C" -> 20,
    "D" -> 15
  )

  // String SKUs mapping to offers
  val offers: Map[String, (Int, Int)] = Map[String, (Int, Int)](
    // SKU -> (x for y)
    "A" -> (3, 130),
    "B" -> (2, 45)
  )

  val exampleShop = List("A", "B", "C", "D", "A", "A", "B", "D", "D", "D")

}

object CheckoutOutdated extends App {

  //  def getTotal(items: List[Item]): Int = ???
  //
  //  case class Offer(quantity: Int, price: Int)
  //
  //  val myOffer = Offer(2, 3)
  //
  //  println(s"Offer: $myOffer")

  import InventoryOutdated._

  print(Console.UNDERLINED)
  println("Checkout Kata\n")
  print(Console.RESET)

  /**
    * Calculates subtotal with a recursive definition
    *
    * @param items Products to be purchased
    * @param stock String SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithRecursionOutdated(items: List[String],
                                             stock: Map[String, Int]): Int =
    if (items.isEmpty)
      0
    else {
      val item: String = items.head
      val price: Int = stock(item)
      val rest: List[String] = items.tail
      price + calculateSubtotalWithRecursionOutdated(rest, stock)
    }

  /**
    * Calculates subtotal with pattern matching
    *
    * @param items Products to be purchased
    * @param stock String SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithPMOutdated(items: List[String],
                                      stock: Map[String, Int]): Int =
    items match {
      case Nil =>
        0
      case h :: t =>
        stock(h) + calculateSubtotalWithPMOutdated(t, stock)
    }

  /**
    * Calculates subtotal using tail recursion (wrapper function)
    *
    * @param items Products to be purchased
    * @param stock String SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithTailRecursionOutdated(
      items: List[String],
      stock: Map[String, Int]): Int = {

    /**
      * Calculates subtotal using tail recursion
      *
      * @param items Products to be purchased
      * @param acc   Accumulator
      * @return Keeps track of the accumulator
      */
    @tailrec
    def tailRecursion(items: List[String], acc: Int): Int =
      items match {
        case Nil =>
          acc
        case h :: t =>
          tailRecursion(t, acc + stock(h))
      }

    tailRecursion(items, 0)
  }

  /**
    * Calculates subtotal with a fold
    *
    * @param items Products to be purchased
    * @param stock String SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithFoldOutdated(items: List[String],
                                        stock: Map[String, Int]): Int =
    items
      .map(p => stock.get(p)) // Using .get is a failsafe
      .collect { case Some(i) => i }
      .sum

  //      Other possible options
  //      .foldLeft(0)(_ + _)
  //      .foldRight(0)(_ + _)
  //      .foldLeft(0)((acc, next) => acc + next)
  //      .fold(0)(_ + _)

  /**
    * Calculate total price of shop
    *
    * @param items  Products to be purchased
    * @param stock  String SKUs mapping to prices
    * @param offers String SKUs mapping to offers
    * @return Total price of shop
    */
  def calculateTotalOutdated(items: List[String],
                             stock: Map[String, Int],
                             offers: Map[String, (Int, Int)]): Int =
    items
      .groupBy(identity) // identity is syntactic sugar for x => x
      .toList
      .map(t => {
        val (item, quantity) = (t._1, t._2.length)
        if (offers.contains(item))
          calculateItemTotalOutdated(item, quantity, offers(item))
        else
          calculateItemTotalOutdated(item, quantity)
      })
      .sum

  //  items // Dave's (better) version
  //    .map(t => (t._1, t._2.length))
  //    .map {
  //      case (item, quantity) if offers.contains(item) =>
  //        calculateItemTotal(item, quantity, offers(item))
  //
  //      case (item, quantity) =>
  //        calculateItemTotal(item, quantity)
  //    }.sum

  /**
    * Calculate total price of n item(s) on offer
    *
    * @param sku      Alphabetical item identifier
    * @param quantity Number of items
    * @param offer    Tuple containing promotional information
    * @return Total price
    */
  def calculateItemTotalOutdated(sku: String,
                                 quantity: Int,
                                 offer: (Int, Int)): Int = {
    val promo: Int = quantity / offer._1 * offer._2
    val fullPrice: Int = quantity % offer._1 * stock(sku)
    promo + fullPrice
  }

  /**
    * Calculate total price of n non-promotional item(s)
    *
    * @param sku      Alphabetical item identifier
    * @param quantity Number of items
    * @return Total price
    */
  def calculateItemTotalOutdated(sku: String, quantity: Int): Int = {
    quantity * stock(sku)
  }

}
