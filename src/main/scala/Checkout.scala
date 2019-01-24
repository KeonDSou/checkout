/**
  * An app to model a supermarket checkout
  *
  * @author Keoni D'Souza
  */

object Stuff {

  // Char SKUs mapping to prices
  val stock: Map[Char, Int] = Map[Char, Int](
    // SKU -> price
    'A' -> 50,
    'B' -> 30,
    'C' -> 20,
    'D' -> 15
  )

  // Char SKUs mapping to offers
  val offers: Map[Char, (Int, Int)] = Map[Char, (Int, Int)](
    // SKU -> (x for y)
    'A' -> (3, 130),
    'B' -> (2, 45)
  )

  val exampleShop = List('A', 'B', 'C', 'D', 'A', 'A', 'B', 'D', 'D', 'D')

}

object Checkout extends App {

  import Stuff._

  println("Checkout Kata\n")

  /**
    * Calculates subtotal with a recursive definition
    * @param items Products to be purchased
    * @param stock Char SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithRecursion(items: List[Char], stock: Map[Char, Int]): Int =
    if (items.isEmpty)
      0
    else {
      val item: Char = items.head
      val price: Int = stock(item)
      val rest: List[Char] = items.tail
      price + calculateSubtotalWithRecursion(rest, stock)
    }

  /**
    * Calculates subtotal with pattern matching
    * @param items Products to be purchased
    * @param stock Char SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithPM(items: List[Char], stock: Map[Char, Int]): Int =
    items match {
      case Nil =>
        0
      case h :: t =>
        stock(h) + calculateSubtotalWithPM(t, stock)
    }

  /**
    * Calculates subtotal with a fold
    * @param items Products to be purchased
    * @param stock Char SKUs mapping to prices
    * @return Item subtotal
    */
  def calculateSubtotalWithFold(items: List[Char], stock: Map[Char, Int]): Int =
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
    * @param items Products to be purchased
    * @param stock Char SKUs mapping to prices
    * @param offers Char SKUs mapping to offers
    * @return Total price of shop
    */
  def calculateTotal(items: List[Char], stock: Map[Char, Int], offers: Map[Char, (Int, Int)]): Int =
    items
      .groupBy(identity) // Also, x => x
      .toList.map(t => {
      val (item, quantity) = (t._1, t._2.length)
      if (offers.contains(item))
        calculateItemTotal(item, quantity, offers(item))
      else
        calculateItemTotal(item, quantity)
    }).sum

  /**
    * Calculate total price of n item(s) on offer
    *
    * @param sku      Alphabetical item identifier
    * @param quantity Number of items
    * @param offer    Tuple containing promotional information
    * @return Total price
    */
  def calculateItemTotal(sku: Char, quantity: Int, offer: (Int, Int)): Int = {
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
  def calculateItemTotal(sku: Char, quantity: Int): Int = {
    quantity * stock(sku)
  }

}
