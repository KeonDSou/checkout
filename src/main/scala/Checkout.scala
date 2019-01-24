/**
  * An app to model a supermarket checkout
  *
  * @author Keoni D'Souza
  */

object Stuff {

  val stock: Map[Char, Int] = Map[Char, Int](
    // SKU -> price
    'A' -> 50,
    'B' -> 30,
    'C' -> 20,
    'D' -> 15
  )

  val offers: Map[Char, (Int, Int)] = Map[Char, (Int, Int)](
    // SKU -> (x for y)
    'A' -> (3, 130),
    'B' -> (2, 45)
  )

  val exampleShop = List('A', 'B', 'C', 'D', 'A', 'A', 'B', 'D', 'D', 'D')

}

object Checkout extends App {
  println("Checkout Kata\n")

  import Stuff._

  def calculateSubtotalWithRecursion(items: List[Char], stock: Map[Char, Int]): Int =
    if (items.isEmpty)
      0
    else {
      val item: Char = items.head
      val price: Int = stock(item)
      val rest: List[Char] = items.tail
      price + calculateSubtotalWithRecursion(rest, stock)
    }

  def calculateSubtotalWithPM(items: List[Char], stock: Map[Char, Int]): Int =
    items match {
      case Nil =>
        0
      case h :: t =>
        stock(h) + calculateSubtotalWithPM(t, stock)
    }

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

  def calculatingTotal(items: List[Char], stock: Map[Char, Int], offers: Map[Char, (Int, Int)]): Int =
    items.groupBy(identity).toList.map(t => {
      val (item, quantity) = (t._1, t._2.length)
      if (offers.contains(item))
        calculateTotal(item, quantity, offers(item))
      else
        calculateTotal(item, quantity)
    }).sum

  /**
    * Initiate checkout process
    *
    * @param items List of items to be purchased
    */
  def processShop(items: List[Char]): Unit = {

    val total = calculateSubtotalWithRecursion(items, stock)
    //    val promos = subtotal - total
    //    println("Promos:\t\t\t" + promos)
    println("Grand total:\t" + total)
  }

  processShop(exampleShop)

  /**
    * Calculate total price of n item(s) on offer
    *
    * @param sku      Alphabetical item identifier
    * @param quantity Number of items
    * @param offer    Tuple containing promotional information
    * @return Total price
    */
  def calculateTotal(sku: Char, quantity: Int, offer: (Int, Int)): Int = {
    val promo = quantity / offer._1 * offer._2
    val fullPrice = quantity % offer._1 * stock(sku)
    promo + fullPrice
  }

  /**
    * Calculate total price of n non-promotional item(s)
    *
    * @param sku      Alphabetical item identifier
    * @param quantity Number of items
    * @return Total price
    */
  def calculateTotal(sku: Char, quantity: Int): Int = {
    quantity * stock(sku)
  }

}
