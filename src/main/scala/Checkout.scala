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
  val exampleShop = List('A', 'B', 'C', 'D', 'A', 'A', 'B', 'D', 'D', 'D')
}

object Checkout extends App {
  println("Checkout Kata\n")

  import Stuff._

  val offers = Map[Char, (Int, Int)](
    // SKU -> (x for y)
    'A' -> (3, 130),
    'B' -> (2, 45)
  )

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

  def calculatingTotal(items: List[Char], stock: Map[Char, Int], subtotal: Int): Int = {

    val total = items.groupBy(x => x)
    println("total: " + total)
    println("total('A'): " + calculateTotal('A', total('A').size, offers('A')))

    val jelly = total.keys.foreach(item => {
      println("item: " + item)
      println("item type: " + item.getType)
      if (offers.contains(item))
        calculateTotal(item, total(item).size, offers(item))
      else
        calculateTotal(item, total(item).size)
    })

    println("jelly: " + jelly)

    0

    //    calculateTotal('A', total('A').size, offers('A'))
    //    total('D').size
    //
    //    0

    //    val diff = subtotal - total

    // Calculate totals with offers applied
    // Map char skus to prices in stock
    // Use subtotal to work out promo difference
  }

  calculatingTotal(exampleShop, stock, 290)

  //  def calculateTotal(items: List[Char], offers: (Int, Int)): Int = {
  //    val Aps = calculateTotal('A', quantities('A'), offers('A'))
  //    val Bps = calculateTotal('B', quantities('B'), offers('B'))
  //    val Cps = calculateTotal('C', quantities('C'))
  //    val Dps = calculateTotal('D', quantities('D'))
  //    val total = Aps + Bps + Cps + Dps
  //  }

  calculateSubtotalWithFold(exampleShop, stock)

  /**
    * Initiate checkout process
    *
    * @param items List of items to be purchased
    */
  def processShop(items: List[Char]): Unit = {
    //    var quantities = List.fill(stock.size)(0)
    //    var quantities = stock.clone
    //    quantities mapValues(0)


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

  /*  // 3 for 30
    def specialA(quantity: Int): Int = {
      val promo = quantity / 3 * 30
      val fullPrice = quantity % 3 * 50
      promo + fullPrice
    }
  //  println("specialA " + specialA(10))

    // 2 for 45
    def specialB(quantity: Int): Int = {
      val promo = quantity / 2 * 45
      val fullPrice = quantity % 2 * 30
      promo + fullPrice
    }
  //  println("specialB " + specialB(5))

    // Filter A items
    def filterA(purchased: Array[String]): Int = {
      specialA(purchased.count((i: String) => i == "A"))
    }
  //  println("filterA " + filterA(Array("A","B","A","B")))

    // Filter B items
    def filterB(purchased: Array[String]): Int = {
      specialB(purchased.count((i: String) => i == "B"))
    }
  //  println("filterB " + filterB(Array("A","B","A","B")))

    // Filter C items
    def filterC(purchased: Array[String]): Int = {
      purchased.count((i: String) => i == "C")
    }
  //  println("filterC " + filterC(Array("C","B","D","B")))

    // Filter D items
    def filterD(purchased: Array[String]): Int = {
      purchased.count((i: String) => i == "D")
    }
  //  println("filterD " + filterD(Array("C","B","D","B")))*/

}
