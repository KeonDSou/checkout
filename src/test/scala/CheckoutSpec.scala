/**
  * An app to model a supermarket checkout
  * Test suite
  *
  * @author Keoni D'Souza
  */
import org.scalatest.{Matchers, WordSpec}

class CheckoutSpec extends WordSpec with Matchers {

  import InventoryOutdated._

  "CheckoutOutdated.calculateSubtotalWithRecursionOutdated" should {

    "Return 0 for an empty list" in {
      CheckoutOutdated.calculateSubtotalWithRecursionOutdated(List.empty, stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      CheckoutOutdated.calculateSubtotalWithRecursionOutdated(List("A"), stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      CheckoutOutdated.calculateSubtotalWithRecursionOutdated(List("C", "D"),
                                                              stock) shouldBe 35
    }

  }

  "CheckoutOutdated.calculateSubtotalWithPMOutdated" should {

    "Return 0 for an empty list" in {
      CheckoutOutdated.calculateSubtotalWithPMOutdated(List.empty, stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      CheckoutOutdated.calculateSubtotalWithPMOutdated(List("A"), stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      CheckoutOutdated.calculateSubtotalWithPMOutdated(List("C", "D"), stock) shouldBe 35
    }

  }

  "CheckoutOutdated.calculateSubtotalWithTailRecursionOutdated" should {

    "Return 0 for an empty list" in {
      CheckoutOutdated.calculateSubtotalWithTailRecursionOutdated(
        List.empty,
        stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      CheckoutOutdated.calculateSubtotalWithTailRecursionOutdated(
        List("A"),
        stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      CheckoutOutdated.calculateSubtotalWithTailRecursionOutdated(
        List("C", "D"),
        stock) shouldBe 35
    }

  }

  "CheckoutOutdated.calculateSubtotalWithFoldOutdated" should {

    "Return 0 for an empty list" in {
      CheckoutOutdated.calculateSubtotalWithFoldOutdated(List.empty, stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      CheckoutOutdated.calculateSubtotalWithFoldOutdated(List("A"), stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      CheckoutOutdated.calculateSubtotalWithFoldOutdated(List("C", "D"), stock) shouldBe 35
    }

  }

  "CheckoutOutdated.calculateTotalOutdated" should {

    "Return 0 for an empty shop" in {
      CheckoutOutdated.calculateTotalOutdated(List.empty, stock, offers) shouldBe 0
    }

    "Return 255 for a list of 3 As, 2 Bs, 1 20 and 4 15s, with offers calculated" in {
      CheckoutOutdated.calculateTotalOutdated(exampleShop, stock, offers) shouldBe 255
    }

  }

  "CheckoutOutdated.calculateItemTotalOutdated" should {

    "Return 0 for no B items" in {
      CheckoutOutdated.calculateItemTotalOutdated("B", 0, offers("B")) shouldBe 0
    }

    "Return 180 for four A items" in {
      CheckoutOutdated.calculateItemTotalOutdated("A", 4, offers("A")) shouldBe 180
    }

    "Return 60 for three C items" in {
      CheckoutOutdated.calculateItemTotalOutdated("C", 3) shouldBe 60
    }

    "Return 30 for two D items" in {
      CheckoutOutdated.calculateItemTotalOutdated("D", 2) shouldBe 30
    }

  }

  import Inventory._

  "Checkout.calculateSubtotalWithRecursion" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithRecursion(List.empty) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithRecursion(List(itemA)) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithRecursion(List(itemC, itemD)) shouldBe 35
    }

  }

  "Checkout.calculateSubtotalWithPM" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithPM(List.empty) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithPM(List(itemA)) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithPM(List(itemC, itemD)) shouldBe 35
    }

  }

  "Checkout.calculateSubtotalWithTailRecursion" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithTailRecursion(List.empty) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithTailRecursion(List(itemA)) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithTailRecursion(List(itemC, itemD)) shouldBe 35
    }

  }

}
