/**
  * An app to model a supermarket checkout
  * Test suite
  *
  * @author Keoni D'Souza
  */
import org.scalatest.{Matchers, WordSpec}

class CheckoutSpec extends WordSpec with Matchers {

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

  "Checkout.calculateSubtotalWithFold" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithFold(List.empty) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithFold(List(itemA)) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithFold(List(itemC, itemD)) shouldBe 35
    }

  }

}
