import org.scalatest.{Matchers, WordSpec}

class CheckoutSpec extends WordSpec with Matchers {

  import Stuff._

  "Checkout.calculateSubtotalWithRecursion" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithRecursion(List.empty, stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithRecursion(List('A'), stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithRecursion(List('C', 'D'), stock) shouldBe 35
    }

  }

  "Checkout.calculateSubtotalWithPM" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithPM(List.empty, stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithPM(List('A'), stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithPM(List('C', 'D'), stock) shouldBe 35
    }

  }

  "Checkout.calculateSubtotalWithFold" should {

    "Return 0 for an empty list" in {
      Checkout.calculateSubtotalWithFold(List.empty, stock) shouldBe 0
    }

    "Return 50 for a list with an A" in {
      Checkout.calculateSubtotalWithFold(List('A'), stock) shouldBe 50
    }

    "Return 35 for a list with a C and a D" in {
      Checkout.calculateSubtotalWithFold(List('C', 'D'), stock) shouldBe 35
    }

  }

    "Checkout.calculateTotal" should {

      "Return 0 for an empty shop" in {
        Checkout.calculatingTotal(List.empty, stock, offers) shouldBe 0
      }

      "Return 255 for a list of 3 As, 2 Bs, 1 20 and 4 15s, with offers calculated" in {
        Checkout.calculatingTotal(exampleShop, stock, offers) shouldBe 255
      }
    }

}
