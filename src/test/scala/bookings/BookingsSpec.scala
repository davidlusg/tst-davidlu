package bookings

import bookings.Bookings.{BestGroupPrice, CabinPrice, Rate, getBestGroupPrices}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers.{be, convertToAnyMustWrapper}

class BookingsSpec extends AnyWordSpec {

  "Bookings" when {
    "given rates and prices" should {
      "find the best group prices" in {

        val rate1 = Rate("M1", "Military")
        val rate2 = Rate("M2", "Military")
        val rate3 = Rate("S1", "Senior")
        val rate4 = Rate("S2", "Senior")
        val rates = Seq(rate1, rate2, rate3, rate4)

        val cabinPrice1 = CabinPrice(cabinCode = "CA", rateCode = "M1", price = BigDecimal(200.00))
        val cabinPrice2 = CabinPrice(cabinCode = "CA", rateCode = "M2", price = BigDecimal(250.00))
        val cabinPrice3 = CabinPrice(cabinCode = "CA", rateCode = "S1", price = BigDecimal(225.00))
        val cabinPrice4 = CabinPrice(cabinCode = "CA", rateCode = "S2", price = BigDecimal(260.00))
        val cabinPrice5 = CabinPrice(cabinCode = "CB", rateCode = "M1", price = BigDecimal(230.00))
        val cabinPrice6 = CabinPrice(cabinCode = "CB", rateCode = "M2", price = BigDecimal(260.00))
        val cabinPrice7 = CabinPrice(cabinCode = "CB", rateCode = "S1", price = BigDecimal(245.00))
        val cabinPrice8 = CabinPrice(cabinCode = "CB", rateCode = "S2", price = BigDecimal(270.00))
        val prices = Seq(cabinPrice1, cabinPrice2, cabinPrice3, cabinPrice4, cabinPrice5, cabinPrice6, cabinPrice7, cabinPrice8)

        // Best price for a cabin type (e.g. "CA") in a rate group (e.g. "Military")
        val bestGroupPrice1 = BestGroupPrice("CA", "M1", BigDecimal(200.00), "Military")
        val bestGroupPrice2 = BestGroupPrice("CA", "S1", BigDecimal(225.00), "Senior")
        val bestGroupPrice3 = BestGroupPrice("CB", "M1", BigDecimal(230.00), "Military")
        val bestGroupPrice4 = BestGroupPrice("CB", "S1", BigDecimal(245.00), "Senior")
        val expected = Seq(bestGroupPrice1, bestGroupPrice2, bestGroupPrice3, bestGroupPrice4)

        val actual = getBestGroupPrices(rates, prices)
        actual.size must be (expected.size)
        actual.toSet must be (expected.toSet)
      }
    }
  }

}
