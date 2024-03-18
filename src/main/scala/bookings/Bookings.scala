package bookings

import scala.collection.immutable

object Bookings {

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String,
                        rateCode: String,
                        price: BigDecimal)

  case class BestGroupPrice(cabinCode: String,
                            rateCode: String,
                            price: BigDecimal,
                            rateGroup: String)

  private def minCabin(cabin1: CabinPrice, cabin2: CabinPrice): CabinPrice = if (cabin1.price < cabin2.price) cabin1 else cabin1

  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    val rateCodeToGroup: Map[CabinCode, String] =
      rates.map { rate => rate.rateCode -> rate.rateGroup }.toMap

    //("Military" -> List("M1", "M2"))
    //("Senior" -> List("S1", "S2"))
    //("BlahGroup" -> List("BlahCode1", "BlahCode2")
    val allCodesByGroup: Map[String, Seq[String]] =
      rates.map { x => x.rateGroup -> x.rateCode }.groupBy(_._1).map(f => (f._1, f._2.map(_._2)))

    type CabinCode = String
    val cabinPricesByGroup: Seq[Seq[CabinPrice]] =
      allCodesByGroup.toSeq.map {
        case (_, rateCodes) =>
          prices.filter { aPrice =>
            rateCodes.contains(aPrice.rateCode)
          }
      }

    val baz: Seq[immutable.Iterable[CabinPrice]] =
      cabinPricesByGroup.map { cabinPricesByAGroup =>
        cabinPricesByAGroup.groupBy { x => x.cabinCode }.map {
          case (_, cabinPricesForACabinCode) =>
            cabinPricesForACabinCode.reduceLeft(minCabin)
        }
      }

    baz.flatten.map { cabinPrice =>
      BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rateCodeToGroup(cabinPrice.rateCode))
    }

  }
}
