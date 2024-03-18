package promotions

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers.{be, convertToAnyMustWrapper}
import promotions.Promotions.{Promotion, PromotionCombo, allCombinablePromotions, combinablePromotions}

class PromotionsSpec extends AnyWordSpec {

  "Promotions" when {
    "given promotions" should {
      "find the maximum number of combinable promotions in each." in {
        val p1 = Promotion("P1", Seq("P3")) // P1 is not combinable with P3
        val p2 = Promotion("P2", Seq("P4", "P5")) // P2 is not combinable with P4 and P5
        val p3 = Promotion("P3", Seq("P1")) // P3 is not combinable with P1
        val p4 = Promotion("P4", Seq("P2")) // P4 is not combinable with P2
        val p5 = Promotion("P5", Seq("P2")) // P5 is not combinable with P2
        val allPromotions = Seq(p1, p2, p3, p4, p5)

        val expected1 = Seq(
          PromotionCombo(Seq("P1", "P2")),
          PromotionCombo(Seq("P1", "P4", "P5")),
          PromotionCombo(Seq("P2", "P3")),
          PromotionCombo(Seq("P3", "P4", "P5"))
        )

        val actual1: Seq[PromotionCombo] = allCombinablePromotions(allPromotions)
        println( s"${actual1.toSet}")
        actual1.size must be (expected1.size)
        actual1.toSet must be (expected1.toSet)

      }

      "find all PromotionCombos for a given Promotion" in {
        val p1 = Promotion("P1", Seq("P3")) // P1 is not combinable with P3
        val p2 = Promotion("P2", Seq("P4", "P5")) // P2 is not combinable with P4 and P5
        val p3 = Promotion("P3", Seq("P1")) // P3 is not combinable with P1
        val p4 = Promotion("P4", Seq("P2")) // P4 is not combinable with P2
        val p5 = Promotion("P5", Seq("P2")) // P5 is not combinable with P2
        val allPromotions = Seq(p1, p2, p3, p4, p5)

        val expected2 = Seq(
          PromotionCombo(Seq("P1", "P2")),
          PromotionCombo(Seq("P1", "P4", "P5"))
        )

        val actual2 = combinablePromotions("P1", allPromotions)
        actual2.size must be (expected2.size)
        actual2.toSet must be (expected2.toSet)

        val expected3 = Seq(
          PromotionCombo(Seq("P2", "P3")),
          PromotionCombo(Seq("P3", "P4", "P5"))
        )

        val actual3 = combinablePromotions("P3", allPromotions)
        actual3.size must be (expected3.size)
        actual3.toSet must be (expected3.toSet)
      }
    }
  }

}
