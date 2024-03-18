package promotions

object Promotions {

  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionWithCombinables( code: String, combinables: Seq[String])

  case class PromotionCombo(promotionCodes: Seq[String])

  def removeSubsets(promotionCombos: Seq[PromotionCombo]): Seq[PromotionCombo] =
    promotionCombos.foldLeft(promotionCombos){ (a,b) =>
      a.diff(List(b)).map(_.promotionCodes.toSet)
        .find{ x =>
          b.promotionCodes.toSet.subsetOf(x)
        } match{
        case Some(_) =>
          a.diff(List(b))
        case _ =>
          a
      }
    }

  private def isCombinableWithEvery( promo: PromotionWithCombinables, runningList: Set[String] ): Boolean = {
    runningList.forall{ promo.combinables.contains }
  }

  private def alreadySeen( promotion: PromotionWithCombinables, seenCodes: Set[String] ): Boolean =
    seenCodes.contains( promotion.code )

  private type APromotionCombination = Set[String]

  private def searchAndAccumulate(allPromotions: Seq[PromotionWithCombinables],
                                  rootCode: String,
                                  promotionToSearch: PromotionWithCombinables,
                                  existingAllowedCombinationsForThisCode: Set[String],
                                  allowedCombinationsForAllCodes: Set[APromotionCombination],
                                  level: Int = 0
                                 ): Set[APromotionCombination] = {

    val tab: String = "LEVEL" + level + ": " + List.fill(level*2){"\t"}.mkString
    val isFirstPromotionToBeConsidered = existingAllowedCombinationsForThisCode.isEmpty
    val allChildrenOfPromotionToSearch: Seq[PromotionWithCombinables] = {
      promotionToSearch.combinables.map { aCode =>
        allPromotions.find{_.code == aCode}.get
      }
    }

    val isPromotionToSearchAlreadySeen: Boolean =
      alreadySeen(promotionToSearch, existingAllowedCombinationsForThisCode)

    val res: Set[APromotionCombination] = {
      if( isPromotionToSearchAlreadySeen ){
        val allAllowedCombinationsForThisCode = existingAllowedCombinationsForThisCode
        allowedCombinationsForAllCodes + allAllowedCombinationsForThisCode
      } else {

        if( isFirstPromotionToBeConsidered ) {
          val updatedAllowedCombinationsForThisCode: Set[String] =
            existingAllowedCombinationsForThisCode + promotionToSearch.code
          allChildrenOfPromotionToSearch.foldLeft(allowedCombinationsForAllCodes){ (everything, nextChildPromoToSearch) =>
            everything ++ searchAndAccumulate(allPromotions, rootCode, nextChildPromoToSearch, updatedAllowedCombinationsForThisCode, allowedCombinationsForAllCodes, level +1)
          }
        } else { // not the first promotion (or root of tree)
          // if( promotionToSearch.combinables.contains( rootCode) ) { // if this promo combinable with root
          if( isCombinableWithEvery( promotionToSearch, existingAllowedCombinationsForThisCode) ) { // if this promo combinable with root
            val newRunningList = existingAllowedCombinationsForThisCode + promotionToSearch.code//   add this to existing running list of combinable promos
            // continue searching children of thie promo. Or for all children minus the root... (one of children is root)
            val promoCorrespondingToRootCode: PromotionWithCombinables = allPromotions.find{_.code == rootCode}.get
            val childrenOfPromotionToSearch = allChildrenOfPromotionToSearch.diff( List(promoCorrespondingToRootCode) )
            childrenOfPromotionToSearch.foldLeft(allowedCombinationsForAllCodes) { (everything, nextChildPromoToSearch) =>
              everything ++ searchAndAccumulate(allPromotions, rootCode, nextChildPromoToSearch, newRunningList, allowedCombinationsForAllCodes, level+1) //    searchAndAccumulate on these children
            }
          } else { // (this promo is not combinable with root)
            //   discard this root by doing this:
            val res = allowedCombinationsForAllCodes + existingAllowedCombinationsForThisCode//      return the existing running list to the all list of lists
            res
          }
        }
      }
    }

    res

  }

  def allCombinablePromotions(allPromotionsWithNonCombinables: Seq[Promotion]): Seq[PromotionCombo] = {

    val allPromotions: Seq[PromotionWithCombinables] = {
      val allCodes = allPromotionsWithNonCombinables.map{_.code}
      allPromotionsWithNonCombinables.map { promo =>
        PromotionWithCombinables(promo.code, allCodes.diff(promo.notCombinableWith).diff(List(promo.code)))
      }
    }

    val withSubsets: Seq[PromotionCombo] = allPromotions.map { aPromotion =>
      searchAndAccumulate(allPromotions, aPromotion.code, aPromotion, Set.empty[String], Set.empty[Set[String]])
    }.flatMap { allCombinationSets =>
      allCombinationSets.map { aCombinationSet =>
        PromotionCombo(aCombinationSet.toSeq)
      }
    }

    val res = removeSubsets(withSubsets)

    res.map{ promotionCombo => promotionCombo.copy( promotionCodes = promotionCombo.promotionCodes.sorted ) }
  }

  // Implement Me.
  def combinablePromotions( promotionCode: String,
                            allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    allCombinablePromotions(allPromotions).filter{ aPromotionCombo => aPromotionCombo.promotionCodes.contains( promotionCode)}

}
