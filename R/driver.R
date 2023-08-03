#--------------------------------------------------------------------------------------
#'
#' Run all of the analyzes starting with the hand-edited data file
#'
#--------------------------------------------------------------------------------------
driver <- function() {
  printCurrentFunction()
  pfasBiomonitoringMerge()
  pfasPartitionCoefficients("Human")
  pfasPartitionCoefficients("Rat")
  pfasCorrrectedMOE()
  pfasPerChemicalMoeBoxplot(T)
  pfasMoeBoxplot(T)
  pfasChemicalTable()
  pfasBiomonitoringOutliers()
  pfasBloodLevelxChainLengthGG(T)
  pfasInVitroVsRat(T)
}
