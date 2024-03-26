#--------------------------------------------------------------------------------------
#'
#' Pull the PFAS data fro MDH
#'
#--------------------------------------------------------------------------------------
pfas.for.mdh <- function(to.file=F) {
  printCurrentFunction()

  file = "../MDH/PFAS wide hts.xlsx"
  mat = read.xlsx(file)
  file = "../MDH/20230303_MDH_PFASlist.xlsx"
  chems = read.xlsx(file)
  mat = mat[is.element(mat$dtxsid,chems$DTXSID),]
  file = "../MDH/PFAS HTS for MDH.xlsx"
  write.xlsx(mat,file)


}
