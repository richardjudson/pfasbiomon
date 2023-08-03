#--------------------------------------------------------------------------------------
#'
#' Extract the half-life information from Dawson et al. supplemental file
#' @param species The species to use
#' @param route The exposre route
#'
#--------------------------------------------------------------------------------------
pfasHalfLife <- function(species="Human",route="Oral") {
  printCurrentFunction()

  if(!exists("HL")) {
    cat("read in the Dawson data\n")
    file = "data/Dawson halflife/S3_Dawsonetal_PFAS_HL_101122.xlsx"
    hl = read.xlsx(file)
    hl = hl[is.element(hl$Species,species),]
    hl = hl[is.element(hl$DosingAdj,route),]
    HL <<- hl
  }
  hl = HL
  file = "data/PFAS chemicals.xlsx"
  chems = read.xlsx(file)

  hl = hl[is.element(hl$DTXSID,chems$dtxsid),]

  chems$female = NA
  chems$male = NA

  classlabels = c("<0.5 day","< 1 week","<2 months",">2 months")
  for(i in 1:nrow(chems)) {
    dtxsid = chems[i,"dtxsid"]
    temp = hl[is.element(hl$DTXSID,dtxsid),]
    if(nrow(temp)>2) {
      cat("too many rows\n")
      browser()
    }
    cat(chems[i,"nickname"],temp[1,"sex"],temp[1,"ClassPredFull"],temp[2,"sex"],temp[2,"ClassPredFull"],"\n")
    chems[i,"male"] =   classlabels[temp[temp$Sex=="Male","ClassPredFull"]]
    chems[i,"female"] = classlabels[temp[temp$Sex=="Female","ClassPredFull"]]
  }

  file = paste0("data/PFAS halflife ",species," ",route,".xlsx")
  write.xlsx(chems,file)
}
