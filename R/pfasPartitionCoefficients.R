library(httk)
#--------------------------------------------------------------------------------------
#'
#' Get the partition coefficients
#' @param to.file If TRUE, write graphs to a file
#' @param data.version Label of folder where input data sits
#'
#--------------------------------------------------------------------------------------
pfasPartitionCoefficients <- function(species="Human") {
  printCurrentFunction()
  # load_sipes2017()
  # load_dawson2021()
  # load_pradeep2020()

  file = "../data/PFAS synonyms.xlsx"
  synonyms = read.xlsx(file)
  rownames(synonyms) = synonyms$dtxsid

  file = "../data/PFAS biomonitoring data final.xlsx"
  print(file)
  chems = read.xlsx(file)
  chems = unique(chems[,c("dtxsid","name","casrn")])
  cat(nrow(chems),"\n")
  chems0 = get_cheminfo(info="CAS")
  chems = chems[is.element(chems$casrn,chems0),]
  cat(nrow(chems),"\n")
  chems$nickname = NA
  chems$invitro.cell.fraction = NA
  chems$invitro.water.fraction = NA
  chems$invitro.vwell = NA
  chems$invitro.vcell = NA
  chems$invitro.vwater = NA
  chems$fub = NA
  chems$blood2plasma = NA
  chems$Kmax = NA
  chems$Kadipose2pu = NA
  chems$Kbone2pu = NA
  chems$Kbrain2pu = NA
  chems$Kgut2pu = NA
  chems$Kheart2pu = NA
  chems$Kkidney2pu = NA
  chems$Kliver2pu = NA
  chems$Klung2pu = NA
  chems$Kmuscle2pu = NA
  chems$Kskin2pu = NA
  chems$Kspleen2pu = NA
  chems$Krbc2pu = NA
  chems$Krest2pu = NA
   for(i in 1:nrow(chems)) {
    dtxsid = chems[i,"dtxsid"]
    casrn = chems[i,"casrn"]
    name = chems[i,"name"]
    print(name)
    if(is.element(dtxsid,synonyms$dtxsid)) chems[i,"nickname"] = synonyms[dtxsid,"nickname"]
    tryCatch({
      params = parameterize_schmitt(dtxsid=dtxsid,species=species,force.human.fup=T,default.to.human=T)
      chems[i,"fub"] = params$Funbound.plasma

      spart = predict_partitioning_schmitt(dtxsid=dtxsid,species=species,default.to.human=T)
      arm = armitage_eval(casrn.vector=casrn,this.FBSf=0.01)
      chems[i,"invitro.cell.fraction"] = arm$xcells
      chems[i,"invitro.water.fraction"] = arm$xwat_s

      chems[i,"invitro.vwell"] = arm$Vwell
      chems[i,"invitro.vcell"] = arm$Vcells
      chems[i,"invitro.vwater"] = arm$Vm

      nlist = names(spart)
      for(coef in nlist) chems[i,coef] = spart[[coef]]
      x = chems[i,12:ncol(chems)]
      x = x[!is.na(x)]
      if(length(x)>0) chems[i,"Kmax"] = max(x,na.rm=T)

      b2p = calc_rblood2plasma(dtxsid=dtxsid,
                               parameters=NULL,hematocrit=NULL,Krbc2pu=NULL,Funbound.plasma=NULL,
                               default.to.human=T,species=species,adjusted.Funbound.plasma = TRUE,
                               suppress.messages = TRUE)
      chems[i,"blood2plasma"] = b2p
    }, error=function(e) {
      print(e)
    })

   }
  file = paste0("../data/PFAS partition data ",species,".xlsx")
  write.xlsx(chems,file)
}
