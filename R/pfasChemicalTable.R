#--------------------------------------------------------------------------------------
#'
#' Generate a table of the chemicals and the data we have for them
#'
#--------------------------------------------------------------------------------------
pfasChemicalTable <- function() {
  printCurrentFunction()
  dir = paste0("../data/")

  file = paste0(dir,"pfas_catalog 2021-10-06.xlsx")
  catalog = read.xlsx(file)
  rownames(catalog) = catalog$dtxsid

  file = paste0(dir,"PFAS synonyms.xlsx")
  synonyms = read.xlsx(file)
  rownames(synonyms) = synonyms$dtxsid

  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  biomon = read.xlsx(file)
  dlist = unique(biomon$dtxsid)

  file = paste0(dir,"pfasInVitroPODs.xlsx")
  ivpod = read.xlsx(file)
  #dlist = c(dlist,ivpod$dtxsid)

  file = paste0(dir,"PFAS partition data.xlsx")
  part = read.xlsx(file)

  nlist = c("dtxsid","casrn","name","abbrv","biomonitoring","in vitro","pk")
  res = as.data.frame(matrix(nrow=length(dlist),ncol=length(nlist)))
  names(res) = nlist
  for(i in 1:length(dlist)) {
    dtxsid = dlist[i]
    res[i,"dtxsid"] = dtxsid
    temp = biomon[is.element(biomon$dtxsid,dtxsid),]
    res[i,"name"] = temp[1,"name"]
    res[i,"casrn"] = temp[1,"casrn"]
    #res[i,"abbrv"] = temp[1,"nickname"]

    if(is.element(dtxsid,biomon$dtxsid)) res[i,"biomonitoring"] = 1
    if(is.element(dtxsid,ivpod$dtxsid)) res[i,"in vitro"] = 1
    if(is.element(dtxsid,part$dtxsid)) res[i,"pk"] = 1
    #res[i,"abbrv"] = res[i,"name"]
    if(is.element(dtxsid,synonyms$dtxsid)) res[i,"abbrv"] = synonyms[dtxsid,"nickname"]

  }
  browser()
  file = paste0(dir,"pfas chemicals.xlsx")
  write.xlsx(res,file)

}
