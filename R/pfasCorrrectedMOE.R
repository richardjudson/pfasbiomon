#--------------------------------------------------------------------------------------
#'
#' Calculate the TK-corrected MOE values
#'
#--------------------------------------------------------------------------------------
pfasCorrrectedMOE <- function() {
  printCurrentFunction()

  dir = "../data/"
  file = paste0(dir,"PFAS partition data Human.xlsx")
  part = read.xlsx(file)
  rownames(part) = part$dtxsid
  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  mat = read.xlsx(file)
  mat = mat[is.element(mat$metric,c("05th percentile","25th percentile",
                                    "50th percentile","75th percentile",
                                    "95th percentile","98th percentile","99th percentile",
                                    "mean","median","minimum","maximum")),]
  file = paste0(dir,"pfasInVitroPODs.xlsx")
  ivpod = read.xlsx(file)

  file = paste0(dir,"pfas_catalog 2021-10-06.xlsx")
  catalog = read.xlsx(file)
  rownames(catalog) = catalog$dtxsid

  dlist = unique(part$dtxsid)
  print(length(dlist))
  dlist = dlist[is.element(dlist,mat$dtxsid)]
  print(length(dlist))
  dlist = dlist[is.element(dlist,ivpod$dtxsid)]
  print(length(dlist))
  #dlist = dlist[is.element(dlist,catalog$dtxsid)]
  cat("Number of chemicals with all data:",length(dlist),"\n")

  res = mat[is.element(mat$dtxsid,dlist),]
  res = subset(res,select= -c(value_original,units_original,acname,desc))
  res$invitro.water.fraction = NA
  res$invitro.cell.fraction = NA
  res$invitro.vwater = NA
  res$invitro.vcell = NA
  res$invitro.vwell = NA
  res$Kmax = NA
  res$mw = NA
  res$fub = NA
  res$blood2plasma = NA
  res$tissue2plasma = NA
  res$pod_min_uM = NA
  res$pod_min_source = NA
  res$pod_min_ngmL = NA
  res$plasma_conc_ngmL = NA
  res$tissue_pod_ngmL = NA
  res$tissue_conc_ngmL = NA
  res$conc_ratio = NA
  res$ac50_ratio = NA
  res$moe_ratio = NA
  res$moe = NA
  res$raw_moe = NA

  for(i in 1:nrow(res)) {
    dtxsid = res[i,"dtxsid"]
    res[i,"invitro.cell.fraction"] = part[dtxsid,"invitro.cell.fraction"]
    res[i,"invitro.water.fraction"] = part[dtxsid,"invitro.water.fraction"]
    res[i,"invitro.vcell"] = part[dtxsid,"invitro.vcell"]
    res[i,"invitro.vwater"] = part[dtxsid,"invitro.vwater"]
    res[i,"invitro.vwell"] = part[dtxsid,"invitro.vwell"]
    res[i,"Kmax"] = part[dtxsid,"Kmax"]
    res[i,"fub"] = part[dtxsid,"fub"]
    res[i,"blood2plasma"] = part[dtxsid,"blood2plasma"]

    if(is.element(res[i,"matrix"],c("serum","cord plasma","plasma","cord serum"))) res[i,"plasma_conc_ngmL"] = res[i,"value"]
    else res[i,"plasma_conc_ngmL"] = res[i,"value"] / res[i,"blood2plasma"]

    temp = ivpod[is.element(ivpod$dtxsid,dtxsid),]
    mw = catalog[dtxsid,"mw"]
    res[i,"mw"] = mw
    temp = temp[order(temp$pod,decreasing=F),]
    res[i,"pod_min_uM"] = temp[1,"pod"]
    res[i,"pod_min_source"] = temp[1,"source"]

    res[i,"pod_min_ngmL"] = min(temp$pod) * mw
    res[i,"tissue_conc_ngmL"] = res[i,"plasma_conc_ngmL"] * res[i,"Kmax"] * res[i,"fub"]
    # res[i,"tissue_pod_ngmL"] =  res[i,"pod_min_ngmL"] *
    #                             res[i,"invitro.cell.fraction"] *
    #                             res[i,"invitro.vwell"] /
    #                             res[i,"invitro.vcell"]

    res[i,"tissue_pod_ngmL"] =  res[i,"pod_min_ngmL"] *
      res[i,"invitro.water.fraction"] *
      res[i,"invitro.vwell"] /
      res[i,"invitro.vwater"]

    res[i,"tissue2plasma"] = res[i,"tissue_conc_ngmL"]/res[i,"plasma_conc_ngmL"]
    res[i,"moe"] = res[i,"tissue_pod_ngmL"] / res[i,"tissue_conc_ngmL"]
    res[i,"raw_moe"] = res[i,"pod_min_ngmL"] / res[i,"value"]

    res[i,"conc_ratio"] = res[i,"tissue_conc_ngmL"]/res[i,"plasma_conc_ngmL"]
    res[i,"ac50_ratio"] = res[i,"tissue_pod_ngmL"]/res[i,"pod_min_ngmL"]
    res[i,"moe_ratio"] =  res[i,"ac50_ratio"]/res[i,"conc_ratio"]
  }

  hist(log10(res$moe))
  file = "../data/PFAS corrected moe.xlsx"
  write.xlsx(res,file)

  nlist = c("dtxsid","casrn","name","nickname","metric","source","location","population",
            "pod_min_ngmL","pod_min_uM","pod_min_source","moe")

  row = as.data.frame(matrix(nrow=1,ncol=length(nlist)))
  names(row) = nlist
  minpod = NULL
  dlist = unique(res$dtxsid)
  for(dtxsid in dlist) {
    temp = unique(res[is.element(res$dtxsid,dtxsid),nlist])
    temp = temp[order(temp$moe),]
    minpod = rbind(minpod,temp[1,])
  }
  file = "../data/PFAS min moe x assay.xlsx"
  write.xlsx(minpod,file)

  nlist = c("dtxsid","casrn","name","nickname","conc_ratio","ac50_ratio")
  temp = unique(res[,nlist])
  file = "../data/PFAS PK conc ratios.xlsx"
  write.xlsx(temp,file)
}
