#--------------------------------------------------------------------------------------
#'
#' Find outliers
#'
#--------------------------------------------------------------------------------------
pfasBiomonitoringOutliers <- function() {
  printCurrentFunction()
  dir = paste0("../data/")

  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  mat = read.xlsx(file)
  mat = mat[mat$value>0,]
  mlist = sort(unique(mat$metric))
  dlist = unique(mat$dtxsid)
  nlist = c("dtxsid","name","metric","n","min","max","range","srcmin","srcmax")
  row = as.data.frame(matrix(nrow=1,ncol=length(nlist)))
  names(row) = nlist
  res = NULL
  for(dtxsid in dlist) {
    temp1 = mat[is.element(mat$dtxsid,dtxsid),]
    for(metric in mlist) {
      temp2 = temp1[is.element(temp1$metric,metric),]
      if(nrow(temp2)>=2) {
        x1 = min(temp2$value)
        x2 = max(temp2$value)
        s1 = temp2[temp2$value==x1,"snaid"][1]
        s2 = temp2[temp2$value==x2,"snaid"][1]


        row[1,"dtxsid"] = dtxsid
        row[1,"name"] = temp2[1,"name"]
        row[1,"metric"] = metric
        row[1,"n"] = nrow(temp2)
        row[1,"min"] = x1
        row[1,"max"] = x2
        row[1,"srcmin"] = s1
        row[1,"srcmax"] = s2
        row[1,"range"] = x2/x1
        res = rbind(res,row)
      }
    }
  }
  file = paste0(dir,"PFAS biomonitoring data outliers.xlsx")
  write.xlsx(res,file)
}
