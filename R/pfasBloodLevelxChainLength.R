#--------------------------------------------------------------------------------------
#'
#' Plotthe blood levels by chain length
#' @param to.file If TRUE, write graphs to a file
#' @param data.version Label of folder where input data sits
#'
#--------------------------------------------------------------------------------------
pfasBloodLevelxChainLength <- function(to.file=F) {
  printCurrentFunction()
  dir = paste0("data/")
  if(to.file) {
    fname <- paste0(dir,"figures/pfasBloodLevelxChainLength.pdf")
    pdf(file=fname,width=7,height=10,pointsize=12,bg="white",paper="letter",pagecentre=T)
  }

  par(mfrow=c(2,1),mar=c(4,10,4,3))
  file = paste0(dir,"PFAS synonyms.xlsx")
  synonyms = read.xlsx(file)
  rownames(synonyms) = synonyms$dtxsid

  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  mat = read.xlsx(file)
  #mat = mat[!is.element(mat$metric,c("concentration")),]
  mat[is.element(mat$metric,"LOQ"),"metric"] = "  LOQ"
  mat[is.element(mat$metric,"LOD"),"metric"] = "  LOD"
  mat[is.element(mat$metric,"minimum"),"metric"] = " minimum"
  mat = mat[mat$value>0,]
  dlist = unique(mat$dtxsid)

  file = paste0(dir,"pfas_catalog 2021-10-06.xlsx")
  catalog = read.xlsx(file)
  rownames(catalog) = catalog$dtxsid
  dlist2 = catalog[!is.na(catalog$ACEA),"dtxsid"]
  dlist = dlist[is.element(dlist,dlist2)]
  cmw = catalog[,c("mw","name")]

  #################################################################################
  # plot blood levels vs. chain length
  #################################################################################
  par(mfrow=c(2,1),mar=c(4,5,4,3))

  plot(c(1,1),type="n",xlab="Chain Length",xlim=c(2,13),ylim=c(1e-3,1e3),log="y",
       ylab="Blood Levels (ng/mL)",cex.lab=1.2,cex.axis=1.2)
  for(y in c(0.01,0.1,1,10,100)) lines(c(0,100),c(y,y),col="gray")
  dlist0 = unique(mat$dtxsid)
  tlist = c("75th percentile","50th percentile","mean","median","95th percentile",
            "90th percentile","99th percentile","98th percentile")
  for(dtxsid in dlist0) {
    temp = mat[is.element(mat$dtxsid,dtxsid),]
    temp = temp[is.element(temp$metric,tlist),]
    name = temp[1,"name"]
    vals = temp$value
    if(length(vals)>0) {
      #cat(name,max(vals),"\n")
      cl = 1
      category = "unknown"
      mw = 1
      if(is.element(dtxsid,catalog$dtxsid)) {
        cl = catalog[dtxsid,"chain_length"]
        category = catalog[dtxsid,"category"]
        mw = catalog[dtxsid,"mw"]
      }
      #vals = vals/mw
      col = "white"
      if(category=="PFAAs") col = "red"
      else if(category=="PFAA precursors") col = "yellow"
      else if(category=="FASA based PFAA precursors") col = "cyan"
      else if(category=="Fluorotelomer PFAA precursors") col = "black"
      #else  cat(dtxsid,category,"\n")
      q = quantile(vals,probs=seq(0,1,0.125))
      x = cl+rnorm(1,0,0.1)
      lines(c(x,x),c(q[2],q[8]),lwd=2)
      w=0.1
      rect(x-w,q[3],x+w,q[7],col=col)
      lowvals = vals[vals<q[2]]
      hivals = vals[vals>q[8]]
      yy = c(hivals,lowvals)
      if(length(yy)>0) for(k in 1:length(yy)) points(x,yy[k],pch=21,cex=0.5)
    }
  }
  if(!to.file) browser()
  if(to.file) dev.off()
}

