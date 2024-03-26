#--------------------------------------------------------------------------------------
#'
#' Compare the in vitro PODs vs the Rat POD concentration
#' @param to.file If TRUE, write graphs to a file
#' @param data.version Label of folder where input data sits
#'
#--------------------------------------------------------------------------------------
pfasInVitroVsRat <- function(to.file=F) {
  printCurrentFunction()
  dir = paste0("data/")
  if(to.file) {
    fname <- paste0(dir,"figures/pfasInVitroVsRat.pdf")
    pdf(file=fname,width=7,height=10,pointsize=12,bg="white",paper="letter",pagecentre=T)
  }
  par(mfrow=c(2,1),mar=c(4,10,4,3))

  file = paste0(dir,"PFAS partition data Rat.xlsx")
  part = read.xlsx(file)
  rownames(part) = part$dtxsid
  file = paste0(dir,"pfasInVitroPODs.xlsx")
  ivpod = read.xlsx(file)
  file = paste0(dir,"ntp 28 day tox summary.xlsx")
  ntp = read.xlsx(file)
  ntp = ntp[order(ntp$name),]
  ntp = ntp[!is.element(ntp$dtxsid,"DTXSID3037709"),]
  file = paste0(dir,"pfas_catalog 2021-10-06.xlsx")
  catalog = read.xlsx(file)
  catalog = catalog[is.element(catalog$dtxsid,ntp$dtxsid),]
  rownames(catalog) = catalog$dtxsid
  dlist = unique(ntp$dtxsid)
  cmw = catalog[,c("mw","name")]

  #################################################################################
  # compare rat POD concs to in vitro
  #################################################################################
  par(mfrow=c(2,1),mar=c(4,5,4,3))
  nchem = length(dlist)
  plot(c(1,1),type="n",xlim=c(1,1e5),log="x",xlab="ng/mL",ylim=c(0,nchem),
       ylab="",
       yaxt="n",
       xaxt="n",
       main="In Vitro vs In Vivo PODs")
  axis(side=1,at=c(1,10,100,1000,10000,100000))

  for(conc in c(10,100,1000,10000,100000)) lines(c(conc,conc),c(0,1000),col="gray")
  #cnames =c("PFBS","PFHxA","PFOA","PFDA","PFOS","PFNA","PFHxSK")
  for(j in 1:nchem) {
    dtxsid = dlist[j]
    #dtxsid = unique(ntp[is.element(ntp$name,name),"dtxsid"])
    name = unique(ntp[is.element(ntp$dtxsid,dtxsid),"name"])
    text(10,j-0.5,name,pos=2)
    lines(c(1e-10,1e10),c(j,j),col="gray")
    vals = ntp[is.element(ntp$dtxsid,dtxsid),c("sex","conc.at.lowest.POD.ng/mL")]
    for(k in 1:nrow(vals)) {
      pod = vals[k,2]
      shift = part[dtxsid,"Kmax"]*part[dtxsid,"fub"]
      cat(name,shift,"\n")
      #pod = pod*shift
      cat(name,shift,"\n")
      sex = vals[k,1]
      col = "black"
      if(sex=="female") col="red"
      lines(c(pod,pod),c(j-1,j),lty=1,lwd=2,col=col)
    }
    temp = ivpod[is.element(ivpod$dtxsid,dtxsid),]
    for(i in 1:nrow(temp)) {
      name1 = temp[i,"source"]
      pod = temp[i,"pod"]
      col = "gray"
      pch = 21
      if(length(grep("ACEA",name1))>0) col = "violet"
      if(length(grep("ATG",name1))>0) col = "blue"
      if(length(grep("BSK",name1))>0) col = "red"
      if(length(grep("HTPP",name1))>0) col = "orange"
      if(length(grep("HTTr",name1))>0) col = "cyan"
      if(length(grep("Zeb",name1))>0) col = "green"
      if(length(grep("MEA",name1))>0) {col = "black"}
      if(length(grep("Thyr",name1))>0) {col = "gray"}

      if(pod<1000) {
        value = pod*cmw[dtxsid,"mw"]
        #value = value*part[dtxsid,"invitro.water.fraction"]*part[dtxsid,"invitro.vwell"]/part[dtxsid,"invitro.vwater"]
        yval = j-0.5 + rnorm(1,0,0.1)
        points(value,yval,pch=pch,bg=col)
      }
    }
  }
  if(!to.file) browser()
  else dev.off()
}

