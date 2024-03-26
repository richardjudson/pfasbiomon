#--------------------------------------------------------------------------------------
#'
#' Analyze the PFAS Biomonitoring Data
#' @param to.file If TRUE, write graphs to a file
#' @param data.version Label of folder where input data sits
#'
#--------------------------------------------------------------------------------------
pfasBiomonitoringAnalysis <- function(to.file=F) {
  printCurrentFunction()
  dir = paste0("../data/")
  if(to.file) {
    fname <- paste0(dir,"pfasBiomonitoringAnalysis.pdf")
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

  file = paste0(dir,"ntp 28 day tox summary.xlsx")
  ntp = read.xlsx(file)

  file = paste0(dir,"pfas_catalog 2021-10-06.xlsx")
  catalog = read.xlsx(file)
  rownames(catalog) = catalog$dtxsid
  dlist2 = catalog[!is.na(catalog$ACEA),"dtxsid"]
  dlist = dlist[is.element(dlist,dlist2)]
  cmw = catalog[,c("mw","name")]

  file = paste0(dir,"pfasInVitroPODs.xlsx")
  ivpod = read.xlsx(file)

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
      col = "white"
      if(category=="PFAAs") col = "red"
      else if(category=="PFAA precursors") col = "yellow"
      else if(category=="FASA based PFAA precursors") col = "cyan"
      else if(category=="Fluorotelomer PFAA precursors") col = "black"
      #else  cat(dtxsid,category,"\n")
      q = quantile(vals,probs=seq(0,1,0.125))
      x = cl+rnorm(1,0,0.2)
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
  #################################################################################
  # plot blood levels vs. chain length by molar units
  #################################################################################

  plot(c(1,1),type="n",xlab="Chain Length",xlim=c(2,13),ylim=c(1e-6,1),log="y",
       ylab="Blood Levels (uM)",cex.lab=1.2,cex.axis=1.2)
  for(y in c(1e-5,1e-4,1e-3,0.01,0.1,1,10,100)) lines(c(0,100),c(y,y),col="gray")
  dlist0 = unique(mat$dtxsid)
  tlist = c("75th percentile","50th percentile","mean","median","95th percentile",
            "90th percentile","99th percentile","98th percentile")
  for(dtxsid in dlist0) {
    temp = mat[is.element(mat$dtxsid,dtxsid),]
    temp = temp[is.element(temp$metric,tlist),]
    vals = temp$value
    if(length(vals)>0) {
      cl = 1
      category = "unknown"
      mw = 1
      if(is.element(dtxsid,catalog$dtxsid)) {
        cl = catalog[dtxsid,"chain_length"]
        category = catalog[dtxsid,"category"]
        mw = catalog[dtxsid,"mw"]
      }
      if(mw>1) {
        vals = vals / mw
        col = "white"
        if(category=="PFAAs") col = "red"
        else if(category=="PFAA precursors") col = "yellow"
        else if(category=="FASA based PFAA precursors") col = "cyan"
        else if(category=="Fluorotelomer PFAA precursors") col = "black"
        else  cat(dtxsid,category,"\n")
        q = quantile(vals,probs=seq(0,1,0.125))
        x = cl+rnorm(1,0,0.2)
        lines(c(x,x),c(q[2],q[8]),lwd=2)
        w=0.1
        rect(x-w,q[3],x+w,q[7],col=col)
      }
    }
  }
  #if(!to.file) browser()
  #################################################################################
  # compare rat POD concs to in vitro
  #################################################################################
  par(mfrow=c(2,1),mar=c(4,5,4,3))
  nchem = length(unique(ntp$dtxsid))
  plot(c(1,1),type="n",xlim=c(1e-1,1e5),log="x",xlab="ng/mL",ylim=c(0,nchem),
       ylab="",
       yaxt="n",
       xaxt="n",
       main="In Vitro vs In Vivo PODs")
  axis(side=1,at=c(1,10,100,1000,10000,100000))

  for(conc in c(1,10,100,1000,10000,100000)) lines(c(conc,conc),c(0,1000),col="gray")
  cnames =c("PFBS","PFHxA","PFOA","PFDA","PFOS","PFNA","PFHxSK")
  for(j in 1:nchem) {
    name = cnames[j]
    dtxsid = unique(ntp[is.element(ntp$name,name),"dtxsid"])
    #name = ntp[j,"name"]
    text(1,j-0.5,name,pos=2)
    lines(c(1e-10,1e10),c(j,j),col="gray")
    vals = ntp[is.element(ntp$dtxsid,dtxsid),c("sex","conc.at.lowest.POD.ng/mL")]
    for(k in 1:nrow(vals)) {
      pod = vals[k,2]
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
      if(length(grep("Zeb",name1))>0) col = "yellow"
      if(length(grep("MEA",name1))>0) {col = "black";pch=25}
      if(length(grep("Thyr",name1))>0) {col = "gray";pch=25}

      if(pod<1000) {
        value = pod*cmw[dtxsid,"mw"]
        yval = j-0.5 + rnorm(1,0,0.1)
        points(value,yval,pch=pch,bg=col)
      }
    }
  }
  if(!to.file) browser()

  #################################################################################
  # plot the distributions for each chemical and metric
  #################################################################################
  # par(mfrow=c(4,3),mar=c(4,5,4,5))
  # metric = "95th percentile"
  # dlist = unique(mat$dtxsid)
  # breaks = seq(from=-5,to=5,by=0.2)
  # for(dtxsid in dlist) {
  #   temp = mat[is.element(mat$dtxsid,dtxsid),]
  #   temp = temp[is.element(temp$metric,metric),]
  #   nstudy = length(unique(temp$snaid))
  #   if(nstudy>4) {
  #     name = temp[1,"name"]
  #     nickname = NA
  #     if(is.element(dtxsid,synonyms$dtxsid)) {
  #       nickname = synonyms[dtxsid,"nickname"]
  #     }
  #     if(is.na(nickname)) nickname = ""
  #     vals = log10(temp$value)
  #     cat(nickname,mean(vals),sd(vals),"\n")
  #     hist(vals,breaks=breaks,cex.axis=1.2,cex.lab=1.2,
  #          main=paste0(name,"\n",nickname," (",nstudy,")"),xlab="log(ng/ml)")
  #     #if(!to.file) browser()
  #   }
  # }

  #################################################################################
  # Human blood levels
  #################################################################################
  par(mfrow=c(2,1),mar=c(4,10,4,3))
  minvals = as.data.frame(matrix(nrow=length(dlist),ncol=4))
  names(minvals) = c("dtxsid","minpod","name","nickname")
  for(i in 1:length(dlist)) {
    dtxsid = dlist[i]
    minvals[i,"dtxsid"] = dtxsid
    minvals[i,"name"] = synonyms[dtxsid,"name"]
    minvals[i,"nickname"] = synonyms[dtxsid,"nickname"]

    temp = mat[is.element(mat$dtxsid,dtxsid),]
    name = temp[1,"name"]
    nickname = NA
    if(is.element(dtxsid,synonyms$dtxsid)) {
      nickname = synonyms[dtxsid,"nickname"]
    }
    if(is.na(nickname)) nickname = ""
    if(nrow(temp)>0) {
      x = temp$metric
      y = temp$value
      if(length(unique(x))==1) {
        x = c(x,"")
        y = c(y,1000000)
      }
      boxplot(y~x,main=paste(name,"\n",nickname),
              xlab="ng/mL",ylab="",log="x",xaxt="n",
              horizontal=T,las=1,par(cex.lab=1,cex.axis=1.0),ylim=c(0.001,10000))
      axis(side=1,at=c(0.001,0.01,0.1,1,10,100,1000,10000))

      if(is.element(dtxsid,ivpod$dtxsid)) {
        temp = ivpod[is.element(ivpod$dtxsid,dtxsid),]
        minvals[i,"minpod"] = min(temp$pod,na.rm=T)*cmw[dtxsid,"mw"]
        for(i in 1:nrow(temp)) {
          name1 = temp[i,"source"]
          pod = temp[i,"pod"]
          col = "gray"
          if(length(grep("ACEA",name1))>0) col = "violet"
          if(length(grep("ATG",name1))>0) col = "blue"
          if(length(grep("BSK",name1))>0) col = "red"
          if(length(grep("HTPP",name1))>0) col = "orange"
          if(length(grep("HTTr",name1))>0) col = "cyan"
          if(length(grep("Zeb",name1))>0) col = "yellow"
          if(length(grep("MEA",name1))>0) col = "black"
          if(length(grep("Thyr",name1))>0) col = "gray"

          if(!is.na(pod)) {
            if(pod<1000) {
              value = pod*cmw[dtxsid,"mw"]
              lines(c(value,value),c(0,1000),col=col)
            }
          }
        }
      }

      if(is.element(dtxsid,ntp$dtxsid)) {
        vals = ntp[is.element(ntp$dtxsid,dtxsid),c("sex","conc.at.lowest.POD.ng/mL")]
        for(k in 1:nrow(vals)) {
          pod = vals[k,2]
          sex = vals[k,1]
          col = "black"
          if(sex=="female") col="red"
          lines(c(pod,pod),c(0,1000),lty=2,lwd=2,col=col)
        }
      }
    }
    if(!to.file) browser()
  }

  metric.list = unique(mat$metric)
  metric.list = metric.list[!is.element(metric.list,c("  LOD","  LOQ"))]
  minvals = minvals[!is.na(minvals$minpod),]
  rownames(minvals) = minvals$dtxsid
  metrics = NULL
  moes = NULL
  name.list = c("study","dtxsid","casrn","name","population","location","source","snaid","matrix","metric","value","units","minpod","moe")
  row = as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) = name.list
  res = NULL
  dlist = minvals$dtxsid
  for(i in 1:length(dlist)) {
    dtxsid = dlist[i]
    minval = minvals[dtxsid,"minpod"]
    temp = mat[is.element(mat$dtxsid,dtxsid),]
    temp = temp[temp$value>0,]
    temp = temp[is.element(temp$metric,metric.list),]
    if(nrow(temp)>0) {
      for(j in 1:nrow(temp)) {
        x = temp[j,"metric"]
        y = temp[j,"value"]
        moe = minval/y
        metrics = c(metrics,x)
        moes = c(moes,moe)

        row[1,"study"] = temp[j,"assay_name"]
        row[1,"dtxsid"] = temp[j,"dtxsid"]
        row[1,"casrn"] = temp[j,"casrn"]
        row[1,"name"] = temp[j,"name"]
        row[1,"population"] = temp[j,"population"]
        row[1,"location"] = temp[j,"location"]
        row[1,"source"] = temp[j,"source"]
        row[1,"snaid"] = temp[j,"snaid"]
        row[1,"metric"] = temp[j,"metric"]
        row[1,"matrix"] = temp[j,"matrix"]
        row[1,"value"] = temp[j,"value"]
        row[1,"units"] = temp[j,"units"]
        row[1,"minpod"] = minval
        row[1,"moe"] = moe
        res = rbind(res,row)
      }
    }
  }
  res$nickname = NA
  for(dtxsid in unique(res$dtxsid)) {
    if(is.element(dtxsid,synonyms$dtxsid)) {
      res[is.element(res$dtxsid,dtxsid),"nickname"] = synonyms[is.element(synonyms$dtxsid,dtxsid),"nickname"]
    }
  }
  file = paste0(dir,"PFAS biomonitoring moe.xlsx")
  res = unique(res)
  write.xlsx(res,file)


  res = res[!is.element(res$metric,c(" LOD"," LOQ")),]
  minres = NULL
  # row = as.data.frame(matrix(nrow=1,ncol=ncol(res)))
  # names(row) = names(res)
  for(dtxsid in unique(res$dtxsid)) {
    temp = res[is.element(res$dtxsid,dtxsid),]
    temp = temp[order(temp$moe),]
    minres = rbind(minres,temp[1,])
  }
  file = paste0(dir,"PFAS biomonitoring minimum moe.xlsx")
  minres = unique(minres)
  write.xlsx(minres,file)
  if(!to.file) browser()
  #################################################################################
  # margins of exposure
  #################################################################################
  par(mfrow=c(2,1),mar=c(4,10,4,3))
  boxplot(moes~metrics,main=paste("BCBCR"),
          xlab="",ylab="",log="x",
          horizontal=T,las=1,par(cex.lab=1,cex.axis=1.0),ylim=c(1e-1,1e6))
  for(i in seq(-2,10)) {
    x = 10**i
    lines(c(x,x),c(0,100),col="gray")
  }
  lines(c(1,1),c(0,100),col="red",lwd=3)
  if(!to.file) browser()

  if(to.file) dev.off()
}

