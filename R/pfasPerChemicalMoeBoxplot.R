#--------------------------------------------------------------------------------------
#'
#' Build the raw MoE Boxplots
#' @param to.file If TRUE, write graphs to a file
#'
#--------------------------------------------------------------------------------------
pfasPerChemicalMoeBoxplot <- function(to.file=F) {
  printCurrentFunction()
  dir = paste0("data/")
  # file = paste0(dir,"PFAS synonyms.xlsx")
  # synonyms = read.xlsx(file)
  # rownames(synonyms) = synonyms$dtxsid
  file = paste0(dir,"PFAS corrected moe.xlsx")
  mat = read.xlsx(file)
  mat[is.element(mat$metric,"LOQ"),"metric"] = "  LOQ"
  mat[is.element(mat$metric,"LOD"),"metric"] = "  LOD"
  mat[is.element(mat$metric,"minimum"),"metric"] = " minimum"
  mat[is.element(mat$metric_class,"LOQ"),"metric_class"] = "  LOQ"
  mat[is.element(mat$metric_class,"LOD"),"metric_class"] = "  LOD"
  mat[is.element(mat$metric_class,"Low"),"metric_class"] = " Low"
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
  # Human blood levels
  #################################################################################
  par(mfrow=c(2,1),mar=c(4,10,4,3))
  minvals = as.data.frame(matrix(nrow=length(dlist),ncol=4))
  names(minvals) = c("dtxsid","minpod","name","nickname")
  for(i in 1:length(dlist)) {
    dtxsid = dlist[i]
    minvals[i,"dtxsid"] = dtxsid
    temp = mat[is.element(mat$dtxsid,dtxsid),]
    name = temp[1,"name"]
    nickname = temp[1,"nickname"]
    minvals[i,"name"] = nickname
    # nickname = name
    # if(is.element(dtxsid,synonyms$dtxsid)) nickname = synonyms[dtxsid,"nickname"]
    cat(nickname,nrow(temp),"\n")
    minvals[i,"nickname"] = nickname
    if(nrow(temp)>0) {
      x = temp$metric
      y = temp$value
      if(length(unique(x))==1) {
        x = c(x,"")
        y = c(y,1000000)
      }
      p = ggplot(data=temp,aes(x=metric_class,y=plasma_conc_ngmL))  +
        ggtitle(nickname) +
        geom_boxplot(outlier.colour="black",
                     outlier.shape=21,outlier.size=2,outlier.fill="white",
                     notch=FALSE) +
        scale_y_continuous(trans="log10",limits=c(0.01,10000)) +
        #scale_fill_manual(values=c("white","red")) +
        coord_flip() +
        theme_bw() +
        ylab("ng/mL") +
        xlab("") +
        labs(color = "Exposure Status") +
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"),
              plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
              strip.text.x = element_text(size = 10),
              plot.margin = margin(t=20,r=20,b=50,l=20),
              legend.text = element_text(size=12),
              legend.title = element_text(size=12)) +
        geom_jitter(aes(color=exposed),size=0.5,alpha = 0.9)

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
          if(length(grep("Zeb",name1))>0) col = "green"
          if(length(grep("MEA",name1))>0) col = "black"
          if(length(grep("Thyr",name1))>0) col = "gray"

          if(!is.na(pod)) {
            if(pod<1000) {
              value = pod*cmw[dtxsid,"mw"]
              p = p + geom_hline(yintercept=value,colour=col,size=0.5)
              #cat(value,col,"\n")
            }
          }
        }
      }

      print(p)

      if(to.file) {
        fname <- paste0(dir,"figures/per chemical moe plots/pfasPerChemicalMoeBoxplot ",nickname,".pdf")
        fname = str_replace_all(fname,":","_")
        ggsave(plot = p, width = 8, height = 4, dpi = 300, filename =fname)
        dev.off()
      }
      else browser()
    }
  }
  # cat("build the raw MoE table\n")
  # metric.list = unique(mat$metric)
  # metric.list = metric.list[!is.element(metric.list,c("  LOD","  LOQ"))]
  # minvals = minvals[!is.na(minvals$minpod),]
  # rownames(minvals) = minvals$dtxsid
  # metrics = NULL
  # moes = NULL
  # name.list = c("study","dtxsid","casrn","name","nickname","population","location","source","snaid","matrix","metric","value","units","minpod","moe")
  # row = as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  # names(row) = name.list
  # res = NULL
  # dlist = minvals$dtxsid
  # for(i in 1:length(dlist)) {
  #   dtxsid = dlist[i]
  #   minval = minvals[dtxsid,"minpod"]
  #   temp = mat[is.element(mat$dtxsid,dtxsid),]
  #   temp = temp[temp$value>0,]
  #   temp = temp[is.element(temp$metric,metric.list),]
  #   if(nrow(temp)>0) {
  #     for(j in 1:nrow(temp)) {
  #       x = temp[j,"metric"]
  #       y = temp[j,"value"]
  #       moe = minval/y
  #       metrics = c(metrics,x)
  #       moes = c(moes,moe)
  #
  #       row[1,"study"] = temp[j,"assay_name"]
  #       row[1,"dtxsid"] = temp[j,"dtxsid"]
  #       row[1,"casrn"] = temp[j,"casrn"]
  #       row[1,"name"] = temp[j,"name"]
  #       row[1,"nickname"] = temp[j,"nickname"]
  #       #if(is.element(dtxsid,synonyms$dtxsid)) row[1,"nickname"] = synonyms[is.element(synonyms$dtxsid,dtxsid),"nickname"]
  #       row[1,"population"] = temp[j,"population"]
  #       row[1,"location"] = temp[j,"location"]
  #       row[1,"source"] = temp[j,"source"]
  #       row[1,"snaid"] = temp[j,"snaid"]
  #       row[1,"metric"] = temp[j,"metric"]
  #       row[1,"matrix"] = temp[j,"matrix"]
  #       row[1,"value"] = temp[j,"value"]
  #       row[1,"units"] = temp[j,"units"]
  #       row[1,"minpod"] = minval
  #       row[1,"moe"] = moe
  #       res = rbind(res,row)
  #     }
  #   }
  # }
  # file = paste0(dir,"PFAS biomonitoring raw moe.xlsx")
  # res = unique(res)
  # write.xlsx(res,file)
}

