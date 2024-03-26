library(ggrepel)
#--------------------------------------------------------------------------------------
#'
#' check the processing for the DNT PODs
#'
#--------------------------------------------------------------------------------------
dnt.check <- function(to.file=F) {
  printCurrentFunction()

  file = "data/pfasInVitroPODs.xlsx"
  new = read.xlsx(file)
  new = new[is.element(new$source,"DNT"),]
  new$logpod = log10(new$pod)

  file = "../DNT/Send Richard for DNT filtering Mar2023/output/PFAS_DNT_bioactivity_mean_ac50.xlsx"
  old = read.xlsx(file)

  old1 = old[,c("dsstox_substance_id","mean.ac50")]
  names(old1) = c("dtxsid","logpod")
  old1[is.na(old1$logpod),"logpod"] = 3
  new1 = new[,c("dtxsid","name","logpod")]
  rownames(old1) = old1$dtxsid
  rownames(new1) = new1$dtxsid
  dlist = unique(c(old1$dtxsid,new1$dtxsid))
  res = as.data.frame(matrix(nrow=length(dlist),ncol=4))
  names(res) = c("dtxsid","name","logpod.carstens","logpod.judson")


  for(i in 1:length(dlist)) {
    dtxsid = dlist[i]
    res[i,"dtxsid"] = dtxsid
    res[i,"logpod.carstens"] = old1[dtxsid,"logpod"]
    res[i,"logpod.judson"] = new1[dtxsid,"logpod"]
    res[i,"name"] = new1[dtxsid,"name"]
  }
  df = res[res$logpod.judson<3,]
  df = df[df$logpod.carstens==3,]

  df = res[abs(res$logpod.judson-res$logpod.carstens)>0.2,]


    file = "../DNT/data compare.xlsx"
  write.xlsx(res,file)
  p = ggplot(data=res,aes(x=logpod.carstens,y=logpod.judson))  +
    ggtitle("DNT Comparison") +
    geom_point() +
     theme_bw() +
    ylab("Judson log(uM)") +
    xlab("Carstens log(uM)") +
     theme(axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 5),
          plot.margin = margin(t=20,r=20,b=50,l=20),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    geom_abline(intercept=0,slope = 1) +
    geom_text_repel(data=df,aes(x=logpod.carstens,y=logpod.judson,label=name),max.overlaps=Inf)
  print(p)

  if(to.file) {
    fname = paste0("../DNT/data compare.pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()

}
