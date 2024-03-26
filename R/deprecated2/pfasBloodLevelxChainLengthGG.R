#--------------------------------------------------------------------------------------
#'
#' Plotthe blood levels by chain length
#' @param to.file If TRUE, write graphs to a file
#' @param data.version Label of folder where input data sits
#'
#--------------------------------------------------------------------------------------
pfasBloodLevelxChainLengthGG <- function(to.file=F) {
  printCurrentFunction()
  dir = paste0("data/")

  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  mat = read.xlsx(file)
  #mat = mat[!is.element(mat$metric,c("concentration")),]
  mat[is.element(mat$metric,"LOQ"),"metric"] = "  LOQ"
  mat[is.element(mat$metric,"LOD"),"metric"] = "  LOD"
  mat[is.element(mat$metric,"minimum"),"metric"] = " minimum"
  mat = mat[mat$value>0,]
  dlist = unique(mat$dtxsid)
  mat$chain_length = NA
  file = paste0(dir,"pfas_catalog 2021-10-06.xlsx")
  catalog = read.xlsx(file)
  rownames(catalog) = catalog$dtxsid
  for(dtxsid in dlist) {
    cl = catalog[dtxsid,"chain_length"]
    mat[is.element(mat$dtxsid,dtxsid),"chain_length"] = cl
  }
  mat = mat[!is.na(mat$chain_length),]
  ####################################################################
  title = paste0("")
  p = ggplot(data=mat,aes(x=as.factor(chain_length),y=value))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE) +
    scale_y_continuous(trans="log10",limits=c(0.01,10000)) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab("Conc (ng/mL)") +
    xlab("Chain Length") +
    labs(color = "Exposure Status") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 5),
          plot.margin = margin(t=20,r=20,b=50,l=20),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    geom_jitter(aes(color=exposed),size=0.5,alpha = 0.9) #+
    #geom_hline(yintercept=1)
  #    geom_hline(color="gray", size = 0.5,yintercept=median_sig_bmd) +
  #    geom_hline(color="cyan", size = 0.5,yintercept=burst_pod_sig)
  print(p)

  if(to.file) {
    fname = paste0("data/figures/pfasBloodLevelxChainLengthGG.pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}

