library(openxlsx)
library(gplots)
library(ggplot2)
library(stringr)
library(forcats)
#library(scales)
#-------------------------------------------------------------------------------
#' Plot the distribution of PODs
#' @param dir The directory where the lists are stored
#-------------------------------------------------------------------------------
pfasMoeBoxplotOPPT <- function(to.file=F) {
  printCurrentFunction()
  dir = "data/"
  file = "data/PFAS corrected moe.xlsx"
  res = read.xlsx(file)
  # excludes = c("05th percentile","75th percentile","25th percentile")
  # excludes = c("maximum")
  # #res = res[!is.element(res$metric,excludes),]
  #
  # res$class = "<95th percentile"
  # res[is.element(res$metric,c("95th percentile","98th percentile","99th percentile","maximum")),"class"] = ">95th percentile"

  mlist = c("95th percentile","98th percentile","99th percentile","maximum")
  nnlist = c("PFOA","PFNA","PFUnDA","PFTrDA","PFTeDA","PFHxDA","PFoDA","PFDA","PFTriDA")
  res = res[is.element(res$nickname,nnlist),]
  res = res[is.element(res$metric,mlist),]

  file = paste0(dir,"PFAS biomonitoring data final.xlsx")
  biomon = read.xlsx(file)
  biomon = biomon[is.element(biomon$nickname,nnlist),]
  biomon = biomon[is.element(biomon$metric,mlist),]
  nlist = c("dtxsid","casrn","name","nickname","matrix","metric","value","units",
            "source","location","population","assay_name","desc","url" )
  biomon = biomon[,nlist]
  file = paste0(dir,"OPPT/PFAS biomonitoring data for OPPT.xlsx")
  write.xlsx(biomon,file)
  title = paste0("")
  p = ggplot(data=res,aes(x= fct_rev(reorder(nickname,moe,FUN=min)),y=moe))  +
    ggtitle(title) +
    geom_boxplot(outlier.colour="black",
                 outlier.shape=21,outlier.size=2,outlier.fill="white",
                 notch=FALSE) +
    scale_y_continuous(trans="log10",limits=c(0.01,10000),breaks=c(0.01,0.1,1,10,100,1000,10000),
                       labels = function(x) format(x, scientific = TRUE,digits=NULL)) +
    #    scale_y_continuous(trans="log10",breaks=pretty_breaks()) +
    scale_fill_manual(values=c("white","red")) +
    coord_flip() +
    theme_bw() +
    ylab("BCBCR") +
    xlab("") +
    labs(color = "Exposure Status") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
          strip.text.x = element_text(size = 10),
          plot.margin = margin(t=20,r=20,b=50,l=20),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12)) +
    geom_jitter(aes(color=exposed),size=1,alpha = 0.9, width=0.15) +
    geom_hline(yintercept=1) +
    geom_hline(yintercept=100)

  print(p)

  if(to.file) {
    fname = paste0("data/OPPT/PFAS all chem moe OPPT.pdf")
    ggsave(plot = p, width = 8, height = 6, dpi = 300, filename =fname)
    dev.off()
  }
  else browser()
}
