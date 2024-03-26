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
pfasMoeBoxplot <- function(to.file=F) {
  printCurrentFunction()
  dir = "data/"
  file = "data/PFAS corrected moe.xlsx"
  res = read.xlsx(file)
  excludes = c("05th percentile","75th percentile","25th percentile")
  excludes = c("maximum")
  #res = res[!is.element(res$metric,excludes),]

  res$class = "<95th percentile"
  res[is.element(res$metric,c("95th percentile","98th percentile","99th percentile","maximum")),"class"] = ">95th percentile"
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
    geom_jitter(aes(color=exposed),size=0.5,alpha = 0.9, width=0.15) +
    geom_hline(yintercept=1) +
    geom_hline(yintercept=100)

  #    geom_hline(color="gray", size = 0.5,yintercept=median_sig_bmd) +
  #    geom_hline(color="cyan", size = 0.5,yintercept=burst_pod_sig)
  print(p)

  if(to.file) {
    fname = paste0("data/figures/PFAS all chem moe.pdf")
    ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
     dev.off()
  }
  else browser()

#
#   title = paste0("Raw MoE")
#   p = ggplot(data=res,aes(x=reorder(nickname,moe,FUN=min),y=raw_moe))  +
#     ggtitle(title) +
#     geom_boxplot(outlier.colour="black",
#                  outlier.shape=21,outlier.size=2,outlier.fill="white",
#                  notch=FALSE) +
#     scale_y_continuous(trans="log10",limits=c(0.01,10000)) +
#     scale_fill_manual(values=c("white","red")) +
#     coord_flip() +
#     theme_bw() +
#     ylab("MoE") +
#     xlab("") +
#     labs(color = "Exposure Status") +
#     theme(axis.text=element_text(size=12),
#           axis.title=element_text(size=16,face="bold"),
#           plot.title=element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
#           strip.text.x = element_text(size = 10),
#           plot.margin = margin(t=20,r=20,b=50,l=20),
#           legend.text = element_text(size=12),
#           legend.title = element_text(size=12)) +
#     geom_jitter(aes(color=exposed),size=0.5,alpha = 0.9) +
#     geom_hline(yintercept=1)
#   #    geom_hline(color="gray", size = 0.5,yintercept=median_sig_bmd) +
#   #    geom_hline(color="cyan", size = 0.5,yintercept=burst_pod_sig)
#   print(p)
#
#   if(to.file) {
#     fname = paste0("data/figures/PFAS all chem moe.pdf")
#     ggsave(plot = p, width = 8, height = 8, dpi = 300, filename =fname)
#     dev.off()
#   }
#   else browser()

}
