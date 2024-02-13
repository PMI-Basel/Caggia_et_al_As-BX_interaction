

## Vincent Somerville (+Selma), 16.05.2017 : plot boxplot of OTU count for different treatments sorted by OTU median

boxplot_OTU <- function(taxon = taxon, designTable = designTable, signif_OTU = signif_OTU, rect=TRUE, title = title, number_OTUs = number_OTUs)
{
  library(reshape2)
  library(ggplot2)
  library(matrixStats)
  
  taxon <- taxon[,match(rownames(designTable),colnames(taxon))]
  designTable <- droplevels(designTable) 
  treatments <- levels(designTable$plant_genotype)
  taxon <- taxon[order(rowMedians(as.matrix(taxon[,which(designTable$plant_genotype==treatments[1])])),decreasing=T),]
  designTable <- designTable[match(colnames(taxon),rownames(designTable)),] 
  stopifnot(all(identical(colnames(taxon),rownames(designTable))))
  colnames(taxon) <- designTable$plant_genotype
  taxon <- melt(t(taxon))
  selected<-levels(taxon$Var2)[1:number_OTUs]
  taxon_selected<-taxon[taxon$Var2 %in% selected,]
  
  ##plot
  p<- ggplot(taxon_selected)+
    geom_boxplot(aes(x=Var2,y=log(value),fill=Var1,color=Var1),outlier.size=0)+
    ggtitle(title)+
    ylab("Count")+
    xlab("")+
    theme_bw()+ 
    theme(
      plot.title=element_text(face="bold",size=18,hjust=0.5,vjust=1),
      axis.title.x=element_text(face="bold",size=16),
      axis.title.y=element_text(face="bold",size=16,angle=90),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      legend.title=element_blank(),
      legend.text=element_text(size=12),
      legend.key.size=unit(1.5,"lines"),
      plot.margin = unit(c(1,1,1,1),"cm"),
      legend.key=element_blank(),
      axis.text.x = element_text(angle=45,hjust=1)
    )
  
  if (rect)
  { 
    signifOTU_position<-match(intersect(selected, signif_OTU),selected)
    # prepare data frame
    signifOTU_present<-intersect(selected, signif_OTU)
    ymax=ggplot_build(p)$layout$panel_ranges[[1]]$y.range[[2]] #value max y axis
    ymin=ggplot_build(p)$layout$panel_ranges[[1]]$y.range[[1]] #value min y axis
    rect_df <- data.frame(x1=-0.5+signifOTU_position,x2=0.5+signifOTU_position,y1=rep(ymin,length(signifOTU_present)), y2=rep(ymax,length(signifOTU_present)))
    p2 <- p + geom_rect(data=rect_df, aes(xmin=x1,xmax=x2,ymin=-Inf,ymax=Inf),fill="gold",
                        alpha=0.1)
    
    print(p2)
  }
  else {print(p)}
}


