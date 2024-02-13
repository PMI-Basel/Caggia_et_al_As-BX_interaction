
### Kyle's MA plot in a function

# define root_CH_edgeR_TMM, design_root_CH, factor separating in design (here plant genotype or groups), plot title : "bacteria in changins roots"

maplot<- function(edgeR_object, design, factor, col_none, col_plus, col_minus, title, ylim, xlim, ylab, xlab)
{
  ## Define BX+ / - enriched BACTERIA 
  # root Changins
  design<-droplevels(design, factor)
  
  otu_norm_16s_enrich <- cpm(edgeR_object, normalized.lib.sizes=T, log=F)
  model_mat_enrich_16s <- model.matrix(~factor, data=design)
  
  dge_enrich_16s <- estimateGLMRobustDisp(edgeR_object, design=model_mat_enrich_16s)
  #plotBCV(dge_enrich_16s)
  #plotSmear(dge_enrich_16s,pair=c("root_changins_B73_bx1","root_changins_B73_WT"))
  
  fit_enrich_16s <- glmFit(dge_enrich_16s, design=model_mat_enrich_16s)
  lrt_enrich_16s <- glmLRT(fit_enrich_16s, coef=colnames(fit_enrich_16s$design)[[2]])
  tt_enrich_16s <- topTags(lrt_enrich_16s, n=Inf, p.value=1)
  head(tt_enrich_16s$table)
  
  # select significantly enriched or depleted
  soil_enrich_16s <- tt_enrich_16s$table[tt_enrich_16s$table$logFC < 0 & tt_enrich_16s$table$FDR < 0.05,]
  root_enrich_16s <- tt_enrich_16s$table[tt_enrich_16s$table$logFC > 0 & tt_enrich_16s$table$FDR < 0.05,]
  
  # data.frame with all, with enrich/deplet info
  tt_enrich_16s <- as.data.frame(tt_enrich_16s)
  forMA_16S <- data.frame(tt_enrich_16s$logCPM,
                          tt_enrich_16s$logFC,
                          tt_enrich_16s$FDR<0.05)
  colnames(forMA_16S) <- c("logCPM", "logFC", "signif")
  
  ## define colors
  forMA_16S$col[forMA_16S$signif==F] <- col_none
  forMA_16S$col[forMA_16S$logFC>0 & forMA_16S$signif==T] <- col_plus
  forMA_16S$col[forMA_16S$logFC<0 & forMA_16S$signif==T] <- col_minus
  # order for plotting colors
  forMA_16S$ord[forMA_16S$col==col_none] <- 1
  forMA_16S$ord[forMA_16S$col==col_plus] <- 2
  forMA_16S$ord[forMA_16S$col==col_minus] <- 3
  forMA_16S <- forMA_16S[sort(forMA_16S$ord,ind=T,decr=F)$ix,]
  
  ## define pch
  forMA_16S$pch[forMA_16S$signif==F] <- 1
  forMA_16S$pch[forMA_16S$signif==T] <- 16
  
  ## plot
  plot(forMA_16S$logCPM, 1*forMA_16S$logFC, log="x", ylim=ylim, xlim=xlim,
       main=title,
       ylab=ylab, xlab=xlab,
       col=forMA_16S$col, cex=1, bg=forMA_16S$col, pch=forMA_16S$pch, las=1)
  
}


### glmLRT produce -logFC for WT-enriched and +logFC for bx1-enriched if I get it well
# 
# design_root_ZH<-droplevels(design_root_ZH, design_root_ZH$plant_genotype)
# maplot_root_ZH <- maplot(root_ZH_edgeR_TMM, design_root_ZH, factor=design_root_ZH$plant_genotype, title="Bacteria in roots ZH", ylim=c(4,-4))
# 
# maplot<- function(edgeR_object, design, factor, title, ylim)
# {
#   ## Define BX+ / - enriched BACTERIA 
#   # root Changins
#   design<-droplevels(design_root_ZH, design_root_ZH$plant_genotype)
#   
#   otu_norm_16s_enrich <- cpm(root_ZH_edgeR_TMM, normalized.lib.sizes=T, log=F)
#   model_mat_enrich_16s <- model.matrix(~design_root_ZH$plant_genotype, data=design_root_ZH)
#   
#   dge_enrich_16s <- estimateGLMRobustDisp(root_ZH_edgeR_TMM, design=model_mat_enrich_16s)
#   #plotBCV(dge_enrich_16s)
#   #plotSmear(dge_enrich_16s,pair=c("root_changins_B73_bx1","root_changins_B73_WT"))
#   
#   fit_enrich_16s <- glmFit(dge_enrich_16s, design=model_mat_enrich_16s)
#   lrt_enrich_16s <- glmLRT(fit_enrich_16s, coef=colnames(fit_enrich_16s$design)[[2]])
#   tt_enrich_16s <- topTags(lrt_enrich_16s, n=Inf, p.value=1)
#   head(tt_enrich_16s$table)
#   
#   # select significantly enriched or depleted
#   soil_enrich_16s <- tt_enrich_16s$table[tt_enrich_16s$table$logFC < 0 & tt_enrich_16s$table$FDR < 0.05,]
#   root_enrich_16s <- tt_enrich_16s$table[tt_enrich_16s$table$logFC > 0 & tt_enrich_16s$table$FDR < 0.05,]
#   
#   # data.frame with all, with enrich/deplet info
#   tt_enrich_16s <- as.data.frame(tt_enrich_16s)
#   forMA_16S <- data.frame(tt_enrich_16s$logCPM,
#                           tt_enrich_16s$logFC,
#                           tt_enrich_16s$FDR<0.05)
#   colnames(forMA_16S) <- c("logCPM", "logFC", "signif")
#   
#   ## define colors
#   forMA_16S$col[forMA_16S$signif==F] <- "dimgrey"
#   forMA_16S$col[forMA_16S$logFC>0 & forMA_16S$signif==T] <- "sienna4"
#   forMA_16S$col[forMA_16S$logFC<0 & forMA_16S$signif==T] <- "gold3"
#   # order for plotting colors
#   forMA_16S$ord[forMA_16S$col=="dimgrey"] <- 1
#   forMA_16S$ord[forMA_16S$col=="sienna4"] <- 2
#   forMA_16S$ord[forMA_16S$col=="gold3"] <- 3
#   forMA_16S <- forMA_16S[sort(forMA_16S$ord,ind=T,decr=F)$ix,]
#   
#   ## define pch
#   forMA_16S$pch[forMA_16S$signif==F] <- 1
#   forMA_16S$pch[forMA_16S$signif==T] <- 16
#   
#   ## plot
#   plot(forMA_16S$logCPM, forMA_16S$logFC, log="x", ylim=ylim,
#        main=title,
#        ylab="log fold change -WT +bx1", xlab="average abundance logCPM",
#        col=forMA_16S$col, cex=1, bg=forMA_16S$col, pch=forMA_16S$pch, las=1)
#   text(rep(95,2), c(-7,7), label=c("bx1", "WT"), adj=0, cex=1.5)
# }
# 
# 
# 
