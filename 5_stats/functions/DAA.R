#run aldex2 DAA function
aldex2 <- function(feature_table, meta_data, group){
  # Generate Monte Carlo samples of the Dirichlet distribution for each sample.
  # Convert each instance using the centred log-ratio transform.
  # This is the input for all further analyses.
  x <- aldex.clr(reads = feature_table, conds = meta_data[,colnames(meta_data)==group], 
                 mc.samples = 128, denom = "all", verbose = FALSE) 
  
  # calculates expected values of the Welch's t-test and Wilcoxon rank test on
  # the data returned by aldex.clr
  x_tt <- aldex.ttest(x, paired.test = FALSE, verbose = FALSE)
  # determines the median clr abundance of the feature in all samples and in
  # groups, the median difference between the two groups, the median variation
  # within each group and the effect size, which is the median of the ratio
  # of the between group difference and the larger of the variance within groups
  x_effect <- aldex.effect(x, CI = TRUE, verbose = FALSE)
  # combine all outputs 
  aldex_out <- data.frame(x_tt, x_effect)
  
  #select sensitive ASVs with CI <= 0.05
  subset <- rownames_to_column(aldex_out, "ASV") %>%
    filter(wi.eBH <= 0.05) %>% #here we chose the wilcoxon output rather than tt
    dplyr::select(ASV, we.eBH, wi.eBH, effect, overlap)
  
  sens_ASVS_ALDEx2 <- subset$ASV
  
  return(list(subset, sens_ASVS_ALDEx2))
}

#run ancom DAA function
ancom <- function(feature_table, meta_data, group, DAT_sample_names){
  #phyloseq
  rownames(meta_data) <- meta_data[,colnames(meta_data)==DAT_sample_names]
  pseq <- phyloseq(sample_data(as.data.frame(meta_data)),
                   otu_table(as.matrix(feature_table), taxa_are_rows=T))
  # perform the analysis 
  out = ancombc(phyloseq = pseq, formula = group, p_adj_method = "BH", #zero_cut = 1, 
                lib_cut = 0, group = group, struc_zero = TRUE, neg_lb = TRUE, 
                tol = 1e-5, max_iter = 100, conserve = TRUE, alpha = 0.05, global = TRUE)
  # store the results in res 
  res <- out$res
  res_df <- as.data.frame(res)
  colnames(res_df) <- names(res)
  subset <- rownames_to_column(res_df, "ASV") %>% filter(diff_abn)
  sens_ASVS_ANCOMBC <- subset$ASV
  return(list(subset, sens_ASVS_ANCOMBC))
}

#run masslin2 DAA function
maaslin2 <- function(feature_table, meta_data, group, reference, DAT_sample_names){
  meta_sub <- data.frame(meta_data[,colnames(meta_data)==group])
  rownames(meta_sub) <- meta_data[,colnames(meta_data)==DAT_sample_names]
  colnames(meta_sub) <- group
  fixed_effects <- group
  asv <- t(feature_table) # maaslin expects features as columns and samples as rows 
  
  sink("/dev/null") #suppress function output
  fit_data <- Maaslin2(asv, meta_sub, output = "DAA example", transform = "AST",
                       fixed_effects = fixed_effects, reference = reference,  
                       normalization = "TSS", standardize = FALSE,
                       min_prevalence = 0, plot_heatmap = FALSE, plot_scatter = FALSE,)
  sink()
  subset <- fit_data$results %>% filter(qval <= 0.05)
  sens_ASVS_Maaslin2 <- subset$feature
  return(list(subset, sens_ASVS_Maaslin2))
}

#run metagenomeSeq DAA function
metagenomeSeq <- function(feature_table, meta_data, group, DAT_sample_names){
  #meta_sub <- data.frame(meta_data[,colnames(meta_data)==group])
  rownames(meta_data) <- meta_data[,colnames(meta_data)==DAT_sample_names]
  #colnames(meta_sub) <- group
  MR <- newMRexperiment(counts=feature_table, phenoData=AnnotatedDataFrame(meta_data))
  MR_norm <-  cumNorm(MR, p = cumNormStatFast(MR)) #CSS normalization
  mod <- model.matrix(as.formula(paste("~", group)), data = pData(MR_norm))
  fitted_mod <-  fitFeatureModel(MR_norm, mod)
  
  stats <- MRcoefs(fitted_mod, number=length(fitted_mod@pvalues)) #extract stats
  subset <- stats[stats$adjPvalues <= 0.05,] #sign ASVs
  sens_ASVS_metagenomeSeq <- rownames(subset)
  return(list(subset, sens_ASVS_metagenomeSeq))
}

#run all for DAA functions
run_all_DAA <- run_all_DAA <- function(feature_table, meta_data, group, DAT_sample_names, reference){
  #aldex2
  temp <- aldex2(feature_table, meta_data, group)
  aldex2_sign <- temp[[1]]; aldex2_sensASV <- temp[[2]]; rm(temp)
  message("aldex2 done")
  #ancom
  temp <- ancom(feature_table, meta_data, group, DAT_sample_names)
  ancom_sign <- temp[[1]]; ancom_sensASV <- temp[[2]]; rm(temp)
  message("ancom done")
  #maaslin2
  temp <- maaslin2(feature_table, meta_data, group, reference, DAT_sample_names)
  maaslin2_sign <- temp[[1]]; maaslin2_sensASV <- temp[[2]]; rm(temp)
  message("maaslin2 done")
  #metagenomeSeq
  temp <- metagenomeSeq(feature_table, meta_data, group, DAT_sample_names)
  metagenomeSeq_sign <- temp[[1]]; metagenomeSeq_sensASV <- temp[[2]]; rm(temp)
  message("metagenomeSeq done")
  #summarise methods
  summ <- data.frame(ASV=rownames(feature_table))
  summ$aldex2 <- summ$ASV %in% aldex2_sensASV
  summ$ancombc <- summ$ASV %in% ancom_sensASV
  summ$maaslin2 <- summ$ASV %in% maaslin2_sensASV
  summ$metagenomeSeq <- summ$ASV %in% metagenomeSeq_sensASV
  summ$score <- rowSums(summ[,2:5])
  return(list(summ, aldex2_sign, ancom_sign, maaslin2_sign, metagenomeSeq_sign))
}

# edger <- function(feature_table, meta_data, taxa=TAX){
# 
#   EDGER <- edgeR.object.TMM(feature_table, 7, meta_data$Plastic, TAX[rownames(TAX) %in% rownames(feature_table),])
#   
#   EDGER_fit <- fit.model.edgeR(EDGER, meta_data$Plastic)
#   
#   #glmLRT
#   bcontrasts <- makeContrasts(1 - 2, levels=meta_data$Plastic)
#   bEDGER_fit_lrt <- glmLRT(EDGER_fit, contrast=bcontrasts)
#   bEDGER_fit_lrt_topTags <- topTags(bEDGER_fit_lrt, n=Inf, p.value=1)  # all stat tests
#   bEDGER_fit_lrt_table <- topTags(bEDGER_fit_lrt, n=nrow(EDGER_fit$counts), adjust.method="BH", sort.by="PValue", p.value=0.05)
#   bEDGER_fit_lrt_table_ASVs <- rownames(bEDGER_fit_lrt_table)
#   bEDGER_fit_lrt_table_ASVs_percent <- round(100 / nrow(EDGER_fit$counts) * length(bEDGER_fit_lrt_table_ASVs), 1)
#   
#   #summary of differentially abundant ASVs
#   bedgeR_summary <- t(summary(decideTestsDGE(bEDGER_fit_lrt, p.value=0.05)))
#   colnames(bedgeR_summary) <- c("lower in CTRL", "unchanged", "higher in CTRL")
#   rownames(bedgeR_summary) <- NULL
#   
#   #Identification of differentially abundant ASVs
#   lrt_bASVs <- rownames(topTags(bEDGER_fit_lrt, n=nrow(DAT_rare), adjust.method="BH", sort.by="PValue", p.value=0.05))
#   
#   sens_ASVS_edgeR <- lrt_bASVs
#   
#   return(list(bedgeR_summary, sens_ASVS_edgeR))
# }

