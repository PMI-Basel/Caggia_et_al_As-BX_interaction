###############################################
## function loading wrapper ##
###############################################
## NOTE: 
## Loads all functions required for the analysis 

functions <- function(path="functions/") {
  
  #add "/" at the end if not there
  if (substring(path, nchar(path), nchar(path)) != "/") {path <- paste0(path, "/")}
  
  #load
  source(paste0(path,"libraries.R"))
  source(paste0(path,"fun_norm_edgeR_obj.R"))
  source(paste0(path,"fun_maplot.R"))
  source(paste0(path,"vennDia.R"))
  source(paste0(path,"maPalette.R"))
  source(paste0(path,"fun_pairwise_comp.R"))
  source(paste0(path,"fun_venn_plot.R"))
  source(paste0(path,"fun_venn_pair.R"))
  source(paste0(path,"fun_venn_triple.R"))
  source(paste0(path,"fun_union_intersect.R"))
  source(paste0(path,"fun_table_numOTU.R"))
  source(paste0(path,"fun_boxplot_OTUs.R"))
  source(paste0(path,"fun_test_prop.R"))
  source(paste0(path,"fun_barplot_TAX.R"))
  source(paste0(path,"fun_sTAXlab.R"))
  source(paste0(path,"fun_calculate_rar_curve.R"))
  source(paste0(path,"phyloseq_merge_mean.R"))
  source(paste0(path,"raref.R"))
  source(paste0(path,"raref_design.R"))
  source(paste0(path,"variability_table.R"))
  source(paste0(path,"CLOUD.R"))
}
  
