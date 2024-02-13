


### Generate table with phyla with significant logFC and number of OTUs for each

diff_phyla_OTU_table_moche <- function(x = lrt_OTU_logFC_table,  caption = title)
{
  otu_nr_minusFC <- numeric(length(unique(x$phylum)))
  otu_nr_plusFC <- numeric(length(unique(x$phylum)))
  for (i in 1:length(unique(x$phylum)))
  {
    otu_nr_minusFC[i] <- 
      ifelse (
        length(rownames(x[x$logFC<1 & x$phylum==unique(x$phylum)[[i]],])) > 0, 
        length(rownames(x[x$logFC<1 & x$phylum==unique(x$phylum)[[i]],])),
        "-")
    otu_nr_plusFC[i] <- 
      ifelse (
        length(rownames(x[x$logFC>1 & x$phylum==unique(x$phylum)[[i]],])) > 0, 
        length(rownames(x[x$logFC>1 & x$phylum==unique(x$phylum)[[i]],])),
        "-")
  }
  p<-data.frame(rbind(as.numeric(otu_nr_minusFC),as.numeric(otu_nr_plusFC)))
  colnames(p) <-unique(x$phylum)
  rownames(p) <- c("bx1", "WT")
  print(p)
}




diff_tax_OTU_table <- function(x = lrt_OTU_logFC_table, tax_level=tax_level, caption = title)
{
  otu_nr_minusFC <- numeric(length(unique(tax_level)))
  otu_nr_plusFC <- numeric(length(unique(tax_level)))
  for (i in 1:length(unique(tax_level)))
  {
    otu_nr_minusFC[i] <- 
      ifelse (
        length(rownames(x[x$logFC<1 & tax_level==unique(tax_level)[[i]],])) > 0, 
        length(rownames(x[x$logFC<1 & tax_level==unique(tax_level)[[i]],])),
        "-")
    otu_nr_plusFC[i] <- 
      ifelse (
        length(rownames(x[x$logFC>1 & tax_level==unique(tax_level)[[i]],])) > 0, 
        length(rownames(x[x$logFC>1 & tax_level==unique(tax_level)[[i]],])),
        "-")
  }
  p<-data.frame(rbind(as.numeric(otu_nr_minusFC),as.numeric(otu_nr_plusFC)))
  colnames(p) <-unique(tax_level)
  rownames(p) <- c("bx1", "WT")
  print(p)
}



diff_phyla_OTU_table <- function(x = x, tax_level=tax_level)
{
  otu_nr_minusFC <- numeric(length(unique(tax_level)))
  otu_nr_plusFC <- numeric(length(unique(tax_level)))
  for (i in 1:length(unique(tax_level)))
  {
    otu_nr_minusFC[i] <- 
      ifelse (
        length (
          rownames(
            x[x$logFC<1 & tax_level == unique(tax_level)[[i]],])) > 0, 
        length (
          rownames(
            x[x$logFC<1 & tax_level == unique(tax_level)[[i]],])),
        "-")
    otu_nr_plusFC[i] <- 
      ifelse (
        length (
          rownames (
            x[x$logFC>1 & tax_level ==unique(tax_level)[[i]],]
            )) > 0, 
        length (
          rownames ( 
            x[x$logFC>1 & tax_level ==unique(tax_level)[[i]],]
            )), "-")
  }
  p <- data.frame(unique(tax_level),as.numeric(otu_nr_minusFC),as.numeric(otu_nr_plusFC))
  colnames(p) <- c("tax.group", "bx1.enriched", "WT.enriched")
  print(p)
}


# take out numeric transfo to have "-" instead of NA in nice table
# table for genus -> minus and plus separated

#

#x=rhizo_CH_TMM_lrt_OTUs_logFC_table
# otu_nr_minusFC <- numeric(length(unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)))
# otu_nr_minusFC[1] <- 
#   ifelse (
#     length(rownames(rhizo_CH_TMM_lrt_OTUs_logFC_table[rhizo_CH_TMM_lrt_OTUs_logFC_table$logFC>1 & rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum==unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)[[1]],])) > 0, 
#     length(rownames(rhizo_CH_TMM_lrt_OTUs_logFC_table[rhizo_CH_TMM_lrt_OTUs_logFC_table$logFC>1 & rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum==unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)[[1]],])),
#     "-")
# otu_nr_minusFC[2] <- 
#   ifelse (
#     length(rownames(rhizo_CH_TMM_lrt_OTUs_logFC_table[rhizo_CH_TMM_lrt_OTUs_logFC_table$logFC>2 & rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum==unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)[[2]],])) > 0, 
#     length(rownames(rhizo_CH_TMM_lrt_OTUs_logFC_table[rhizo_CH_TMM_lrt_OTUs_logFC_table$logFC>2 & rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum==unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)[[2]],])),
#     "-")
# otu_nr_minusFC[3] <- 
#   ifelse (
#     length(rownames(rhizo_CH_TMM_lrt_OTUs_logFC_table[rhizo_CH_TMM_lrt_OTUs_logFC_table$logFC>3 & rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum==unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)[[3]],])) > 0, 
#     length(rownames(rhizo_CH_TMM_lrt_OTUs_logFC_table[rhizo_CH_TMM_lrt_OTUs_logFC_table$logFC>3 & rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum==unique(rhizo_CH_TMM_lrt_OTUs_logFC_table$phylum)[[3]],])),
#     "-")


