alpha_div_test <- function(design, dat, tax, variable, title) {

dat_phy <- phyloseq(
  sample_data(design),
  otu_table(dat, taxa_are_rows=T),
  tax_table(as.matrix(tax[rownames(dat),])))

richness_mat <- c()
diversity_mat <- c()
even_mat <- c()
# for (i in 1:500){
# generate rarefied community
temp_mat <- t(rrarefy(t(otu_table(dat_phy)), 10000))
# richness is species number
s <- specnumber(temp_mat, MARGIN=2)
#cbind( colSums(temp_mat>0),specnumber(temp_mat, MARGIN=2))
richness_mat <- cbind(richness_mat,s)
# from Jost 2006: D=exp(H), exponential of shannon entropy
shannon <- vegan::diversity(temp_mat, index="shannon", MARGIN=2)
d <- exp(shannon)
diversity_mat <- cbind(diversity_mat,d)
# Hill's ratio also called Sheldon's evenness
j <- exp(shannon)/s
even_mat <- cbind(even_mat,j)
#}
# calculate means of subsamples
otu_diversity <- rowMeans(diversity_mat)
otu_richness <- rowMeans(richness_mat)
otu_evenness <- rowMeans(even_mat)
# put them all in one dataframe
summary_diversity_16S <- data.frame( cbind( otu_diversity, otu_richness, otu_evenness))
stopifnot(identical(rownames(sample_data(dat_phy)), names(otu_richness)))
for_anova_16S <- cbind(summary_diversity_16S, sample_data(dat_phy))

### ANOVA
model_richness_16S <- aov(log(otu_richness) ~ variable,data=for_anova_16S)
model_diversity_16S <- aov(log(otu_diversity) ~ variable,data=for_anova_16S)
model_evenness_16S <- aov(log(otu_evenness) ~ variable,data=for_anova_16S)

table_values <- cbind(c("richness", "diversity", "evenness"), 
      c(round(anova(model_richness_16S)$"Pr(>F)"[[1]],2),
round(anova(model_diversity_16S)$"Pr(>F)"[[1]],2),
round(anova(model_evenness_16S)$"Pr(>F)"[[1]],2)))

table_values <- data.frame(table_values)
colnames(table_values)<- list("",title)

table_values
}

capture.output(summary(model_richness_16S), file="../results/Table_ANOVA_richness_16S.txt")

# adapted from Natacha's script
