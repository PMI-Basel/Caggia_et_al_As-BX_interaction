phyla_prop_test <- function(dat_phy) {
  
  #require(phyloseq)
  dat_glom <- tax_glom(dat_phy, "phylum") # merge by phylum
  dat_glom_rel = transform_sample_counts(dat_glom, function(x) 100 * x/sum(x)) 
  dat_glom_rel_melt <- psmelt(dat_glom_rel)
  dat_glom_rel_melt$phylum <- as.character(dat_glom_rel_melt$phylum)
  
  p_val_anova<- c(1:length(unique(dat_glom_rel_melt$phylum)))
  p_val_kruskal<- c(1:length(unique(dat_glom_rel_melt$phylum)))
  
  for (i in 1:length(unique(dat_glom_rel_melt$phylum)))  {
    p_val_anova[i] <- anova(aov(dat_glom_rel_melt$Abundance
                                [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]]
                                ~dat_glom_rel_melt$plant_genotype
                                [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]]))$"Pr(>F)"[[1]]
    #qqnorm(residuals(aov(dat_glom_rel_melt$x[dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]] ~dat_glom_rel_melt$y[dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]])))
    p_val_kruskal[i] <- kruskal.test(dat_glom_rel_melt$Abundance
                                     [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]]
                                     ~dat_glom_rel_melt$plant_genotype
                                     [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]])$p.value
    p_val_anova <- round(p_val_anova,2)
    p_val_kruskal <- round(p_val_kruskal,2)
    
  }
  print(data.frame(p_val_anova,p_val_kruskal,unique(dat_glom_rel_melt$phylum)))
}

# try with attach and detach

phyla_prop_test_bxdef <- function(dat_phy) {
  
  #require(phyloseq)
  dat_glom <- tax_glom(dat_phy, "phylum") # merge by phylum
  dat_glom_rel = transform_sample_counts(dat_glom, function(x) 100 * x/sum(x)) 
  dat_glom_rel_melt <- psmelt(dat_glom_rel)
  dat_glom_rel_melt$phylum <- as.character(dat_glom_rel_melt$phylum)
  
  p_val_anova<- c(1:length(unique(dat_glom_rel_melt$phylum)))
  p_val_kruskal<- c(1:length(unique(dat_glom_rel_melt$phylum)))
  
  for (i in 1:length(unique(dat_glom_rel_melt$phylum)))  {
    p_val_anova[i] <- anova(aov(dat_glom_rel_melt$Abundance
                                [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]]
                                ~dat_glom_rel_melt$BX_def_wo6
                                [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]]))$"Pr(>F)"[[1]]
    #qqnorm(residuals(aov(dat_glom_rel_melt$x[dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]] ~dat_glom_rel_melt$y[dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]])))
    dat_glom_rel_melt$BX_def_wo6 <- as.factor(dat_glom_rel_melt$BX_def_wo6)
    p_val_kruskal[i] <- kruskal.test(dat_glom_rel_melt$Abundance
                                     [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]]
                                     ~dat_glom_rel_melt$BX_def_wo6
                                     [dat_glom_rel_melt$phylum==unique(dat_glom_rel_melt$phylum)[i]])$p.value
    p_val_anova <- round(p_val_anova,2)
    p_val_kruskal <- round(p_val_kruskal,2)
    
  }
  print(data.frame(p_val_anova,p_val_kruskal,unique(dat_glom_rel_melt$phylum)))
}

