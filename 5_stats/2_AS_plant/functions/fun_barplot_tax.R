
# fonction barplot taxonomy selon sample (phyloseq), changé couleur de l'outline

plot_bar <- function (physeq, x = "Sample", y = "Abundance", fill = NULL, 
                      title = NULL, facet_grid = NULL) 
{
  mdf = psmelt(physeq)
  p = ggplot(mdf, aes_string(x = x, y = y, fill = fill))
  p = p + geom_bar(stat = "identity", position = "stack")
  p = p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  return(p)
}
# https://github.com/joey711/phyloseq/issues/468








### Plot phylogenetic proportions

# adapted from:
# how to divide abundance by number of sample?
# https://github.com/joey711/phyloseq/issues/468

phyla.distr <- function(phy_object, factor, tax_level)
{
  phy_object_prune <- prune_taxa(
    names( sort( taxa_sums(phy_object), decreasing = T )[ 1:10 ] ),
    phy_object
  )
  #merge
  phy_object_merge <- merge_samples(phy_object, factor)
  sample_data(phy_object_merge)$groups <- factor(sample_names(phy_object_merge))
  #taxa merge
  phy_object_merge <- tax_glom(phy_object_merge, tax_level)
  phy_object_merge = transform_sample_counts(phy_object_merge, function(x) 100 * x/sum(x))
  p <- plot_bar(phy_object_merge, x=factor,fill=tax_level)+
    geom_bar( aes(color = phylum, fill = phylum ),position="dodge",stat = 'identity') +
    ylab("Relative abundance (% of total sequences)")
  print(p)
}

# (changé couleur de l'outline)
# ex: phyla.distr(rhizo_CH_phy_TMM, "groups", "phylum")



