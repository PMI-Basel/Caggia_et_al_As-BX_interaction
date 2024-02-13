

## data edgeR, TMM normalization, model fitting

# create edgeR object with abundant OTUs and TMM normalize
edgeR.object.TMM <- function(dat, threshold.filter, groups, tax)
{
  ## define abundant OTUs
  # filter with min nr of sequences in min nr of samples
  keep_OTUs <- which(rowSums(dat >= threshold.filter) >= threshold.filter)
  dat_filtered <- dat[keep_OTUs,]
  # filter by mean relative abundance of 0.05 %
  # dat.ra <- t(t(dat)/colSums(dat)) * 1000000
  # dat_filtered <- dat.ra[rowMeans(dat.ra) >= 500,] 
  
  ## define edgeR object
  edgeR_object <- DGEList(counts=dat_filtered,
                          group=groups, 
                          genes=tax[rownames(dat_filtered),])
  
  ## TMM (trimmed mean of M value) normalization
  edgeR_object_TMM <- calcNormFactors(edgeR_object) # skip this step if you don’t want TMM
}

# to fit weighted likelihood empirical Bayes model (on edgeR object)
fit.model.edgeR <- function(edgeR_object, groups)
{
  ## setting the model with one factor
  edgeR_model <- model.matrix(~0 + groups )
  ## estimate dispersion by weighted likelihood empirical Bayes
  # use Empirical Robust Bayes Tagwise Dispersions, suggested by Mark Robinson
  edgeR_object <- estimateGLMRobustDisp(edgeR_object, edgeR_model)
  ## fitting the model
  edgeR_fit <- glmFit(edgeR_object, edgeR_model)
  # colnames(rhizo_edgeR_fit$coefficients)
  
  return(edgeR_fit)
}

# to use OTU abundance data in RA that has been TMM normalized via edgeR object
convert.edgeR.to.datRA <-function(edgeR_object)
{
  dat <- cpm(edgeR_object, normalized.lib.sizes=FALSE, log=FALSE)
  dat <- dat/10000 # only if log=FALSE: transform from cpm to RA in percent
  # total sum as %
  dat <- t(t(dat)/colSums(dat)) * 100
  # remove rows having a sum count = 0
  dat <- dat[rowSums(dat) >= 1,] 
}

