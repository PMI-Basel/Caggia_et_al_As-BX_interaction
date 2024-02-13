# design excluding samples below rarefying threshold
raref.design <- function(data, design, threshold.rar) {
  if (min(colSums(data)) < threshold.rar) {
    index_outliers <- which(colSums(data) < threshold.rar)
    design_rare <- design[-index_outliers, ]
  } else {
    design_rare <-design
  }
  design_rare
}