# rarefaction
raref <- function(data, design, threshold.rar, threshold.filter) {
  if (min(colSums(data)) < threshold.rar) {
    index_outliers <- which(colSums(data) < threshold.rar)
    data_for_rare <- data[ ,-index_outliers]
    design_rare <- design[-index_outliers, ]
  } else {
    data_for_rare <- data
    design_rare <- design
  }
  stopifnot(identical(rownames(design_rare), colnames(data_for_rare)))
  set.seed(74500)     # 74500=zip code of Evian
  data_rare <- t(rrarefy(t(data_for_rare), threshold.rar))
  data_rare <- data_rare[rowSums(data_rare) > 0,]  # removal of rows with 0 values
}
