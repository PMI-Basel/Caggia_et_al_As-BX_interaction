axis_label_pco <- function(ordination){
  extract_eigenvalue.pcoa=function(ordination) ordination$values$Relative_eig
  if (length(extract_eigenvalue.pcoa(ordination)) > 0) {
    eigvec=extract_eigenvalue.pcoa(ordination)
    fracvar=eigvec/sum(eigvec)
    percvar=round(100 * fracvar, 1)
    # strivar=as(c(p0$label$x, p0$label$y), "character")
    strivar=as(c("PCo 1","PCo 2"), "character")
    strivar=paste0(strivar, "   [", percvar, "%]")
    print(strivar[1:2])
  }
}
