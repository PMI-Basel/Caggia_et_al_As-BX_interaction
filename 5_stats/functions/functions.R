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
  source(paste0(path,"variability_table.R"))
  source(paste0(path,"ancom.R"))
  source(paste0(path,"DAA.R"))
  source(paste0(path,"CLOUD.R"))
}
  
