libraries <- function(){
  #BiocManager
  if (!requireNamespace("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
  if (!require(phyloseq)){BiocManager::install("phyloseq")}; library(phyloseq)
  if (!require(vegan)){BiocManager::install("vegan")}; library(vegan)

  #data manipulation
  if (!require(readxl)){install.packages("readxl")}; library(readxl)
  if (!require(tidyr)){install.packages("tidyr")}; library(tidyr)
  if (!require(dplyr)){install.packages("dplyr")}; library(dplyr)
  if (!require(tibble)){install.packages("tibble")}; library(tibble)

  #figures and tables
  if (!require(ggplot2)){install.packages("ggplot2")}; library(ggplot2)
  if (!require(cowplot)){install.packages("cowplot")}; library(cowplot)
  if (!require(pander)){install.packages("pander")}; library(pander)
  
  #stats
  if (!require(hilldiv)){install.packages("hilldiv")}; library(hilldiv)
  if (!require(emmeans)){install.packages("emmeans")}; library(emmeans)
  if (!require(multcomp)){install.packages("multcomp")}; library(multcomp)
  if (!require(ALDEx2)){BiocManager::install("ALDEx2")}; library(ALDEx2)
  if (!require(ANCOMBC)){BiocManager::install("ANCOMBC")}; library(ANCOMBC)
  if (!require(Maaslin2)){BiocManager::install("Maaslin2")}; library(Maaslin2)
  if (!require(metagenomeSeq)){BiocManager::install("metagenomeSeq")}; library(metagenomeSeq)
  if (!require(parameters)){install.packages("parameters")}; library(parameters)
  }
