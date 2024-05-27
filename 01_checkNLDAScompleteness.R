library(tidyverse)
library(stringr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Adjust the file name to your downloaded file ###
linkslist <- readLines("subset_NLDAS_FORA0125_H_002_20240527_154035_.txt")
###

fileslist <- list.files(paste0(getwd(),"/output"))
linkslist <- linkslist[-1]

badlinks <- c()

tally <- 0

for (link in linkslist){
  filefromlink <- str_extract_all(link, paste0("NLDAS_FORA0125_H.A", "(.*?)", ".grb"), simplify = TRUE)[1]
  filefromlink <- paste0(filefromlink, ".SUB.nc4")
  if (!(filefromlink %in% fileslist)){
    badlinks <- append(badlinks, link)
  }
  tally <- tally + 1
  cat("\r", length(badlinks), "found in", tally, "checked", filefromlink)
}

if (length(badlinks) == 0){
  print("No missing files found. Congratulations! Move to 02_combineNLDAS.R")
} else {
  writeLines(badlinks, "todownload.txt")
  print("Missing files found and listed in todownload.txt")
}