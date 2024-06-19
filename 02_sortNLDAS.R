library(tidyverse)
library(stringr)

LakeName <- "Trout_Lake"

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

path <- paste0(getwd(), "/output")
files <- list.files(path = path)



for (file in files){
  if (file != "README.NLDAS2.pdf"){
    from_name <- paste0(getwd(), "/output/", file)
    
    file_year <- as.numeric(str_extract(file, "(?<=NLDAS_FORA0125_H\\.A)\\d{4}"))
    
    if (file_year <= 1985){
      directory <- "pre_1985"
    }else if (file_year > 1985 & file_year <= 1990){
      directory <- "1985_1990"
    }else if (file_year > 1990 & file_year <= 1995){
      directory <- "1990_1995"
    }else if (file_year > 1995 & file_year <= 2000){
      directory <- "1995_2000"
    }else if (file_year > 2000 & file_year <= 2005){
      directory <- "2000_2005"
    }else if (file_year > 2005 & file_year <= 2010){
      directory <- "2005_2010"
    }else if (file_year > 2010 & file_year <= 2015){
      directory <- "2010_2015"
    }else if (file_year > 2015 & file_year <= 2020){
      directory <- "2015_2020"
    }else{
      directory <- "post_2020"
    }
    
    to_name <- paste0(getwd(), "/", LakeName,"_sorted/",directory, "/", file)
    
    my.file.rename(from = from_name,
                   to = to_name)
    
    cat("\r", "Sorted ", file, " into ", to_name)
    
  }
}
