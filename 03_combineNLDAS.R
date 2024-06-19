###########################################################
### Downloading NLDAS2 data for meteorological hourly forcing
### http://ldas.gsfc.nasa.gov/nldas/NLDAS2forcing.php
### Author: Hilary Dugan (2019-09-30) & Bennett McAfee (2024-06-17)
###########################################################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(RCurl)
library(lubridate)
library(raster)
library(ncdf4)
library(httr)
library(curl)
library(sf)
library(stringr)
library(lubridate)


LakeName = "Trout_Lake"

dir_list <- list.dirs(path = paste0(getwd(), "/", LakeName, "_sorted"), recursive = FALSE)

for (directory in dir_list){
  files <- list.files(path = directory)
  
  start_date_full <- str_extract(files[1], "(?<=NLDAS_FORA0125_H\\.A)\\d{8}\\.\\d{4}")
  date_part <- str_sub(start_date_full, 1, 8)
  time_part <- str_sub(start_date_full, 10, 13)
  start_date_datetime <- sprintf("%s-%s-%s %s:%s:00",
                                str_sub(date_part, 1, 4),
                                str_sub(date_part, 5, 6),
                                str_sub(date_part, 7, 8),
                                str_sub(time_part, 1, 2),
                                str_sub(time_part, 3, 4))
  
  end_date_full <- str_extract(files[length(files)], "(?<=NLDAS_FORA0125_H\\.A)\\d{8}\\.\\d{4}")
  date_part <- str_sub(end_date_full, 1, 8)
  time_part <- str_sub(end_date_full, 10, 13)
  end_date_datetime <- sprintf("%s-%s-%s %s:%s:00",
                                 str_sub(date_part, 1, 4),
                                 str_sub(date_part, 5, 6),
                                 str_sub(date_part, 7, 8),
                                 str_sub(time_part, 1, 2),
                                 str_sub(time_part, 3, 4))

  ###########################################################
  ### Set timeframe
  ###########################################################
  
  
  out = seq.POSIXt(as.POSIXct(start_date_datetime,tz = 'GMT'),as.POSIXct(end_date_datetime,tz='GMT'),by = 'hour')
  vars = c('PEVAPsfc_110_SFC_acc1h', 'DLWRFsfc_110_SFC', 'DSWRFsfc_110_SFC', 'CAPE180_0mb_110_SPDY',
           'CONVfracsfc_110_SFC_acc1h', 'APCPsfc_110_SFC_acc1h', 'SPFH2m_110_HTGL',
           'VGRD10m_110_HTGL', 'UGRD10m_110_HTGL', 'TMP2m_110_HTGL', 'PRESsfc_110_SFC')
  
  vars <- c('TMP', 'SPFH', 'PRES', 'UGRD', 'VGRD', 'DLWRF', 'CONVfrac', 'CAPE', 'PEVAP', 'APCP', 'DSWRF')
  vars <- c('PEVAP', 'DLWRF', 'DSWRF', 'CAPE', 'CONVfrac', 'APCP', 'SPFH', 'VGRD', 'UGRD', 'TMP','PRES')#,'SPFH')
  
  # Create output list of tables
  output = list()
  
  ###########################################################
  ### Need to know how many cells your lake falls within
  ### Can download one instance of data and see how many columns there are
  ###########################################################
  cellNum = 6 #How many output cells will there be? Need to check this beforehand
  for (l in 1:11){
    colClasses = c("POSIXct", rep("numeric",cellNum))
    col.names = c('dateTime',rep(vars[l],cellNum))
    output[[l]] = read.table(text = "",colClasses = colClasses,col.names = col.names)
    attributes(output[[l]]$dateTime)$tzone = 'GMT'
  }
  
  
  ###########################################################
  ### Run hourly loop
  ###########################################################
  # Start the clock!
  ptm <- proc.time()
  
  files <- list.files(path = directory)
  
  for (i in 1:length(files)) {
    #print(out[i])
    filename <- files[i]
    datetime <- make_datetime(year = as.numeric(str_sub(filename, 19, 22)), 
                              month = as.numeric(str_sub(filename, 23, 24)), 
                              day = as.numeric(str_sub(filename, 25, 26)), 
                              hour = as.numeric(str_sub(filename, 28, 29)), 
                              min = 00, 
                              sec = 0, 
                              tz = "GMT")
    cat("\r", filename)
    # yearOut = year(out[i])
    # monthOut = format(out[i], "%m")
    # dayOut = format(out[i], "%d")
    # hourOut = format(out[i], "%H%M")
    # doyOut = format(out[i],'%j')
    
    #filename = format(out[i], "%Y%m%d%H%M")
    filename2 <- paste0(directory, "/", filename)
    
    for (v in 1:11) {
      # id <- nc_open(paste(output_folder, filename,'.nc',sep=''), readunlim=TRUE)
      if (filename != "README.NLDAS2.pdf"){
        id <- nc_open(filename2)
        meteoVal <- ncvar_get(id, vars[v])
        
        # br = brick(paste('~/Documents/NLDAS2/MendotaRawData/',filename,'.nc',sep=''),varname = vars[v])
        output[[v]][i,1] = datetime
        # output[[v]][i,-1] = getValues(br[[1]])
        output[[v]][i,-1] = meteoVal
        
        nc_close(id)
      }
    }
    # rm(br)
    
  }
  # Stop the clock
  proc.time() - ptm
  
  ###########################################################
  ### Save all 11 variables from the output list
  ###########################################################
  dir.create(paste0(directory,'/Final'), showWarnings = FALSE)
  for (f in 1:11){
    write.csv(output[[f]],paste0(directory,'/Final/',LakeName,"_",vars[f],'.csv'),row.names=F)
  }
  rm(output)
  gc()
}