###########################################################
### Downloading NLDAS2 data for meteorological hourly forcing
### http://ldas.gsfc.nasa.gov/nldas/NLDAS2forcing.php
### Author: Hilary Dugan hilarydugan@gmail.com
### Date: 2019-09-30
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


###########################################################
### Set timeframe
###########################################################
LakeName = "Trout_Lake"

out = seq.POSIXt(as.POSIXct('2024-01-01 00:00:00',tz = 'GMT'),as.POSIXct('2024-01-06 23:00:00',tz='GMT'),by = 'hour')
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

path <- paste0(getwd(), "/output")
files <- list.files(path = path)

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
  filename2 <- paste0(path, "/", filename)

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
dir.create(paste0(LakeName,'_Final'), showWarnings = FALSE)
for (f in 1:11){
  write.csv(output[[f]],paste0(LakeName,'_Final/',LakeName,"_",vars[f],'.csv'),row.names=F)
}
