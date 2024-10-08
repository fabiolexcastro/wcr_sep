
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, climateR, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
root <- 'https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly'
vars <- c('tas', 'tasmin', 'tasmax', 'pet', 'pr', 'vpd')
dout <- glue('./tif/ch/bsl'); dir_create(dout)

# Function ----------------------------------------------------------------
to.down <- function(var){
  
  # var <- vars[1]
  
  ## To list the files 
  cat('To process: ', var, '\n')
  mnt <- c(glue('0{1:9}'), 10:12)
  
  ## To create the URLs
  nms <- map(.x = 1:12, .f = function(i){glue('{root}/{var}/CHELSA_{var}_{mnt[i]}_{2015:2019}_V.2.1.tif')})
  nms <- unlist(nms)
  
  ## To download
  map(.x = 1:length(nms), .f = function(i){
    
    ## Downloading
    cat('To process: ', nms[i], '\n')
    url <- nms[i]
    out <- glue('{dout}/{basename(url)}')
    download.file(url = url, destfile = out, mode = 'wb')
    cat('Finish!\n')
    
  })
  
}
map(vars[2:length(vars)], to.down)
  