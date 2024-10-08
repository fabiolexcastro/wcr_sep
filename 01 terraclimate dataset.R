

# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
root <- '//catalogue/workspace-cluster9/2024/WCR/V5_analogues_TC/data/tif'
dirs <- as.character(dir_ls(root, type = 'directory'))
wrld <- ne_countries(returnclass = 'sf', scale = 50)
wrld <- vect(wrld)

## Source: 
source('//catalogue/workspace-cluster9/2024/WCR/V5_analogues_TC/bioclimatic functions.R')

# Bios --------------------------------------------------------------------
bios <- as.character(dir_ls(dirs[1], regexp = '.tif$'))
bios <- rast(bios)
nmes <- glue('bio{1:19}')

## Average 
bios.avrg <- map(.x = 1:length(nmes), .f = function(i){
  bio <- bios[[grep(paste0(nmes[i], '$'), names(bios))]]
  bio <- mean(bio)
  names(bio) <- glue(nmes[i])
  return(bio)
})
bios.avrg <- reduce(bios.avrg, c)
names(bios.avrg) <- glue('bioc_{1:19}')

dir_create('./tif/tc')
terra::writeRaster(x = bios.avrg, filename = './tif/tc/bios_avrg.tif', overwrite = TRUE)

# Climate average ---------------------------------------------------------

## To list the files
fles <- as.character(dir_ls(dirs))
prec <- grep('ppt', fles, value = T)
tmin <- grep('tmi', fles, value = T)
tmax <- grep('tma', fles, value = T)

## To read as a raster file
prec <- rast(prec)
tmin <- rast(tmin)
tmax <- rast(tmax)

## To make a loop
stck <- map(.x = 1:12, .f = function(i){
  
  ## To start 
  cat('To process: ', month.abb[i], '\n')
  
  ## To filter the month
  ppt <- prec[[grep(paste0('_', i, '$'), names(prec))]]
  ppt <- mean(ppt)
  
  tmn <- tmin[[grep(paste0('_', i, '$'), names(tmin))]]
  tmn <- mean(tmn)
  
  tmx <- tmax[[grep(paste0('_', i, '$'), names(tmax))]]
  tmx <- mean(tmx)
  
  ## To change the names
  names(ppt) <- glue('prec_{i}')
  names(tmn) <- glue('tmin_{i}')
  names(tmx) <- glue('tmax_{i}')
  
  ## To return the files 
  stk <- c(ppt, tmn, tmx)
  cat('Done!\n')
  return(stk)
  
})

## To make just one stack
stck <- reduce(stck, c)

## To extract as stack individuals
prec <- stck[[grep('prec', names(stck))]]
tmin <- stck[[grep('tmin', names(stck))]]
tmax <- stck[[grep('tmax', names(stck))]]

## To write the raster 
terra::writeRaster(x = prec, filename = glue('./tif/tc/prec_avrg.tif'), overwrite = TRUE)
terra::writeRaster(x = tmin, filename = glue('./tif/tc/tmin_avrg.tif'), overwrite = TRUE)
terra::writeRaster(x = tmax, filename = glue('./tif/tc/tmax_avrg.tif'), overwrite = TRUE)

# To calculate ETP  -------------------------------------------------------

## Solar radiation
srad <- '//catalogue/workspace-cluster9/DATA/ET_SolRad'
srad <- dir_ls(srad, regexp = 'et_solrad_') %>% as.character() %>% mixedsort()
srad <- rast(srad)
names(srad) <- glue('srad_{1:12}')

## To calculate tavg
tavg <- (tmax + tmin) / 2
names(tavg) <- glue('tavg_{1:12}')

## Resampling srad
srad <- terra::resample(srad, tmax, method = 'bilinear')

# To calculate the ETP  ---------------------------------------------------
etps <- 0.0013 * 0.408 * srad * (tavg + 17) * (tmax - tmin - 0.0123 * prec) ^ 0.76
etps <- etps * c(31,29,31,30,31,30,31,31,30,31,30,31)
etps <- map(.x = 1:nlyr(etps), .f = function(i){
  etp <- etps[[i]]
  etp[etp < 0] <- 0
  return(etp)
})
etps <- reduce(etps, c)
etps <- terra::crop(etps, wrld) %>% terra::mask(., wrld)
names(etps) <- glue('etps_{c(paste0(0, 1:9), 10:12)}')
terra::writeRaster(x = etps, filename = glue('./tif/tc/etps_avrg.tif'), overwrite = TRUE)
etps <- terra::rast('./tif/tc/etps_avrg.tif')

# To extract by mask ------------------------------------------------------
prec <- terra::crop(prec, wrld) %>% terra::mask(., wrld)
tmin <- terra::crop(tmin, wrld) %>% terra::mask(., wrld)
tmax <- terra::crop(tmax, wrld) %>% terra::mask(., wrld)
etps <- terra::crop(etps, wrld) %>% terra::mask(., wrld)

# Resample ----------------------------------------------------------------
prec <- terra::resample(prec, tavg, method = 'bilinear')
etps <- terra::resample(etps, tavg, method = 'bilinear')
tmax <- terra::resample(tmax, tavg, method = 'bilinear')
tmin <- terra::resample(tmin, tavg, method = 'bilinear')

## To create the table as a matrix ----------------------------------------
stk <- c(etps, prec, tavg)
dfm <- as.data.frame(stk, xy = T)
dfm <- drop_na(dfm)
trr <- rast(dfm)
mtx <- as.matrix(trr)
etb <- t(apply(mtx, 1, etpvars))
zro <- ppt[[1]]

etb.rstr <- map(1:ncol(etb), function(i){
  cat(i, '\n')
  lyr <- zro
  values(lyr) <- etb[,i]
  names(lyr) <- glue('bioc_{20 + i}')
  return(lyr)
}) %>% reduce(., c)
plot(etb.rstr)
etb.rstr
terra::writeRaster(x = etb.rstr, filename = glue('./tif/tc/bioc-etp.tif'), overwrite = T)

# To make the stack  ------------------------------------------------------
bios.avrg
etb.rstr <- terra::crop(etb.rstr, ext(bios.avrg))
stck <- c(bios.avrg, etb.rstr)
terra::writeRaster(x = stck, filename = './tif/tc/bios-etps.tif', overwrite = TRUE)

# To calculate budget -----------------------------------------------------
defn <- prec - etps
names(defn) <- glue('defn_{1:12}')
terra::writeRaster(x = defn, filename = './tif/tc/defn.tif')


# VPD ---------------------------------------------------------------------
vpds <- as.character(dir_ls(dirs[2])) %>% grep('vpd', ., value = T)
vpds <- rast(vpds)
nmes <- unique(names(vpds))

vpds.avg <- map(.x = 1:12, .f = function(i){
  nme <- nmes[i]
  nme <- paste0('', nme, '$')
  vpd <- vpds[[grep(nme, names(vpds))]]
  vpd <- mean(vpd)
  names(vpd) <- glue("vpd_{i}")
  return(vpd)  
})
vpds.avg <- reduce(vpds.avg, c)
terra::writeRaster(x = vpds.avg, filename = './tif/tc/vpds_avrg.tif', overwrite = TRUE)
 
dir_ls(dirs[2])

# PET ---------------------------------------------------------------------
pets <- as.character(dir_ls(dirs[2])) %>% grep('pet', ., value = T)
pets <- rast(pets)
nmes <- unique(names(pets))

pets.avg <- map(.x = 1:12, .f = function(i){
  nme <- nmes[i]
  nme <- paste0('', nme, '$')
  pet <- pets[[grep(nme, names(pets))]]
  pet <- mean(pet)
  names(pet) <- glue("pet_{i}")
  return(pet)  
})
pets.avg <- reduce(pets.avg, c)
terra::writeRaster(x = pets.avg, filename = './tif/tc/pets_avrg.tif', overwrite = TRUE)



