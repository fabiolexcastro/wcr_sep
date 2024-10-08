
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, dismo, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Download future terraclimate --------------------------------------------
root <- 'http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data_plus2C'

# To process the dataset --------------------------------------------------
fles <- as.character(dir_ls('./tif/tc_2c'))
prec <- rast(grep('ppt', fles, value = T))
tmin <- rast(grep('tmin', fles, value = T))
tmax <- rast(grep('tmax', fles, value = T))
pets <- rast(grep('pet', fles, value = T))
vpds <- rast(grep('vpd', fles, value = T))

# tmin <- tmin * 1; tmax <- tmax * 1; prec <- prec * 1; pets <- pets * 1; vpds <- vpds * 1

## To calculate the average
avrg <- map(.x = 1:12, .f = function(i){
  
  ## Filtering
  cat('To process: ', i, '\n')
  mnt <- paste0('_', i, '$')
  
  ## To calculate the average
  tmn <- mean(tmin[[grep(mnt, names(tmin), value = F)]])
  tmx <- mean(tmax[[grep(mnt, names(tmax), value = F)]])
  ppt <- mean(prec[[grep(mnt, names(prec), value = F)]])
  pet <- mean(pets[[grep(mnt, names(pets), value = F)]])
  vpd <- mean(vpds[[grep(mnt, names(vpds), value = F)]])
  
  ## To change the names 
  names(tmn) <- glue('tmin_{i}')
  names(tmx) <- glue('tmax_{i}')
  names(ppt) <- glue('prec_{i}')
  names(pet) <- glue('etps_{i}')
  names(vpd) <- glue('vpd_{i}')
  
  ## To write the raster
  dout <- paste0('./tif/tc_2c/avg_2007-2015')
  terra::writeRaster(x = tmn, filename = glue('{dout}/tmin_{i}.tif'), overwrite = TRUE)
  terra::writeRaster(x = tmx, filename = glue('{dout}/tmax_{i}.tif'), overwrite = TRUE)
  terra::writeRaster(x = ppt, filename = glue('{dout}/prec_{i}.tif'), overwrite = TRUE)
  terra::writeRaster(x = pet, filename = glue('{dout}/pets_{i}.tif'), overwrite = TRUE)
  terra::writeRaster(x = vpd, filename = glue('{dout}/vpds_{i}.tif'), overwrite = TRUE)
  
  ## To remove the object and finish the process 
  rm(tmn, tmx, ppt, pet, vpd)
  gc(reset = TRUE)
  cat('Done!\n')

})

## To list the results for the average
fles <- dir_ls('./tif/tc_2c/avg_2007-2015', regexp = '.tif$')
fles <- as.character(fles)
fles <- gtools::mixedsort(fles)
prec <- rast(grep('prec', fles, value = T))
tmin <- rast(grep('tmin', fles, value = T))
tmax <- rast(grep('tmax', fles, value = T))
pets <- rast(grep('pets', fles, value = T))
vpds <- rast(grep('vpds', fles, value = T))

## Points 
pnts <- read_csv('./tbl/points/bunn_gbif_wcr_cleaned10min.csv', show_col_types = FALSE)

# To make bioclimatic  ----------------------------------------------------
prec.mtrx <- terra::as.matrix(prec)
tmin.mtrx <- terra::as.matrix(tmin)
tmax.mtrx <- terra::as.matrix(tmax)

bioc.mtrx <- dismo::biovars(prec = prec.mtrx, tmin = tmin.mtrx, tmax = tmax.mtrx)
nrow(bioc.mtrx)
crds <- as.data.frame(prec, xy = T, na.rm = FALSE)
bioc.dfrm <- cbind(crds[,1:2], bioc.mtrx)

## Table to raster 
bioc.rstr <- terra::rast(bioc.dfrm, type = 'xyz')
names(bioc.rstr) <- glue('bioc_{1:19}')
bioc.rstr

## To write the raster 
terra::writeRaster(x = bioc.rstr, filename = './tif/tc_2c/avg_2007-2015/bios_2c.tif', overwrite = TRUE)

# To make the stack -------------------------------------------------------
names(prec) <- glue('prec_{1:12}')
clma <- c(prec, tmin, tmax)
bioc.rstr

# To extract the values  --------------------------------------------------
pnts.vles <- dplyr::select(as_tibble(cbind(pnts, terra::extract(bioc.rstr, pnts[,c('Longitude', 'Latitude')]))), -ID)
pnts.clma <- dplyr::select(as_tibble(cbind(pnts, terra::extract(clma, pnts.vles[,c('Longitude', 'Latitude')]))), -ID)
pnts.etps <- dplyr::select(as_tibble(cbind(pnts, terra::extract(pets, pnts.vles[,c('Longitude', 'Latitude')]))), -ID)
pnts.clma <- inner_join(pnts.clma, pnts.etps, by = c('id', 'source', 'Longitude', 'Latitude'))

## Get the summarise
smmr <- pnts.clma %>% 
  dplyr::select(id, source, Longitude, Latitude, starts_with('prec'), starts_with('tmin'), starts_with('tmax'), starts_with('etps')) %>% 
  gather(var, value, -c(id, source, Longitude, Latitude)) %>% 
  separate(data = ., col = 'var', into = c('variable', 'month'), sep = '_') %>% 
  spread(variable, value) %>% 
  group_by(id, source, Longitude, Latitude) %>% 
  dplyr::summarise(
    prec = sum(prec), 
    tmax = mean(tmax), 
    tmin = mean(tmin), 
    etps = mean(etps)
  ) %>% 
  ungroup()

## Seasons
ssns <- tibble(seasons = LETTERS[1:12], month_1 = c(1:12), month_2 = c(2:12, 1), month_3 = c(3:12, 1:2))
gids <- unique(pnts.clma$id)

## Déficit values
defn <- pnts.clma %>% 
  dplyr::select(id, source, Longitude, Latitude, starts_with('etps'), starts_with('prec')) %>%
  gather(var, value, -c(id, source, Longitude, Latitude)) %>% 
  separate(data = ., col = 'var', into = c('variable', 'month'), sep = '_') %>% 
  mutate(month = as.numeric(month)) %>% 
  spread(variable, value) %>% 
  mutate(defn = prec - etps)

# Function to calculate driest quarter by each coffee presence
extr.drst <- function(gid){
  
  ## Filtering
  cat('To process: ', gid, '\n')
  ppt <- defn %>% filter(id == gid)
  
  ## Accumulate by each month 
  drs <- map_dfr(.x = 1:12, .f = function(s){
    ssn <- ssns[s,] %>% dplyr::select(2:4) %>% as.numeric()
    rsl <- ppt %>% filter(month %in% as.character(ssn))
    rsl <- tibble(id = gid, month_1 = ssn[1], month_2 = ssn[2], month_3 = ssn[3], prec_ssn = sum(pull(rsl, prec)))
    return(rsl)
  }) %>% top_n(x = ., n = -1, wt = prec_ssn) %>% .[1,]
  
  dfr.mnt <- drs %>% dplyr::select(2:4) %>% as.numeric()
  
  rsl <- ppt %>% 
    filter(month %in% as.character(dfr.mnt)) %>% 
    group_by(source, id, Longitude, Latitude) %>%
    dplyr::summarise(def_driest = sum(defn, na.rm = T)) %>% ungroup()
  
  return(rsl)
  
}

## To apply the function 
drst <- map(gids, extr.drst)
drst <- bind_rows(drst)

pnts.vles <- inner_join(pnts.vles, drst, by = c('id', 'source', 'Longitude', 'Latitude'))
pnts.vles <- inner_join(pnts.vles, smmr, by = c('id', 'source', 'Longitude', 'Latitude'))
pnts.vles <- mutate(pnts.vles, period = '2C', .before = 'id')

write.csv(pnts.vles, './tbl/points/values_tc/vles_2c.csv', row.names = FALSE)

  

