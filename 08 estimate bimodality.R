


# Load libraries ----------------------------------------------------------
require(pacman)
p_load(mousetrap, terra, diptest, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Points
vles <- read_csv('./tbl/points/values_tc/vles_current-2c_v2.csv')
crds <- vles %>% dplyr::select(id, source, Longitude, Latitude) %>% distinct()

## Current raster 
fles.crnt <- as.character(dir_ls('./tif/tc'))
prec.crnt <- rast(grep('prec', fles.crnt, value = T))
tmin.crnt <- rast(grep('tmin', fles.crnt, value = T))
tmax.crnt <- rast(grep('tmax', fles.crnt, value = T))

## Future raster 
fles.ftre <- as.character(dir_ls('./tif/tc_2c'))
prec.ftre <- rast(grep('ppt', fles.ftre, value = T))

### 
prec.ftre <- map(.x = 1:12, .f = function(i){
  
  cat('To process: ', i, '\n')
  ppt <- prec.ftre[[grep(paste0('_', i, '$'), names(prec.ftre), value = F)]]
  ppt <- mean(ppt)
  names(ppt) <- glue('prec_{i}')
  return(ppt)
  
})

prec.ftre <- reduce(prec.ftre, c)

# Extract current values --------------------------------------------------
prec.crnt <- as_tibble(dplyr::select(cbind(crds, terra::extract(prec.crnt, crds[,c('Longitude', 'Latitude')])), -ID))

## To calculate bimodal ---------------------------------------------------

##
calc.bmdl <- function(tb, gid){
  
  tb <- prec.crnt
  gid <- pull(prec.crnt, id)[1]
  
  ## Filtering
  cat('To process: ', gid, '\n')
  df <- filter(tb, id == gid)
  df <- df %>% gather(var, value, -c(id, source, Longitude, Latitude)) %>% separate(data = ., col = 'var', into = c('variable', 'month'))
  df <- df %>% mutate(month = as.numeric(month))
  
  mn <- df %>% top_n(x = ., n = -1, wt = value) %>% pull(month)
  vc <- df %>% pull(value) %>% as.numeric()
  pr <- c(vc[mn:length(vc)], vc[1:(mn-1)])
  
  ## To calculate bimodality coefficient
  ix <- bimodality_coefficient(pr)
  ix
  bimodality_coefficient(vc)
  
  ## Diptest
  dp <- dip.test(x = pr)
  dp$statistic
  tb <- tibble(id = gid, bimodal = ix, dpi = dp$statistic, pvalue = dp$p.value)
  
  ## Return
  cat('Finish!\n')
  return(tb)
  
}

##
# gids <- prec.crnt %>% pull(id) %>% .[1:34]
gids <- prec.crnt %>% pull(id)
ixcr <- map(.x = gids, .f = function(i){calc.bmdl(tb = prec.crnt, gid = i)})
ixcr <- bind_rows(ixcr)
ixcr <- mutate(ixcr, period = 'Current')

# Extract future values ---------------------------------------------------

prec.ftre <- as_tibble(dplyr::select(cbind(crds, terra::extract(prec.ftre, crds[,c('Longitude', 'Latitude')])), -ID))

##
ixft <- map(.x = gids, .f = function(i){calc.bmdl(tb = prec.ftre, gid = i)})
ixft <- bind_rows(ixft)
ixft <- mutate(ixft, period = '2C')

# Join current and future table into only one  ----------------------------
indx <- rbind(ixcr, ixft)

# Join with the main table  -----------------------------------------------
fnal <- inner_join(vles, indx, by = c('id', 'period'))
fnal %>% dplyr::select(bimodal, dpi, pvalue)
hist(fnal$pvalue)
write.csv(fnal, './tbl/points/values_tc/vles_current-2c_v4.csv', row.names = FALSE)


fnal
fnal %>% filter(id == gids[1]) %>% dplyr::select(period, id, source, bimodal, dpi, pvalue)


