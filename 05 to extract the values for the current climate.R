


# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Vector data --------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)

## Raster -------------------------------------------
fles <- as.character(dir_ls('./tif/tc', regexp = '.tif$'))

### Individual climate 
prec <- rast(grep('prec', fles, value = T))
tmin <- rast(grep('tmin', fles, value = T))
tmax <- rast(grep('tmax', fles, value = T))
etps <- rast(grep('etps_', fles, value = T))

## Bios climate
bios.clma <- rast(grep('bios_', fles, value = T))
bios.clma <- crop(bios.clma, wrld) %>% terra::mask(., wrld)
bios.clma <- crop(bios.clma, ext(bios.etps))

## Tabular data --------------------------------------
pnts.1 <- read_csv('./tbl/points/gbif_arabica_cleaned.csv', show_col_types = FALSE)
pnts.1 <- filter(pnts.1, type == 'cleaned')
pnts.1 <- dplyr::select(pnts.1, Longitude = decimalLongitude, Latitude = decimalLatitude)
pnts.1 <- mutate(pnts.1, source = 'GBIF')

pnts.2 <- read_csv('//catalogue/workspace-cluster9/2024/WCR/V7_worldclim_trials/tble/points/CoffeePoints_2.csv', show_col_types = FALSE)
pnts.2 <- dplyr::select(pnts.2, Longitude, Latitude)
pnts.2 <- mutate(pnts.2, source = 'Bunn')

pnts.3 <- read_excel('//catalogue/workspace-cluster9/2024/WCR/V1/r/tbl/points/IMLVT Site info.xlsx')
pnts.3 <- dplyr::select(pnts.3, id, Longitude, Latitude)
pnts.3 <- mutate(pnts.3, source = 'WCR')

## Join all the tables into only one 
pnts.1.2 <- rbind(pnts.1, pnts.2)

## To make a simple map 
g.pnt <- ggplot() +
  geom_sf(data = wrld, fill = NA, col = 'grey40') +
  geom_point(data = pnts.1.2, aes(x = Longitude, y = Latitude, col = source)) +
  scale_color_manual(values = brewer.pal(n = 3, name = 'Set1')) +
  labs(col = 'Source') +
  theme_void() + 
  theme(
    legend.position = 'bottom'
  ) + 
  guides(colour = guide_legend(override.aes = list(size=10)))

ggsave(plot = g.pnt, filename = './png/maps/points_sources.jpg', units = 'in', width = 9, height = 6.5, dpi = 300)  
write.csv(pnts, './tbl/points/bunn_gbif_wcr.csv', row.names = FALSE)

# Removed duplicated cell by 20 km ----------------------------------------
mskr <- geodata::worldclim_global(var = 'prec', res = 10, path = './tmpr')
mskr <- mskr[[1]] * 0 + 1
names(mskr) <- glue('mskr')

cell <- terra::extract(mskr, pnts.1.2[,c('Longitude', 'Latitude')], cell = TRUE)
cell <- cell[,3]
pnts.1.2 <- pnts.1.2 %>% mutate(dupv = duplicated(cell))
pnts.1.2 <- pnts.1.2 %>% filter(dupv == FALSE)
pnts.1.2 <- pnts.1.2 %>% dplyr::select(., -dupv)
pnts.1.2 <- mutate(pnts.1.2, id = 1:nrow(pnts.1.2))

pnts.1.2 <- pnts.1.2 %>% dplyr::select(id, source, Longitude, Latitude)
pnts.3 <- pnts.3 %>% dplyr::select(id, source, Longitude, Latitude)

pnts <- rbind(pnts.3, pnts.1.2)
write.csv(pnts, './tbl/points/bunn_gbif_wcr_cleaned10min.csv', row.names = FALSE)
pnts <- read_csv('./tbl/points/bunn_gbif_wcr_cleaned10min.csv', show_col_types = FALSE)

# To extract the values  --------------------------------------------------
vles.bios <- as_tibble(cbind(pnts, terra::extract(bios.clma, pnts[,c('Longitude', 'Latitude')])))
vles.bios <- dplyr::select(vles.bios, -ID)

vles.clma <- as_tibble(cbind(pnts, terra::extract(c(prec, tmin, tmax), pnts[,c('Longitude', 'Latitude')])))
vles.clma <- dplyr::select(vles.clma, -ID)

names(etps) <- glue('etps_{1:12}')
vles.etps <- as_tibble(cbind(pnts, terra::extract(etps, pnts[,c('Longitude', 'Latitude')])))
vles.etps[is.na(vles.etps)] <- 0
vles.etps <- dplyr::select(vles.etps, -ID)

vles <- list(vles.bios, vles.clma, vles.etps) %>% reduce(., inner_join, by = c('id', 'source', 'Longitude', 'Latitude'))

smmr <- vles %>% 
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

vles <- inner_join(
  vles %>% dplyr::select(-starts_with('prec'), -starts_with('tmin'), -starts_with('tmax'), -starts_with('etps')),
  smmr, 
  by = c('id', 'source', 'Longitude', 'Latitude')
  )

# To calculate the DEF ----------------------------------------------------

## Déficit values
defn <- vles.clma %>% 
  dplyr::select(id, source, Longitude, Latitude, starts_with('etp'), starts_with('prec')) %>%
  inner_join(., vles.etps, by = c('id', 'source', 'Longitude', 'Latitude')) %>% 
  gather(var, value, -c(id, source, Longitude, Latitude)) %>% 
  separate(data = ., col = 'var', into = c('variable', 'month'), sep = '_') %>% 
  mutate(month = as.numeric(month)) %>% 
  spread(variable, value) %>% 
  mutate(defn = prec - etps)

## Seasons
ssns <- tibble(seasons = LETTERS[1:12], month_1 = c(1:12), month_2 = c(2:12, 1), month_3 = c(3:12, 1:2))
gids <- unique(vles$id)

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

## Values
vles <- inner_join(vles, drst, by = c('id', 'source', 'Longitude', 'Latitude'))
vles <- mutate(vles, period = 'Current', .before = 'id')
write.csv(vles, './tbl/points/values_tc/vles_current.csv', row.names = FALSE)


# Join baseline and 2c ----------------------------------------------------
vles <- read_csv('./tbl/points/values_tc/vles_current.csv')
ftre <- read_csv('./tbl/points/values_tc/vles_2c.csv')

vles <- rename(vles, pet = etps)
ftre <- rename(ftre, pet = etps)
vles <- dplyr::select(vles, colnames(ftre))

## Join both tables into only one
alld <- rbind(vles, ftre)
write.csv(alld, './tbl/points/values_tc/vles_current-2c_v2.csv', row.names = FALSE)



