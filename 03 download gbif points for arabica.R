


# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl, rgbif, outliers)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

wrld <- ne_countries(returnclass = 'sf', scale = 50)
spce <- 'Coffea arabica'

# To download  ------------------------------------------------------------

## Points
pnts <- occ_search(scientificName = spce, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
pnts <- pnts$data
colnames(pnts)
pnts <- pnts %>% dplyr::select(scientificName, decimalLongitude, decimalLatitude)

## Altitude 
srtm <- elevation_global(res = 2.5, path = './tmpr')

# Extract the srtm values for the points ----------------------------------
vles.srtm <- cbind(pnts, terra::extract(srtm, pnts[,c(2, 3)]))
vles.srtm <- as_tibble(vles.srtm)
vles.srtm <- dplyr::select(vles.srtm, scientificName, decimalLongitude, decimalLatitude, srtm = wc2.1_2.5m_elev)

write.csv(vles.srtm, './tbl/points/gbif_arabica_srtm.csv', row.names = FALSE)

# To make the map  --------------------------------------------------------
gpnt <- ggplot() +
  geom_sf(data = wrld, fill = NA, col = 'grey30') + 
  geom_point(data = vles.srtm, aes(x = decimalLongitude, y = decimalLatitude, col = srtm), size = 0.7) +
  scale_color_gradientn(colours = terrain.colors(n = 10), breaks = seq(0, 3000, 500), labels = seq(0, 3000, 500), name = 'Altitude (m.a.s.l.)') +
  coord_sf() + 
  ggtitle(label = 'Coffea arabica - Source: GBIF', 
          subtitle = paste0('n = ', nrow(pnts))) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 15), 
    legend.position = 'bottom', 
    legend.key.width = unit(3, 'line'), 
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
ggsave(plot = gpnt, filename = './png/maps/points-gbif_altitude.jpg', units = 'in', width = 12, height = 8, dpi = 300)


# Extract the tmin values -------------------------------------------------
tmin <- rast('./tif/tc/tmin_avrg.tif')
vles.srtm <- cbind(vles.srtm, terra::extract(tmin, vles.srtm[,c('decimalLongitude', 'decimalLatitude')]))
vles.srtm <- as_tibble(vles.srtm)

nrow(vles.srtm) # n = 500
vles.srtm <- drop_na(vles.srtm) # 497

dfrm <- vles.srtm[,6:ncol(vles.srtm)]
dfrm <- as.data.frame(dfrm)

## Outliers analysis
norm <- scores(dfrm, type = 'z')
norm_na <- norm
norm_na[abs(norm_na) > 3.5] <- NA
normpoints <- cbind(vles.srtm[,c('decimalLongitude', 'decimalLatitude')], norm_na) # n = 497
rmve <- normpoints[!complete.cases(normpoints),]
normpoints <- drop_na(normpoints) # n = 494
dfrm <- normpoints[,c('decimalLongitude', 'decimalLatitude')]

## Join removed and not removed into only one
dfrm <- as_tibble(dfrm)
rmve <- dplyr::select(rmve, decimalLongitude, decimalLatitude)
rmve <- mutate(rmve, type = 'removed')
dfrm <- mutate(dfrm, type = 'cleaned')
dfrm <- as_tibble(rbind(rmve, dfrm))

write.csv(dfrm, './tbl/points/gbif_arabica_cleaned.csv', row.names = FALSE)

# To make the map again  --------------------------------------------------
dfrm <- mutate(dfrm, type = factor(type, levels = c('removed', 'cleaned')))

gpnt.cln <- ggplot() +
  geom_sf(data = wrld, fill = NA, col = 'grey30') + 
  geom_point(data = dfrm, aes(x = decimalLongitude, y = decimalLatitude, col = type), size = 0.7) +
  scale_color_manual(values = c('darkred', 'darkgreen')) +
  coord_sf() + j
  ggtitle(label = 'Coffea arabica - Source: GBIF', 
          subtitle = paste0('n = ', 494)) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 15), 
    legend.position = 'bottom', 
    legend.key.width = unit(3, 'line'), 
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
ggsave(plot = gpnt.cln, filename = './png/maps/points-gbif_cleaned.jpg', units = 'in', width = 12, height = 8, dpi = 300)
