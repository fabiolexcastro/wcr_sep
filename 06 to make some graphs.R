
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
vles <- read_csv('./tbl/points/values_tc/vles_current-2c_v2.csv')
vles
dfrm <- vles %>% gather(var, value, -c(period, id, source, Longitude, Latitude))
dfrm <- mutate(dfrm, period = factor(period, levels = c('Current', '2C')))
dfrm <- mutate(dfrm, var = factor(var, levels = c(paste0('bioc_', 1:19), 'tmin', 'tmax', 'prec', 'pet', 'def_driest')))

# Make graphs comparison --------------------------------------------------
g.box <- ggplot(data = dfrm, aes(x = period, y = value)) +
  geom_boxplot() +
  facet_wrap(~var, scales = 'free_y') +
  labs(x = '', y = 'Value') +
  theme_minimal() + 
  theme(
    strip.text = element_text(face = 'bold'), 
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )

ggsave(plot = g.box, filename = glue('./png/graphs/boxplot_current-2c_v2.jpg'), units = 'in', width = 12, height = 10, dpi = 300)

# To estimate the difference ----------------------------------------------
tble <- vles %>% gather(var, value, -c(period, id, source, Longitude, Latitude))

tble.wcr <- tble %>% filter(source == 'WCR')
tble.wcr <- tble.wcr %>% spread(period, value) %>% mutate(difference = `2C` - Current)

tble.oth <- tble %>% filter(source != 'WCR')
tble.oth <- tble.oth %>% mutate(source = 'Others')
tble.oth <- tble.oth %>% spread(period, value)
tble.oth <- tble.oth %>% mutate(difference = `2C` - Current)
tble.all <- rbind(tble.wcr, tble.oth)

## As factors
tble.all <- mutate(tble.all, var = factor(var, levels = c(paste0('bioc_', 1:19), 'tmin', 'tmax', 'prec', 'pet', 'def_driest')))

## To make the graphs
gbox.dfrn <- ggplot(data = tble.all, aes(x = source, y = difference, fill = source)) +
  geom_boxplot() +
  facet_wrap(~var, scales = 'free_y') +
  labs(x = '', y = 'Value', fill =  'Source') +
  theme_minimal() + 
  theme(
    strip.text = element_text(face = 'bold'), 
    axis.text.y = element_text(angle = 90, hjust = 0.5), 
    legend.position = 'bottom'
  )

ggsave(plot = gbox.dfrn, filename = './png/graphs/boxplot_difference_current-2c_v2.jpg', units = 'in', width = 12, height = 10, dpi = 300)

# To write the tables -----------------------------------------------------
write.csv(dfrm, './tbl/points/values_tc/graphic_current-2c_v2.csv', row.names = FALSE)
write.csv(tble.all, './tbl/points/values_tc/graphic_difference_current-2c_v2.csv', row.names = FALSE)


pnts <- tble.all %>% distinct(id, source, Longitude, Latitude)
pnts <- mutate(pnts, source = factor(source, levels = c('WCR', 'Others')))

## Final map 
gpnt <- ggplot() + 
  geom_point(data = pnts, aes(x = Longitude, y = Latitude, col = source), size = 0.1) + 
  scale_color_manual(values = c('brown', 'darkgreen')) +
  geom_sf(data = ne_countries(returnclass = 'sf', scale = 50), fill = NA, col = 'grey40') +
  coord_sf() + 
  labs(col = 'Source') +
  theme_void() + 
  theme(
    legend.position = 'bottom'
  ) + 
  guides(colour = guide_legend(override.aes = list(size=10)))

ggsave(plot = gpnt, filename = './png/maps/points-final_v2.jpg', units = 'in', width = 13, height = 9, dpi = 300)

pnts

table(pnts$source)
