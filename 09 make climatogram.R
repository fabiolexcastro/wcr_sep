


# Load libraries ----------------------------------------------------------
require(pacman)
p_load(mousetrap, terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)


# Load data ---------------------------------------------------------------

vles <- read_csv('./tbl/points/values_tc/vles_current-2c_v3.csv', show_col_types = FALSE)
crds <- vles %>% filter(period == 'Current') %>% distinct(id, Longitude, Latitude)

## Current rasters
fles.crnt <- as.character(dir_ls('./tif/tc'))
prec.crnt <- rast(grep('prec', fles.crnt, value = T))
tmin.crnt <- rast(grep('tmin', fles.crnt, value = T))
tmax.crnt <- rast(grep('tmax', fles.crnt, value = T))

## Future raster 
fles.ftre <- as.character(dir_ls('./tif/tc_2c'))
prec.ftre <- rast(grep('ppt', fles.ftre, value = T))
tmin.ftre <- rast(grep('tmin', fles.ftre, value = T))
tmax.ftre <- rast(grep('tmax', fles.ftre, value = T))

# Future average ----------------------------------------------------------

##
rstr.avrg <- function(stk){
  
  cat('Start\n')
  fnl <- map(.x = 1:12, .f = function(i){
    
    rst <- stk[[grep(paste0('_', i, '$'), names(stk), value = F)]]
    rst <- mean(rst)
    return(rst)
    
  }) %>% 
    reduce(., c)
  
}

##
prec.ftre <- rstr.avrg(prec.ftre)
names(prec.ftre) <- glue('prec_{1:12}')
tmin.ftre <- rstr.avrg(tmin.ftre)
names(tmin.ftre) <- glue('tmin_{1:12}')
tmax.ftre <- rstr.avrg(tmax.ftre)
names(tmax.ftre) <- glue('tmax_{1:12}')

## 


# To extract the values ---------------------------------------------------

## Current 
prec.crnt.vles <- as_tibble(cbind(crds, terra::extract(prec.crnt, crds[,2:3]))) %>% dplyr::select(-ID)
tmin.crnt.vles <- as_tibble(cbind(crds, terra::extract(tmin.crnt, crds[,2:3]))) %>% dplyr::select(-ID)
tmax.crnt.vles <- as_tibble(cbind(crds, terra::extract(tmax.crnt, crds[,2:3]))) %>% dplyr::select(-ID)

crnt.vles <- list(prec.crnt.vles, tmin.crnt.vles, tmax.crnt.vles) %>% reduce(., inner_join, by = c('id', 'Longitude', 'Latitude'))

## Future
prec.ftre.vles <- as_tibble(cbind(crds, terra::extract(prec.ftre, crds[,2:3]))) %>% dplyr::select(-ID)
tmin.ftre.vles <- as_tibble(cbind(crds, terra::extract(tmin.ftre, crds[,2:3]))) %>% dplyr::select(-ID)
tmax.ftre.vles <- as_tibble(cbind(crds, terra::extract(tmax.ftre, crds[,2:3]))) %>% dplyr::select(-ID)

ftre.vles <- list(prec.ftre.vles, tmin.ftre.vles, tmax.ftre.vles) %>% reduce(., inner_join, by = c('id', 'Longitude', 'Latitude'))

## Join current and future dataset 
crnt.vles <- crnt.vles %>% mutate(Period = 'Current')
ftre.vles <- ftre.vles %>% mutate(Period = 'Future')
vles <- rbind(crnt.vles, ftre.vles)
vles <- mutate(vles, Period = factor(Period, levels = c('Current', 'Future')))
write.csv(vles, './tbl/points/values_tc/values_prec-tmin-tmax_periods.csv', row.names = FALSE)

## Filtering for the WCR points
vles <- vles %>% filter(id %in% pull(vles, id)[1:34])

## Tidy the table 
vles <- vles %>% 
  gather(var, value, -c(id, Longitude, Latitude, Period)) %>% 
  separate(data = ., col = 'var', into = c('Variable', 'Month'), sep = '_') %>% 
  mutate(Month = as.numeric(Month)) %>% 
  inner_join(., tibble(Month = 1:12, month_abb = month.abb), by = 'Month') %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))

vles <- vles %>% 
  spread(Variable, value)

write.csv(vles, './tbl/points/values_tc/values_climatogram.csv', row.names = FALSE)

# To make the graph  ------------------------------------------------------

##
make.graph <- function(gid){
  
  # gid <- unique(vles$id)[1]
  
  
  ## Tidy the table
  vls <- filter(vles, id == gid)
  vls.tas <- vls %>% dplyr::select(-prec) %>% gather(var, value, -c(id, Longitude, Latitude, Period, Month, month_abb))
  vls.tas <- vls.tas %>% mutate(var = factor(var, levels = c('tmin', 'tmax')))
  vls.tas <- vls.tas %>% spread(var, value) %>% mutate(tavg = (tmin + tmax) / 2)
  
  ## Relation between temperature and precipitation
  rlc <- mean(pull(vls, prec)) / mean(pull(vls, tmin))
  rlc <- 15
  
  ## To make the graph
  ggl <- ggplot() + 
    geom_col(data = vls, aes(x = month_abb, y = prec, fill = Period), position = 'dodge') + 
    geom_line(data = vls.tas, aes(x = month_abb, y = tmin * rlc, col = Period, group = Period, linetype = 'A')) +
    geom_line(data = vls.tas, aes(x = month_abb, y = tavg * rlc, col = Period, group = Period, linetype = 'B')) +
    geom_line(data = vls.tas, aes(x = month_abb, y = tmax * rlc, col = Period, group = Period, linetype = 'C')) +
    scale_fill_manual(name = 'Temp. / Prec.',
                       values = c('Current' = '#63BE5C', 'Future' = '#009933'),
                       labels = c('Current' = 'Current', 'Future' = 'Future'),
                       breaks = c('Current', 'Future')) +
    scale_color_manual(name = 'Temperature',
                       values = c('Current' = '#63BE5C', 'Future' = '#009933'),
                       labels = c('Current' = 'Current', 'Future' = 'Future'),
                       breaks = c('Current', 'Future')) +
    scale_linetype_manual(name = ' ', 
                          values = c("A" = 1, 'B' = 2, 'C' = 3), 
                          labels = c("A" = "Min", 'B' = 'Avg', 'C' = 'Max')) +
    scale_y_continuous(sec.axis = sec_axis(~./rlc, name = 'Temperature ºC')) +
    ggtitle(label = gid) +
    labs(x = '', y = 'Precipitation (mm)', caption = 'Source: Terraclimate') +
    theme_minimal() + 
    theme(
      legend.position = 'bottom',
      plot.title = element_text(face = 'bold', hjust = 0.5, size = 18)
    ) +
    guides(linetype = guide_legend(nrow = 2, keywidth = 3, order = 4, title.position = 'top', size = 15),
           color = guide_legend(nrow = 2, keywidth = 3, order = 3, title.position = 'top', size = 15),
           fill = guide_legend(order = 1, title.position = 'top', size = 15),
           size = guide_legend(order = 2, nrow = 2, title.position = 'top', size = 15)) 
  
  
  ## To save the graph
  ggsave(plot = ggl, filename = glue('./png/graphs/climatogram/clima_{gid}.jpg'), units = 'in', width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  
}

## 
vles <- vles %>% mutate(id = gsub('Bulegeni\t\t_UGA', 'Bulegeni_UGA', id))
duplicated(pull(vles, id)[1:34])
map(pull(vles, id)[1:34], make.graph)



