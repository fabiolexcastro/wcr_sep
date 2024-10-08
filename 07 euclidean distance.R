


# Load libraries ----------------------------------------------------------
require(pacman)
p_load(terra, fs, sf, tidyverse, glue, ggspatial, raster, climateR, gtools, geodata, rnaturalearthdata, rnaturalearth, readxl)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Read the table
vles <- read_csv('./tbl/points/values_tc/vles_current-2c_v2.csv', show_col_types = FALSE)

## Filtering current
vles <- filter(vles, period == 'Current')
unique(vles$period)

# Function ----------------------------------------------------------------
calc.dist <- function(gid){
  
  ## To start the analysis 
  cat('To process: ', gid, '\n')
  mtx <- filter(vles, id != gid) %>% dplyr::select(bioc_1:pet) %>% as.matrix()
  vls.1 <- filter(vles, id == gid) %>% dplyr::select(bioc_1:pet) %>% as.vector() %>% as.numeric()

  ## To calculate the euclidean distance
  euc <- map_dbl(.x = 1:nrow(mtx), .f = function(i){
    
    ## Select the vector
    cat('Pixel: ', i)
    v1 <- vls.1
    v2 <- mtx[i,]
    
    ## Function
    eucl.dist <- function(vect1, vect2) sqrt(sum((vect1 - vect2)^2))
    
    ## To apply the function and return 
    eu <- eucl.dist(v1, v2)
    return(eu)
    
  })
  
  ## Final tibble
  rsl <- tibble(pixel = gid, euclidean = euc)

  ## Return 
  return(rsl)
  
}

# To apply the function ---------------------------------------------------
rwa <- calc.dist(gid = 'Rubona_RWA')
nic <- calc.dist(gid = 'LaCumplida_NIC')

dst <- rbind(rwa, nic)

# To make the boxplot -----------------------------------------------------

gvln <- ggplot(data = dst, aes(x = pixel, y = euclidean)) + 
  geom_violin(fill = 'grey40') + 
  labs(x = 'Point', y = 'Eucliean distance') + 
  theme_bw() + 
  theme(
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10), 
    axis.text.x = element_text(size = 10)
  )

write.csv(dst, './tbl/distances_rubona_lacumplida.csv', row.names = FALSE)
ggsave(plot = gvln, filename = './png/graphs/violin_dist-euc.jpg', units = 'in', width = 7, height = 12, dpi = 300)

dst %>% 
  group_by(pixel) %>% 
  dplyr::summarise(euclidean = mean(euclidean)) %>% 
  ungroup()
