# -----------------------------------------------------------------------------------------------
# Datras plot.r 
#
# R version: 3.5
# 
# Plot simple survey plots
#
# 05/09/2017 First version based on code from Einar
# 13/08/2018 Updated on the basis of Datras doodle website
# -----------------------------------------------------------------------------------------------

# library(devtools)
library(icesDatras)   # install.packages("icesDatras")
library(tidyices)     # devtools::install_github("fishvice/tidyices", dependencies = TRUE)
library(tidyverse)    # tidying packages
library(lubridate)
library(sf)
library(data.table)
# library(gisland)      # devtools::install_github("einarhjorleifsson/gisland", dependencies = TRUE)

library(maps)
library(mapdata)
library(lubridate)
library(viridis)

# source my utils
source("D:/GIT/mptools/R/my_utils.r")

# Data path
datapath <- "F:/DATRAS"

# -----------------------------------------------------------------------------------------------
# Load the tidy datras data
# -----------------------------------------------------------------------------------------------

# names(hh)
# unique(hh$hauldur)

# hh files
list.hh <- list.files(file.path(datapath, "tidy"),
                      pattern = "hh", 
                      full.names=TRUE) 

for (i in 1:length(list.hh)) {
  if (i == 1) {
    hh <- read_rds(path = list.hh[i])
  } else {
    hh <- bind_rows(hh, read_rds(path = list.hh[i])) 
  } 
}

# hh <- 
#   read_rds(path = paste0(datapath, "/tidy/", "ns-ibts_hh.rds")) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "evhoe_hh.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "fr-cgfs_hh.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "ie-igfs_hh.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "scowcgfs_hh.rds"))) 

# hl files
list.hl <- list.files(file.path(datapath, "tidy"),
                      pattern = "hl", 
                      full.names=TRUE) 

for (i in 1:length(list.hl)) {
  if (i == 1) {
    hl <- read_rds(path = list.hl[i])
  } else {
    hl <- bind_rows(hl, read_rds(path = list.hl[i])) 
  } 
}

# hl <- 
#   read_rds(path = paste0(datapath, "/tidy/", "ns-ibts_hl.rds")) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "evhoe_hl.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "fr-cgfs_hl.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "ie-igfs_hl.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "scowcgfs_hl.rds")))

# ca files
list.ca <- list.files(file.path(datapath, "tidy"),
                      pattern = "ca", 
                      full.names=TRUE) 

for (i in 1:length(list.ca)) {
  if (i == 1) {
    ca <- read_rds(path = list.ca[i])
  } else {
    ca <- bind_rows(ca, read_rds(path = list.ca[i])) 
  } 
}

# ca <- 
#   read_rds(path = paste0(datapath, "/tidy/", "ns-ibts_ca.rds")) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "evhoe_ca.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "fr-cgfs_ca.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "ie-igfs_ca.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "scowcgfs_ca.rds")))

# -----------------------------------------------------------------------------------------------
# Now the plots
# -----------------------------------------------------------------------------------------------

# cat(unique(hh$survey))

# set selection criteria
mysurvey  <- c( "BTS-VIII","BTS","DYFS","EVHOE","FR-CGFS","IE-IGFS","NIGFS", "DWS", 
                "NS-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                "SP-PORC","SWC-IBTS")

mysurvey  <- c("NS-IBTS", "EVHOE", "IE-IGFS", "SCOWCGFS", "FR-CGFS", "NIGFS", "SCOROC",
               "SP-ARSA", "SP-NORTH", "SP-PORC", "PT-IBTS", "SWC-IBTS")

mysurvey  <- c("NS-IBTS", "BTS")

myyear    <- 2003:2017

myquarter <- c(1, 3, 4)
myquarter <- c(3)

myspecies <- "Trachurus trachurus"
myspecies <- "Chelidonichthys cuculus"
myspecies <- "Mullus surmuletus"
myspecies <- "Pleuronectes platessa"
myspecies <- "Solea solea"
myspecies <- "Gadus morhua"

mylength  <- c(30,50)

# sort(unique(hl$latin))

# numbers at length
le <- 
  hl %>%
  dplyr::filter(survey  %in% mysurvey, latin %in% myspecies) %>% 
  group_by(survey, id, latin, length) %>% 
  summarise(n = n())

# stations
st <- 
  hh %>%
  filter(survey  %in% mysurvey, year %in% myyear, quarter %in% myquarter) %>% 
  select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat) 

xlim <- range(st$lon)
ylim <- range(st$lat)

m    <- map_data("worldHires", xlim = xlim, ylim = ylim)

# plot map
map <-
  m %>% 
  ggplot() +
  theme_bw() +
  geom_polygon(data = m, aes(long, lat, group = group), fill = "grey") +
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_x_continuous(NULL, NULL) +
  scale_y_continuous(NULL, NULL)


# onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 
# load(file.path(onedrive, "rdata/haul.RData"))


# map all surveys
# map +
#   theme_publication() +
#   # geom_point(data = filter(haul, shootlat > 40, shootlat < 62, year >= 2015), 
#   #            aes(x=shootlon, y=shootlat, size=catch), colour = "blue", alpha = 0.2) +
#   geom_point(data = st, aes(lon, lat, group=survey, colour=survey), size=0.1, alpha = 0.4) 
#   # + facet_wrap(~survey)



df <-
  le %>% 
  
  filter(latin %in% myspecies) %>%
  filter(length >= mylength[1] & length < mylength[2]) %>%
  filter(survey %in% mysurvey) %>%
  
  mutate(b = n * 0.01 * length^3) %>%

  group_by(id, latin) %>% 
  summarise(N = sum(n),
            b = sum(b)) %>%
  ungroup() %>% 
  
  right_join(st) %>%
  
  filter(quarter %in% myquarter) %>% 
  filter(year %in% myyear) %>% 
  
  # only use points with rc smaller than myline
  # filter((lat-myline$y[1])/(lon-myline$x[1]) < (myline$y[2]-myline$y[1])/(myline$x[2]-myline$x[1])) %>% 
  
  mutate(year = year(date),
         N    = ifelse(is.na(N), 0, N),
         B    = ifelse(is.na(b), 0, b),
         sq   = encode_zchords(lon, lat, dx = 1)) %>% 
  
  group_by(sq, year, latin) %>% 
  summarise(N = mean(N),
            B = mean(N)) %>% 
  separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>%
  filter(!is.na(latin))


# Create final plot
map +
  geom_raster(data = df, aes(lon, lat, fill = B)) +
  scale_fill_viridis(option = "B", direction = -1) +
  ggtitle(paste(paste(myspecies, collapse=" "), 
                paste(range(myyear), collapse=" "), 
                "quarter:", 
                paste(myquarter, collapse=" "), 
                "length:",
                paste(mylength, collapse="-"))) +
  facet_wrap(~ year, ncol = 6)


