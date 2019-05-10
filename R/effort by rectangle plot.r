# -----------------------------------------------------------------------------------------------
# Effort by rectangle plot.r 
#
# From globalfishingwatch
#
# 09/05/2019 First coding
# -----------------------------------------------------------------------------------------------

rm(list=ls())

# library(devtools)
library(icesDatras)   # install.packages("icesDatras")
library(tidyices)     # devtools::install_github("fishvice/tidyices", dependencies = TRUE)
library(tidyverse)    # tidying packages
library(lubridate)
library(sf)
library(sp)
library(data.table)
library(scales)
# library(gisland)      # devtools::install_github("einarhjorleifsson/gisland", dependencies = TRUE)

library(maps)
library(mapdata)
library(lubridate)
library(viridis)

# source my utils
source("D:/GIT/mptools/R/my_utils.r")

# Data path
datapath <- "E:/globalfishingwatch_fishing_effort_byvessel/daily_csvs"

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# load spatial datasets
load(file.path(onedrive, "rdata/world.df.RData"))
load(file.path(onedrive, "rdata/eez.df.RData"))
load(file.path(onedrive, "rdata/fao.df.RData"))
load(file.path(onedrive, "rdata/depth200.df.RData"))
load(file.path(onedrive, "rdata/icesrectangles.df.RData"))

load(file.path(onedrive, "rdata/fao.RData"))

# -----------------------------------------------------------------------------------------------
# Load the catch data
# -----------------------------------------------------------------------------------------------

list.files <- 
  list.files(path = datapath, pattern = "-03-|-04-", full.names = TRUE)

df <- data.frame()

for (f in list.files) {
  t  <- 
    read.csv(file=f, header=TRUE) %>% 
    mutate(lat = as.numeric(lat_bin)/10,
           lon = as.numeric(lon_bin)/10,
           date    = ymd(date),
           year    = year(date)) %>% 
    filter(lon >= -20, lon <= 0, lat >= 50, lat <= 62) 
    
  df <- bind_rows(df, t) 
}  

effort <- 
  df %>% 
  group_by(year, lon, lat) %>% 
  summarize(effort = sum(fishing_hours, na.rm=TRUE)) %>% 
  filter(effort > 0) %>% 
  mutate(logeffort = log(effort)) %>% 
  mutate(effort_interval = cut(logeffort, breaks=seq(-5, 8, by=1)) ) %>% 
  group_by(year, lat, lon, effort_interval) %>% 
  summarize(effort = log(sum(effort, na.rm=TRUE)))


xlim <- range(effort$lon, na.rm=TRUE)
ylim <- range(effort$lat, na.rm=TRUE)

effort %>% 
  # filter(year >= 2015) %>% 
  ggplot(aes(lon, lat)) + 
  theme_publication() +
  theme(panel.border     = element_rect(colour="black" , size=0.2),
        panel.grid.major = element_blank(),
        # panel.background = element_rect(fill = "lightskyblue1"),
        strip.background = element_rect(colour="black", size =0.2),
        plot.margin      = unit(c(0,0,0,0),"cm"),
        plot.title       = element_text(vjust=0, size=10),
        axis.text        = element_text(size=6),
        legend.key.width = unit(0.4, "cm"), 
        # plot.background  = element_rect(fill = "lightblue"),
        # legend.position  = "none", 
        axis.title       = element_blank()) +
  
  coord_quickmap(xlim=xlim,
                 ylim=ylim) +
  
  # geom_polygon(data=depth200.df, aes(long, lat, group=group, fill=hole), 
  #              size=0.05, colour="lightblue", show.legend = FALSE) +

  geom_polygon(data=fao.df, aes(long, lat, group=group), fill = NA, size=0.2,
               color="gray80", alpha=0.3) +

  geom_tile(aes(fill = effort_interval, height=0.1, width=0.1), colour=NA, alpha=1.0) +
  # scale_fill_distiller(palette = "RdBu", breaks=b) +
  scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
  # scale_fill_viridis(option = "plasma", direction = -1, breaks=b) +
  # geom_point(aes(size=catch), alpha = 0.6, shape=20) +
  # scale_size(range = c(0.1,5)) +

  geom_polygon(data=world.df, aes(long, lat, group=group), fill = "cornsilk", 
               size=0.1, color="black") +
  
  labs(x = NULL, y = NULL, size = "fishing hours") +
  # guides(colour=FALSE) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap( ~ year, ncol=3)
