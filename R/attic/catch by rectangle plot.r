# -----------------------------------------------------------------------------------------------
# Catch by rectangle plot.r 
#
# Plot catch by rectangle data
#
# 05/09/2017 First version based on code from Einar
# 13/08/2018 Updated on the basis of Datras doodle website
# 08/05/2019 Adapted from Datras plots for catch data
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
library(mapproj)
library(lubridate)
library(viridis)

# source my utils
source("D:/GIT/mptools/R/my_utils.r")

# Data path
datapath <- "D:/WGWIDE/2018/06. Data/_catch_by_rectangle"

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# load spatial datasets
load(file.path(onedrive, "rdata/world.df.RData"))
load(file.path(onedrive, "rdata/eez.df.RData"))
load(file.path(onedrive, "rdata/fao.df.RData"))
load(file.path(onedrive, "rdata/depth200.df.RData"))
load(file.path(onedrive, "rdata/icesrectangles.df.RData"))

load(file.path(onedrive, "rdata/fao.RData"))
load(file.path(onedrive, "rdata/afsis.RData"))

# -----------------------------------------------------------------------------------------------
# Load the catch data
# -----------------------------------------------------------------------------------------------

catch <- 
  read.csv(file.path(datapath, "WGCatchBySR_MAC.csv")) %>% 
  lowcase() %>% 
  group_by(year, lat, lon) %>% 
  summarize(catch = sum(catch, na.rm=TRUE)) %>% 
  
  mutate(catch_interval = Hmisc::cut2(as.integer(catch), cuts=c(1 %o% 10^(0:12)), digits=10 ) ) %>% 
  mutate(catch_interval = gsub(" ","", catch_interval)) %>% 
  filter(!is.na(catch_interval)) %>% 
  filter(grepl("\\,", catch_interval)) %>%   
  group_by(year, lat, lon, catch_interval) %>% 
  summarize(catch = sum(catch, na.rm=TRUE))


xlim <- range(catch$lon, na.rm=TRUE)
ylim <- range(catch$lat, na.rm=TRUE)

catch %>% 
  # filter(year >= 2011) %>% 
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
  
  coord_quickmap(xlim=xlim, ylim=ylim) +
  # coord_map(projection="ortho", xlim=xlim, ylim=ylim) +
  
  # geom_polygon(data=depth200.df, aes(long, lat, group=group, fill=hole), 
  #              size=0.05, colour="lightblue", show.legend = FALSE) +

  geom_polygon(data=fao.df, aes(long, lat, group=group), fill = NA, size=0.2,
               color="gray80", alpha=0.3) +

  # geom_tile(aes(fill = catch, height=0.5, width=1), colour=NA, alpha=1.0) +
  geom_tile(aes(fill = catch_interval, height=0.5, width=1), colour=NA, alpha=1.0) +
  # scale_fill_distiller(palette = "RdBu", breaks=b) +
  scale_fill_viridis(option = "plasma", direction = -1, discrete=TRUE) +
  # scale_fill_viridis(option = "plasma", direction = -1, breaks=b) +
  # geom_point(aes(size=catch), alpha = 0.6, shape=20) +
  # scale_size(range = c(0.1,5)) +

  geom_polygon(data=world.df, aes(long, lat, group=group), fill = "cornsilk", 
               size=0.1, color="black") +
  
  labs(x = NULL, y = NULL, size = "tons") +
  # guides(colour=FALSE) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap( ~ year, ncol=7)
