# -----------------------------------------------------------------------------------------------
# Catch by rectangle HOM data.r 
#
# 23/05/2019 Generate HOM catch by rectangle data from raw input files
# -----------------------------------------------------------------------------------------------

rm(list=ls())

# library(devtools)
library(icesDatras)   # install.packages("icesDatras")
library(tidyices)     # devtools::install_github("fishvice/tidyices", dependencies = TRUE)
library(tidyverse)    # tidying packages
library(readxl)       # read excel
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

library(animation)
library(gganimate)

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
# HOM excel files 2001-2011
# -----------------------------------------------------------------------------------------------

files.list <- list.files(path=file.path(datapath, "HOM"), pattern="xlsx",full.names=TRUE )

df <- data.frame()

# loop over files
for (f in files.list) {
  
  print(f)
  
  # find worksheets
  worksheets <- excel_sheets(f)
  
  # loop over worksheets
  for (w in worksheets) {
    if(!(tolower(w) %in% c("tot","sr","div"))) {
      # print(w) 
      t <- 
        read_excel(f, sheet=w, col_names=TRUE, col_types="text") %>% 
        lowcase() %>% 
        mutate(
          species = "HOM",
          country = w, 
          file = f ,
          year = gsub("[^0-9]+","", basename(f)) )
      
      df <- bind_rows(df, t)
      
    }
  }
}

hom1 <-
  df %>% 
  dplyr::select(-starts_with("x")) %>% 
  dplyr::select(-icesdiv, -tot) %>% 
  gather(key=pnum, value=catch, quarter1:quarter4) %>% 
  mutate(pnum = as.integer(gsub("quarter","", pnum))) %>% 
  mutate(ptype = "q") %>% 
  rename(icesrect = rect,
         lat      = latitude,
         lon      = longitude) %>% 
  mutate_at(c("year", "pnum"), list(as.integer) )  %>% 
  mutate_at(c("lat","lon","catch"), list(as.numeric) ) %>% 
  mutate(
    country = ifelse(country == "DE", "GER", country),
    country = ifelse(country == "DK", "DNK", country),
    country = ifelse(country == "ES", "ESP", country),
    country = ifelse(country == "FO", "FAR", country),
    country = ifelse(country == "IE", "IRL", country),
    country = ifelse(country == "NL", "NLD", country),
    country = ifelse(country == "NO", "NOR", country),
    country = ifelse(country == "PT", "POR", country),
    country = ifelse(country == "UKS","SCO", country),
    country = ifelse(country == "UKE","ENG", country),
    country = ifelse(country == "UKN","NIRL", country)
  ) %>% 
  filter(catch > 0) 
  
# glimpse(hom)
# unique(hom1$country)


save(hom1, file=file.path(datapath, "CatchBySR HOM1.RData"))


# -----------------------------------------------------------------------------------------------
# process hom data ; ERRORS IN THE DATA FILES; EVERYTHING ZERO?
# -----------------------------------------------------------------------------------------------

files.list <- list.files(path=file.path(datapath, "HOM"), pattern="csv",full.names=TRUE )
# print(files.list)

df <- data.frame()

# f  <- "D:/WGWIDE/2018/06. Data/_catch_by_rectangle/HOM/HOM_NLD_SR_2015.csv"
  
for (f in files.list) {
  print(f)
  t <-
    read.csv(f, header=TRUE, stringsAsFactors = FALSE, colClasses = "character") %>%
    lowcase() 
  
  df <- bind_rows(df, t)
}

# check
# df %>%
#   filter(year == 2012, country == "NLD") %>%
#   mutate(catch = as.numeric(catch)) %>%
#   group_by(country, year) %>%
#   summarize(catch=sum(catch, na.rm=TRUE)) %>%
#   View()

# mutate ices rectangles for mergin
t <- 
  icesrectangles.df %>% 
  mutate(rect = as.character(rect)) %>% 
  distinct(rect, .keep_all=TRUE) %>% 
  dplyr::select(rect, lat, long)

hom2 <-
  df %>% 
  mutate_at(c("catch", "longitude","latitude"), list(as.numeric)) %>% 
  mutate_at(c("year"), list(as.integer)) %>% 

  filter(!is.na(rect)) %>% 
  filter( catch > 0 ) %>% 
  
  left_join(t, by=c("rect")) %>% 
  mutate(longitude = ifelse(is.na(longitude), long, longitude),
         latitude  = ifelse(is.na(latitude), lat, latitude)) %>% 
  dplyr::select(-long, -lat) %>% 
  
  rename(icesrect = rect,
         lat      = latitude,
         lon      = longitude ) %>% 
  mutate(season = gsub("quarter","q",tolower(season))) %>% 
  mutate(pnum     = as.integer(gsub("[^0-9]+", "", season)),
         ptype    = gsub("[^a-z]+","", season) )  %>% 
  
  dplyr::select(-season)

save(hom2, file=file.path(datapath, "CatchBySR HOM2.RData"))


# hom2 %>% distinct(country) %>% View()
# hom2 %>% filter(country=="ENG") %>% View()
# hom2 %>% group_by(year, country) %>% summarize(catch=round(sum(catch, na.rm=TRUE))) %>% View()
# hom %>% 
#   group_by(year, country) %>% 
#   summarize(catch=round(sum(catch, na.rm=TRUE))) %>% 
#   
#   ggplot(aes(x=year, y=catch, group=country)) + 
#   geom_line(aes(colour=country)) + 
#   facet_wrap(~country)

# check
# hom2 %>%
#   filter(year == 2015, country == "NLD") %>%
#   mutate(catch = as.numeric(catch)) %>%
#   group_by(country, year) %>%
#   summarize(catch=sum(catch, na.rm=TRUE)) %>%
#   View()




hom <-
  bind_rows(hom1, hom2) %>% 
  mutate(species = "HOM")

save(hom, file=file.path(datapath, "CatchBySR HOM.RData"))


