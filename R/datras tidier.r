# -------------------------------------------------------------------------------
# Datras tidy it up
#
# http://www.hafro.is/~einarhj/datrasdoodle/import.html#data-tidying
#
# 14/08/2018 first coding
# 15/03/2019 coding during HAWG
# 19/07/2019 small change in mutate_at
# 
# NEED TO CLEAN OUT THE TIDY DIRECTORY BEFORE RUNNING THIS CODE
# -------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(lubridate)
library(sf)
library(icesDatras)  # install.packages("icesDatras")
library(tidyices)    
library(data.table)

# library(gisland)     # devtools::install_github("einarhjorleifsson/gisland", dependencies = FALSE); requires geo package that no longer exists
source("D:/GIT/gisland/R/read_sf_ftp.R")
source("D:/GIT/gisland/R/geo_inside.R")

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

# list_all_objects_in_package("tidyices")

# Data path
datapath <- "E:/DATRAS"

# Make sure the needed objects are available
# fao     <- gisland::read_sf_ftp("fao-areas_nocoastline") %>% as("Spatial")
# ns_area <- gisland::read_sf_ftp("NS_IBTS_RF") %>% as("Spatial")

fao     <- read_sf_ftp("fao-areas_nocoastline") %>% as("Spatial")
ns_area <- read_sf_ftp("NS_IBTS_RF") %>% as("Spatial")
species <- read_csv("ftp://ftp.hafro.is/pub/reiknid/einar/datras_worms.csv")

# Get an overview of all the surveys that have been downloaded in raw format
fil <- dir(paste(datapath, "/raw", sep=""), full.names = TRUE)

# create empty data.frames
hh <- hl <- ca <- data.frame()
prevsur <- ""

i <- 27

for(i in 1:length(fil)) {

# for(i in c(1,28,29, 32, 33, 306, 307)) {
# for(i in c(1,28,29, 32, 33)) {
# for(i in 1:4) {
  year <- str_extract_all(fil[[i]],"\\(?[0-9]+\\)?")[[1]] 
  
  raw <- read_rds(fil[i])
  
  # check that there is actually data in the file
  if(!is.null(dimnames(summary(read_rds(fil[i])))) ) {
    
    sur  <- raw$hh$Survey[1] %>% tolower()
    
    hhy  <-
      raw$hh %>% 
      tidy_hh(all_variables = TRUE) %>%
      mutate_all(funs(ifelse(is.na(.) & is.numeric(.),NA_character_,.))) %>%
      mutate_at(c("netopening","doorsurface","doorspread","wingspread","kitedim","groundspeed",
                   "speedwater","surcurspeed","botcurspeed","swellheight","surtemp","bottemp",
                  "sursal","botsal","shootlat","shootlong","haullat","haullong"), 
                list(as.numeric)) %>% 

      mutate_at(c("quarter","sweeplngt","haulno","month","day","hauldur","depth","tickler","distance",
                  "warplngt","warpdia","warpden","doorwgt","buoyancy","wgtgroundrope","towdir",
                  "surcurdir","botcurdir","winddir","windspeed","swelldir","thclinedepth",
                  "dateofcalculation","datetime"), 
                list(as.integer)) %>% 
      
      mutate(nsarea  = geo_inside(shootlong, shootlat, ns_area, "AreaName") %>% as.integer(),
             faoarea = geo_inside(shootlong, shootlat, fao, "name"), 
             date    = dmy(paste(day, month, year, sep="/")))
    
    if(!is.null(raw$hl)) hly <- raw$hl %>%  tidy_hl(hhy, species) 
    if(!is.null(raw$ca)) cay <- raw$ca %>%  tidy_ca(species)  
    
    if (sur == prevsur) {
      
      # continue with this survey, just another year
      print(paste (i,sur,year, sep=" "))
      
      hh <- bind_rows(hh, hhy)
      if(!is.null(raw$hl)) hl <- bind_rows(hl, hly)
      if(!is.null(raw$ca)) ca <- bind_rows(ca, cay)
      
      prevsur <- sur
      
    } else {
      
      # except for the first line, save the hh data
      if (i > 1) {
        # save the old survey data
        hh %>% write_rds(path = paste0(datapath, "/tidy/", tolower(prevsur), "_hh.rds"))
        hl %>% write_rds(path = paste0(datapath, "/tidy/", tolower(prevsur), "_hl.rds"))
        ca %>% write_rds(path = paste0(datapath, "/tidy/", tolower(prevsur), "_ca.rds"))
      }
      
      # start of a new survey
      print("New survey ------------------")
      print(paste0(i, sur,year, sep=" "))
      print(paste(sur, prevsur))
      
      # reset the data.frames
      hh <- hl <- ca <- data.frame()
      
      hh <- bind_rows(hh, hhy)
      if(!is.null(raw$hl)) hl <- bind_rows(hl, hly)
      if(!is.null(raw$ca)) ca <- bind_rows(ca, cay)
      
      prevsur <- sur
      
    } # end of if statement on whether a new survey is started
  } # end of if statement to check for empty files
  
}

# final loop for the last datafile
if (i == length(fil)) {
  
  # save the last survey
  hh %>% write_rds(path = paste0(datapath, "/tidy/", tolower(prevsur), "_hh.rds"))
  hl %>% write_rds(path = paste0(datapath, "/tidy/", tolower(prevsur), "_hl.rds"))
    ca %>% write_rds(path = paste0(datapath, "/tidy/", tolower(prevsur), "_ca.rds"))
    
} # end of loop over file names

# glimpse(hh)
# glimpse(hhy)
# unique(hh$datetime)
# unique(hhy$survey)


# tst_hh <- read_rds(path = paste0(datapath, "/tidy/", "bits_hh.rds"))
