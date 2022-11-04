# -----------------------------------------------------------------------------------------------
# Read catch by rectangle from exchange.r 
#
# 21/08/2019 First coding
# -----------------------------------------------------------------------------------------------

rm(list=ls())

# library(devtools)
library(tidyverse)    # tidying packages
library(readxl)       # read excel
library(lubridate)

# source my utils
source("D:/GIT/mptools/R/my_utils.r")

# Data path
datapath <- "D:/WGWIDE/2019/06. Data/_catch_by_rectangle/HER/2006"
# datapath <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/WGWIDE/2019 Meeting docs/06. Data/_catch_by_rectangle"

# list the available files within the directory
files.list <- list.files(path=datapath, 
                         pattern="xls",
                         full.names=TRUE )

# read the files
f <- files.list[3]

catch_by_rect <- data.frame()
t             <- area_wg_name <-  area_official_name <- NULL

for (f in files.list) {
  print(f)
  
  area_official_name <- excel_sheets(f)[grepl("area-official", tolower(excel_sheets(f)))]
  area_wg_name       <- excel_sheets(f)[grepl("area-wg", tolower(excel_sheets(f)))]
  
  country <- as.character(read_excel(f, sheet = area_official_name, range="B4", col_names=FALSE, col_types="text"))
  species <- as.character(read_excel(f, sheet = area_official_name, range="B5", col_names=FALSE, col_types="text"))
  year    <- as.integer  (read_excel(f, sheet = area_official_name, range="B6", col_names=FALSE, col_types="text"))
  
  print(country)
  print(species)
  
  t <-
    read_excel(f, sheet = area_official_name, range="A17:E1829", col_names=TRUE, col_types="text") %>% 
    lowcase() %>% 
    gather(key=pnum, value=catch, quarter1:quarter4) %>% 
    mutate_at(c("catch"), list(as.numeric) )  %>%
    mutate(pnum = as.integer(gsub("quarter","", pnum))) %>% 
    mutate(ptype = "Q") %>% 
    filter(catch >0) %>% 
    mutate(country = country, species = species, year = year, source="official")
  
  if(!is.null(t)) {
    catch_by_rect <- bind_rows(catch_by_rect, t)
    t <- NULL; area_official_name <- NULL
  }

  t <-
    read_excel(f, sheet = area_wg_name, range="A17:E1829", col_names=TRUE, col_types="text") %>% 
    lowcase() %>% 
    gather(key=pnum, value=catch, quarter1:quarter4) %>% 
    mutate_at(c("catch"), list(as.numeric) )  %>%
    mutate(pnum = as.integer(gsub("quarter","", pnum))) %>% 
    mutate(ptype = "Q") %>% 
    filter(catch >0) %>% 
    mutate(country = country, species = species, year = year, source="WG")

  if(!is.null(t)) {
    catch_by_rect <- bind_rows(catch_by_rect, t)
    t <- NULL; area_wg_name <- NULL
  }
  
}


