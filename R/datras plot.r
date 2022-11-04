# -----------------------------------------------------------------------------------------------
# Datras plot.r 
#
# R version: 3.5
# 
# Plot simple survey plots
#
# 05/09/2017 First version based on code from Einar
# 13/08/2018 Updated on the basis of Datras doodle website
# 18/09/2019 Corrected error in calculation of B (used N instead of B before)
# -----------------------------------------------------------------------------------------------

rm(list=ls())

# library(devtools)
library(icesDatras)   # install.packages("icesDatras")
# library(tidyices)     # devtools::install_github("fishvice/tidyices", dependencies = TRUE)
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
source("../prf/R/my utils.r")

onedrive <- get_onedrive()

# Data path
datapath <- "F:/ICES/DATRAS"

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
  print(paste(i, list.hh[i], sep = " - "))
  
  if (i == 1) {
    hh <- read_rds(file = list.hh[i])
  } else {
    hh <- bind_rows(hh, read_rds(file = list.hh[i])) 
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
  print(paste(i, list.hl[i], sep = " - "))
  if (i == 1) {
    hl <- read_rds(file = list.hl[i])
  } else {
    hl <- bind_rows(hl, read_rds(file = list.hl[i])) 
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
  print(paste(i, list.ca[i], sep = " - "))
  if (i == 1) {
    ca <- read_rds(file = list.ca[i])
  } else {
    ca <- bind_rows(ca, read_rds(file = list.ca[i])) 
  } 
}

# ca <- 
#   read_rds(path = paste0(datapath, "/tidy/", "ns-ibts_ca.rds")) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "evhoe_ca.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "fr-cgfs_ca.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "ie-igfs_ca.rds"))) %>% 
#   bind_rows(read_rds(path = paste0(datapath, "/tidy/", "scowcgfs_ca.rds")))

# afsis
afsis <- 
  get(load(file.path(onedrive, "rdata/afsis.RData"))) %>% 
  rename(latin = scientific_name)

# -----------------------------------------------------------------------------------------------
# Survey station plots
# -----------------------------------------------------------------------------------------------
mysurvey <- "FR-CGFS"
myspecies <- "her"

le <- 
  hl %>%
  left_join(dplyr::select(afsis, species, latin), by="latin") %>% 
  dplyr::filter(survey  %in% mysurvey, 
                species %in% myspecies) %>% 
  group_by(survey, id, latin, species, length) %>% 
  summarise(n = n())

# stations
st <-
  hh %>%
  filter(
    survey  %in% mysurvey, 
    year %in% 2020:2022) %>%
  select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat)

# summary(st)

xlim <- range(st$lon)
ylim <- range(st$lat)

# generate final data frame
df <-
  le %>%
  filter(length >= 10 & length < 30) %>%
  
  mutate(b = n * 0.01 * length^3) %>%
  
  group_by(id, latin, species) %>%
  summarise(N = sum(n),
            b = sum(b)) %>%
  ungroup() %>%
  
  right_join(st) %>%
  
  # filter(quarter %in% 1) %>%
  filter(year %in% 2020:2022) %>%
  
  # only use points with rc smaller than myline
  # filter((lat-myline$y[1])/(lon-myline$x[1]) < (myline$y[2]-myline$y[1])/(myline$x[2]-myline$x[1])) %>%
  
  mutate(
    # year = year(date),
    N    = ifelse(is.na(N), 0, N),
    B    = ifelse(is.na(b), 0, b),
    sq   = encode_zchords(lon, lat, dx = 1)) %>%
  
  # Special treatment of CGFS; one year later
  # mutate(year = ifelse(survey == "FR-CGFS", year+1, year)) %>%
  # mutate(year = paste(year-1,year,sep="-")) %>%
  
  group_by(lat, lon, year, latin, species) %>%
  summarise(N = mean(N),
            B = mean(B)) %>%
  filter(!is.na(latin)) 

# Create final plot
# plot map
map_data("worldHires", xlim = xlim, ylim = ylim) %>%
  ggplot() +
  theme_bw() +
  geom_polygon(aes(long, lat, group = group), fill = "grey") +
  coord_quickmap(xlim = xlim, ylim = ylim) +
  # coord_quickmap(xlim = c(xlim[1],4), ylim = ylim) +
  scale_x_continuous(expand = expand_scale(add=c(1,1))) +
  scale_y_continuous(expand = expand_scale(add=c(1,1))) + 
  
  geom_point(data = st, aes(lon, lat), colour="red", shape=3, size=0.5) +
  geom_point(data = df, aes(lon, lat, size = N), shape=21, fill=NA) +
  facet_wrap(~ year, ncol = 6)



# -----------------------------------------------------------------------------------------------
# Now the plots
# -----------------------------------------------------------------------------------------------

# cat(unique(hh$survey))

# set selection criteria
mysurvey  <- c( "BTS-VIII","BTS","DYFS","EVHOE","FR-CGFS","IE-IGFS","NIGFS", "DWS",
                "NS-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                "SP-PORC","SWC-IBTS")

# mysurvey  <- c("NS-IBTS", "EVHOE", "IE-IGFS", "SCOWCGFS", "FR-CGFS", "NIGFS", "SCOROC",
#                "SP-ARSA", "SP-NORTH", "SP-PORC", "PT-IBTS", "SWC-IBTS")

# mysurvey  <- c("NS-IBTS", "FR-CGFS")

# mysurvey  <- c("NS-IBTS")

myyear    <- 2015:2021
# myyear    <- 2003:2019

# myquarter <- c(1)
# myquarter <- c(1,4)
# myquarter <- c(3)
myquarter <- c(1, 3, 4)

# myspecies <- "Sprattus sprattus"
# myspecies <- "Sardina pilchardus"
# myspecies <- "Clupea harengus"
myspecies <- "hom"
# myspecies <- "Chelidonichthys cuculus"
# myspecies <- "Mullus surmuletus"
# myspecies <- "Pleuronectes platessa"
# myspecies <- "Solea solea"
# myspecies <- "Gadus morhua"
# myspecies <- "Engraulis encrasicolus"

# myspecies <- "ary"

mylength  <- c(10,40)

midpoint <- F

# sort(unique(hl$latin))

plot_datras <- function(mysurvey, myyear, myquarter, myspecies, mylength, midpoint=F) {
  
  mylatin <- filter(afsis, species %in% myspecies) %>% dplyr::select(latin)
  
  # numbers at length
  le <- 
    hl %>%
    left_join(dplyr::select(afsis, species, latin), by="latin") %>% 
    dplyr::filter(survey  %in% mysurvey, 
                  species %in% myspecies) %>% 
    group_by(survey, id, latin, species, length) %>% 
    summarise(n = n())
  
  # stations
  st <-
    hh %>%
    filter(survey  %in% mysurvey, year %in% myyear, quarter %in% myquarter) %>%
    select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat)
  
  # summary(st)
  
  xlim <- range(st$lon)
  ylim <- range(st$lat)

  # plot map
  map <-
    map_data("worldHires", xlim = xlim, ylim = ylim) %>%
    ggplot() +
    theme_bw() +
    geom_polygon(aes(long, lat, group = group), fill = "grey") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(NULL, NULL) +
    scale_y_continuous(NULL, NULL)
  
  # generate final data frame
  df <-
    le %>%
    filter(length >= mylength[1] & length < mylength[2]) %>%
    
    mutate(b = n * 0.01 * length^3) %>%

    group_by(id, latin, species) %>%
    summarise(N = sum(n),
              b = sum(b)) %>%
    ungroup() %>%

    right_join(st) %>%

    filter(quarter %in% myquarter) %>%
    filter(year %in% myyear) %>%

    # only use points with rc smaller than myline
    # filter((lat-myline$y[1])/(lon-myline$x[1]) < (myline$y[2]-myline$y[1])/(myline$x[2]-myline$x[1])) %>%

    mutate(
      # year = year(date),
      N    = ifelse(is.na(N), 0, N),
      B    = ifelse(is.na(b), 0, b),
      sq   = encode_zchords(lon, lat, dx = 1)) %>%

    # Special treatment of CGFS; one year later
    # mutate(year = ifelse(survey == "FR-CGFS", year+1, year)) %>%
    # mutate(year = paste(year-1,year,sep="-")) %>%

    group_by(sq, year, latin, species) %>%
    summarise(N = mean(N),
              B = mean(B)) %>%
    separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>%
    filter(!is.na(latin)) 
  
  df2 <-
    df %>% 
    group_by(year, latin, species) %>% 
    summarize(lat = weighted.mean(lat, w=B, na.rm=TRUE),
              lon = weighted.mean(lon, w=B, na.rm=TRUE))
  
  df3 <- 
    df %>% 
    group_by(latin, species, year) %>% 
    summarize(B = mean(B, na.rm=TRUE))
  
  # Create final plot
  map +
    geom_raster(data = df, aes(lon, lat, fill = B)) +
    scale_fill_viridis(option = "B", direction = -1) +
    
    {if(midpoint) geom_point(data=df2, aes(lon, lat), shape=3, size=2, stroke=2, colour="green")} +
    
    labs(
      title    = paste0(paste(mylatin, collapse=" "),
                        paste(" ("),
                        paste(toupper(myspecies), collapse=" "),
                        paste(") ")),
      subtitle = paste0("Length:",
                        paste(mylength, collapse="-")),
      caption  = paste0(paste(unique(st$survey), collapse=","),
                        " quarter:",
                        paste(myquarter, collapse=";") ) ) +
    facet_wrap(~ year, ncol = 6)

  
  # df3 %>% 
  #   ggplot(aes(year, B)) +
  #   theme_bw() +
  #   geom_line()
  
}


# sort(unique(hl$latin))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("her"), mylength=c(10,20))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(3), myspecies=c("her"), mylength=c(10,20))

plot_datras(mysurvey=c("BTS"),     myyear=2003:2020, myquarter=c(3), myspecies=c("ple"), mylength=c(30,40))
plot_datras(mysurvey=c("BTS"),     myyear=2003:2020, myquarter=c(3), myspecies=c("ple"), mylength=c(5,15))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2022, myquarter=c(1), myspecies=c("ple"), mylength=c(30,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2020, myquarter=c(3), myspecies=c("ple"), mylength=c(25,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2005:2022, myquarter=c(3), myspecies=c("ple"), mylength=c(30,40))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("spr"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("pil"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("ane"), mylength=c(10,30))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2021, myquarter=c(1), myspecies=c("mac"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2021, myquarter=c(1), myspecies=c("hom"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("cod"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("whg"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("had"), mylength=c(10,30))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("rjc"), mylength=c(10,50))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("aru","ary","arg"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(3), myspecies=c("aru","ary","arg"), mylength=c(10,40))

plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("ary"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("aru"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=2003:2019, myquarter=c(1), myspecies=c("arg"), mylength=c(10,40))

plot_datras(mysurvey=c("NS-IBTS"), myyear=1991:2018, myquarter=c(1), myspecies=c("ple"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=1991:2018, myquarter=c(1), myspecies=c("sol"), mylength=c(10,40))

plot_datras(mysurvey=c("BTS"), myyear=1991:2018, myquarter=c(3), myspecies=c("ple"), mylength=c(10,40))
plot_datras(mysurvey=c("NS-IBTS"), myyear=1991:2018, myquarter=c(3), myspecies=c("ple"), mylength=c(10,40))

plot_datras(mysurvey=c("NS-IBTS","SCOWCGFS","SWC-IBTS"), 
            myyear=c(2000:2018,2020:2021), 
            # myquarter=c(1,3, 4), 
            myquarter=c(3,4), 
            myspecies=c("ple"), 
            mylength=c(30,40))

plot_datras(mysurvey=c("SCOWCGFS"), 
            myyear=c(2012:2021), 
            myquarter=c(4), 
            myspecies=c("ple"), 
            mylength=c(20,40))

plot_datras(mysurvey=c("EVHOE","FR-CGFS","IE-IGFS","NIGFS",
                        "NS-IBTS","ROCKALL","SCOROC","SCOWCGFS",
                        "SP-PORC","SWC-IBTS"), 
            myyear=2000:2021, 
            myquarter=c(2), 
            myspecies=c("mac"), 
            mylength=c(10,40))



# map all surveys
# map +
#   theme_publication() +
#   # geom_point(data = filter(haul, shootlat > 40, shootlat < 62, year >= 2015), 
#   #            aes(x=shootlon, y=shootlat, size=catch), colour = "blue", alpha = 0.2) +
#   geom_point(data = st, aes(lon, lat, group=survey, colour=survey), size=0.1, alpha = 0.4) 
#   # + facet_wrap(~survey)



plot_datras_bysurvey <- function(mysurvey, myyear, myquarter, myspecies, mylength) {
  
  mylatin <- filter(afsis, species %in% myspecies) %>% dplyr::select(latin)
  
  # numbers at length
  le <- 
    hl %>%
    left_join(dplyr::select(afsis, species, latin), by="latin") %>% 
    dplyr::filter(survey  %in% mysurvey, 
                  species %in% myspecies) %>% 
    group_by(survey, id, latin, species, length) %>% 
    summarise(n = n())
  
  # stations
  st <-
    hh %>%
    filter(survey  %in% mysurvey, year %in% myyear, quarter %in% myquarter) %>%
    select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat) %>% 
    mutate(survey2 = paste0(survey, "-q", quarter))
  
  # summary(st)
  
  xlim <- range(st$lon)
  ylim <- range(st$lat)
  
  # plot map
  map <-
    map_data("worldHires", xlim = xlim, ylim = ylim) %>%
    ggplot() +
    theme_bw() +
    geom_polygon(aes(long, lat, group = group), fill = "grey") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(NULL, NULL) +
    scale_y_continuous(NULL, NULL)
  
  # generate final data frame
  df <-
    le %>%
    filter(length >= mylength[1] & length < mylength[2]) %>%
    
    mutate(b = n * 0.01 * length^3) %>%
    
    group_by(survey, id, latin, species) %>%
    summarise(N = sum(n),
              b = sum(b)) %>%
    ungroup() %>%
    
    right_join(st) %>%
    
    filter(quarter %in% myquarter) %>%
    filter(year %in% myyear) %>%
    
    # only use points with rc smaller than myline
    # filter((lat-myline$y[1])/(lon-myline$x[1]) < (myline$y[2]-myline$y[1])/(myline$x[2]-myline$x[1])) %>%
    
    mutate(
      # year = year(date),
      N    = ifelse(is.na(N), 0, N),
      B    = ifelse(is.na(b), 0, b),
      sq   = encode_zchords(lon, lat, dx = 1)) %>%
    
    # Special treatment of CGFS; one year later
    # mutate(year = ifelse(survey == "FR-CGFS", year+1, year)) %>%
    # mutate(year = paste(year-1,year,sep="-")) %>%
    
    group_by(survey2, sq, year, latin, species) %>%
    summarise(N = mean(N),
              B = mean(N)) %>%
    separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>%
    filter(!is.na(latin)) 
  
  df2 <-
    df %>% 
    group_by(survey2, year, latin, species) %>% 
    summarize(lat = weighted.mean(lat, w=B, na.rm=TRUE),
              lon = weighted.mean(lon, w=B, na.rm=TRUE))
  
  df3 <- 
    df %>% 
    group_by(survey2, latin, species, year) %>% 
    summarize(B = mean(B, na.rm=TRUE))
  
  # Create final plot
  map +
    geom_raster(data = df, aes(lon, lat, fill = B)) +
    scale_fill_viridis(option = "B", direction = -1) +
    # geom_point(data=df2, aes(lon, lat), shape=3, size=2, stroke=2, colour="green") +
    ggtitle(paste0(paste(mylatin, collapse=" "),
                   paste(" ("),
                   paste(toupper(myspecies), collapse=" "),
                   paste(") "),
                   paste(mysurvey, collapse=","),
                   paste(range(myyear), collapse="-"),
                   paste(" "),
                   "quarter:",
                   paste(myquarter, collapse=";"),
                   paste(" "),
                   "length:",
                   paste(mylength, collapse="-"))) +
    facet_wrap(~ survey2, ncol = 6)
}

plot_datras_bysurvey(
  mysurvey=c( "BTS-VIII","BTS","DYFS","EVHOE","FR-CGFS","IE-IGFS","NIGFS", "DWS",
              "NS-IBTS","ROCKALL","SCOROC","SCOWCGFS", "DWS",
              "SP-PORC","SWC-IBTS"), 
  myyear=2009, 
  myquarter=c(1,2,3,4), 
  myspecies=c("mac"), 
  mylength=c(10,40))

plot_datras_bysurvey(
  mysurvey=c( "BTS","FR-CGFS","IE-IGFS","NIGFS","NS-IBTS","SCOWCGFS"), 
  myyear=2012:2021, 
  myquarter=c(1,2,3,4), 
  myspecies=c("ple"), 
  mylength=c(10,40))

unique(hh$survey)


cutter <- function(x, lower = 0, upper, by = 10, sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  as.character(cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
      right = FALSE, labels = labs))
}

cutlength <- 5
cutyear   <- 5
mysurvey  <- c("NS-IBTS")
myyear    <- 1991:2019
myquarter <- c(1)
myspecies <- "her"
mylength  <- c(10,40)

# Plot data aggregated by years and lengths
plot_datras_aggregated <- function(mysurvey, myyear, myquarter, myspecies, mylength, cutyear, cutlength) {
  
  mylatin <- filter(afsis, species %in% myspecies) %>% dplyr::select(latin)
  
  # numbers at length
  le <- 
    hl %>%
    left_join(dplyr::select(afsis, species, latin), by="latin") %>% 
    dplyr::filter(survey  %in% mysurvey, species %in% myspecies) %>% 
    dplyr::filter(length >= mylength[1] & length < mylength[2]) %>%
    mutate(length2 = cutter(length, upper=50, by=cutlength)) %>% 
    group_by(survey, id, latin, species, length, length2) %>% 
    summarise(n = n()) %>% 
    
    mutate(b = n * 0.01 * length^3) %>%
    group_by(id, latin, species, length2) %>%
    summarise(N = sum(n),
              b = sum(b)) %>%
    ungroup()
    
  # stations
  st <-
    hh %>%
    filter(survey  %in% mysurvey, year %in% myyear, quarter %in% myquarter) %>%
    select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat) %>% 
    mutate(year2 = cutter(year, upper=2020, by=cutyear))  
    

  # summary(st)
  
  xlim <- range(st$lon)
  ylim <- range(st$lat)
  
  # plot map
  map <-
    map_data("worldHires", xlim = xlim, ylim = ylim) %>%
    ggplot() +
    theme_bw() +
    geom_polygon(aes(long, lat, group = group), fill = "grey") +
    coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(NULL, NULL) +
    scale_y_continuous(NULL, NULL)
  
  # generate final data frame
  df <-
    le %>%
    
    right_join(st) %>%
    
    filter(quarter %in% myquarter) %>%
    filter(year %in% myyear) %>%
    
    mutate(
      N    = ifelse(is.na(N), 0, N),
      B    = ifelse(is.na(b), 0, b),
      sq   = encode_zchords(lon, lat, dx = 1)) %>%
    
    # Special treatment of CGFS; one year later
    # mutate(year = ifelse(survey == "FR-CGFS", year+1, year)) %>%
    # mutate(year = paste(year-1,year,sep="-")) %>%
    
    group_by(sq, year2, length2, latin, species) %>%
    summarise(N = mean(N),
              B = mean(N)) %>%
    separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>%
    filter(!is.na(latin)) 
  
  # unique(le$length2)
  
  # df2 <-
  #   df %>% 
  #   group_by(year, latin, species) %>% 
  #   summarize(lat = weighted.mean(lat, w=B, na.rm=TRUE),
  #             lon = weighted.mean(lon, w=B, na.rm=TRUE))
  
  df3 <- 
    df %>% 
    group_by(latin, species, year2, length2) %>% 
    summarize(B = mean(B, na.rm=TRUE))
  
  # Create final plot
  map +
    geom_raster(data = df, aes(lon, lat, fill = B)) +
    scale_fill_viridis(option = "B", direction = -1) +
    ggtitle(paste0(paste(mylatin, collapse=" "),
                   paste(" ("),
                   paste(toupper(myspecies), collapse=" "),
                   paste(") "),
                   paste(mysurvey, collapse=","),
                   paste(" "),
                   "quarter:",
                   paste(myquarter, collapse=";"),
                   paste(" "))) +
    facet_grid(length2 ~ year2)
}

plot_datras_aggregated(mysurvey=c("NS-IBTS"), myyear=1992:2018, myquarter=c(3), myspecies=c("cod"), mylength=c(10,50), cutlength=10, cutyear=3)
plot_datras_aggregated(mysurvey=c("NS-IBTS"), myyear=1992:2018, myquarter=c(1), myspecies=c("aru","ary","arg"), mylength=c(10,50), cutlength=10, cutyear=3)
plot_datras_aggregated(mysurvey=c("NS-IBTS"), myyear=1992:2018, myquarter=c(3), myspecies=c("aru","ary","arg"), mylength=c(10,50), cutlength=10, cutyear=3)
plot_datras_aggregated(mysurvey=c("BTS"), myyear=1992:2018, myquarter=c(3), myspecies=c("sol"), mylength=c(10,50), cutlength=10, cutyear=3)
