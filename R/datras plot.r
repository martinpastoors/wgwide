# -----------------------------------------------------------------------------------------------
# getDatras.r 
#
# download some of the datras data and make tidy; plot simple survey plots
#
# 05/09/2017 First version based on code from Einar
# -----------------------------------------------------------------------------------------------

# library(devtools)
# install.packages("icesDatras")
# devtools::install_github("fishvice/tidyices")

library(icesDatras)   # facilities to download datatras
library(tidyices)     # codes to make datras data tidy
library(tidyverse)    # tidying packages

library(maps)
library(mapdata)
library(lubridate)
library(viridis)

# source my utils
source("D:/GIT/mptools/R/my_utils.r")

# -----------------------------------------------------------------------------------------------
# Download and save raw Datras data
# -----------------------------------------------------------------------------------------------

# getSurveyYearQuarterList("IE-IGFS",2016)

# raw_hh <- 
#             getDATRAS(record = "HH", survey = c("NS-IBTS"), quarters = c(1, 3), years = 1990:2017) %>% 
#   bind_rows(getDATRAS(record = "HH", survey = c("FR-CGFS"), quarters = c(4),    years = 1990:2016))
# 
# raw_hl <- 
#             getDATRAS(record = "HL", survey = c("NS-IBTS"), quarters = c(1, 3), years = 1990:2017) %>% 
#   bind_rows(getDATRAS(record = "HL", survey = c("FR-CGFS"), quarters = c(4), years = 1990:2016))
# 
# raw_ca <- 
#             getDATRAS(record = "CA", survey = c("NS-IBTS"), quarters = c(1, 3), years = 1990:2017) %>% 
#   bind_rows(getDATRAS(record = "CA", survey = c("FR-CGFS"), quarters = c(4), years = 1990:2016))


# ibtsq1_hh <- getDATRAS(record="HH",survey=c("NS-IBTS"), quarters=c(1), years=1990:2017)
# ibtsq3_hh <- getDATRAS(record="HH",survey=c("NS-IBTS"), quarters=c(3), years=1990:2016)
# cgfs_hh   <- getDATRAS(record="HH",survey=c("FR-CGFS"), quarters=c(4), years=1990:2016)
igfs_hh   <- getDATRAS(record="HH",survey=c("IE-IGFS"), quarters=c(4), years=2003:2016)
 
# ibtsq1_hl <- getDATRAS(record="HL",survey=c("NS-IBTS"), quarters=c(1), years=1990:2017)
# ibtsq3_hl <- getDATRAS(record="HL",survey=c("NS-IBTS"), quarters=c(3), years=1990:2016)
# cgfs_hl   <- getDATRAS(record="HL",survey=c("FR-CGFS"), quarters=c(4), years=1990:2016)
igfs_hl   <- getDATRAS(record="HL",survey=c("IE-IGFS"), quarters=c(4), years=1990:2016)
 
# ibtsq1_ca <- getDATRAS(record="CA",survey=c("NS-IBTS"), quarters=c(1), years=1990:2017)
# ibtsq3_ca <- getDATRAS(record="CA",survey=c("NS-IBTS"), quarters=c(3), years=1990:2016)
# cgfs_ca   <- getDATRAS(record="CA",survey=c("FR-CGFS"), quarters=c(4), years=1990:2016)
igfs_ca   <- getDATRAS(record="CA",survey=c("IE-IGFS"), quarters=c(4), years=1990:2016)

# save(ibtsq1_hh, file="D:/XXX/DATRAS/ibtsq1_hh.RData")
# save(ibtsq3_hh, file="D:/XXX/DATRAS/ibtsq3_hh.RData")
# save(cgfs_hh  , file="D:/XXX/DATRAS/cgfs_hh.RData")
save(igfs_hh  , file="D:/XXX/DATRAS/igfs_hh.RData")
 
# save(ibtsq1_hl, file="D:/XXX/DATRAS/ibtsq1_hl.RData")
# save(ibtsq3_hl, file="D:/XXX/DATRAS/ibtsq3_hl.RData")
# save(cgfs_hl  , file="D:/XXX/DATRAS/cgfs_hl.RData")
save(igfs_hl  , file="D:/XXX/DATRAS/igfs_hl.RData")

# save(ibtsq1_ca, file="D:/XXX/DATRAS/ibtsq1_ca.RData")
# save(ibtsq3_ca, file="D:/XXX/DATRAS/ibtsq3_ca.RData")
save(igfs_ca  , file="D:/XXX/DATRAS/igfs_ca.RData")

load("D:/XXX/DATRAS/ibtsq1_hh.RData")
load("D:/XXX/DATRAS/ibtsq3_hh.RData")
load("D:/XXX/DATRAS/cgfs_hh.RData")
load("D:/XXX/DATRAS/ibtsq1_hl.RData")
load("D:/XXX/DATRAS/ibtsq3_hl.RData")
load("D:/XXX/DATRAS/cgfs_hl.RData")
load("D:/XXX/DATRAS/ibtsq1_ca.RData")
load("D:/XXX/DATRAS/ibtsq3_ca.RData")

# raw_hh <- rbind(ibtsq1_hh, ibtsq3_hh, cgfs_hh, igfs_hh)
# raw_hh <- rbind(ibtsq1_hh, ibtsq3_hh, cgfs_hh)
raw_hh <- rbind(cgfs_hh)

# raw_hl <- rbind(ibtsq1_hl, ibtsq3_hl, cgfs_hl, igfs_hl)
# raw_hl <- rbind(ibtsq1_hl, ibtsq3_hl, cgfs_hl)
raw_hl <- rbind(cgfs_hl)

# raw_ca <- rbind(ibtsq1_ca, ibtsq3_ca, igfs_ca)
raw_ca <- rbind(ibtsq1_ca, ibtsq3_ca)

# -----------------------------------------------------------------------------------------------
# Create species codes dataset
# -----------------------------------------------------------------------------------------------

species <- get_latin(raw_hl)

# -----------------------------------------------------------------------------------------------
# save all raw datasets
# -----------------------------------------------------------------------------------------------

save(raw_hh, raw_hl, raw_ca, species, file = "D:/XXX/DATRAS/raw_data.RData")
load("D:/XXX/DATRAS/raw_data.RData")

# -----------------------------------------------------------------------------------------------
# Make data tidy
# -----------------------------------------------------------------------------------------------

hh <-
  raw_hh%>% 
  tidy_hauls()

hl <-
  raw_hl %>% 
  tidy_lengths(hh, species=species)

ca <- 
  raw_ca %>% 
  tidy_ages(species = species)

# -----------------------------------------------------------------------------------------------
# Now the plots
# -----------------------------------------------------------------------------------------------

le <- 
  hl %>%
  select(id, latin, length, n) %>% 
  group_by(id, latin, length) %>% 
  summarise(n = n())

st <- 
  hh %>% 
  select(id, survey, year, quarter, date, lon = shootlong, lat = shootlat) 

xlim <- range(st$lon)
ylim <- range(st$lat)
m <- map_data("worldHires", xlim = xlim, ylim = ylim)


# plot north sea
ns <-
  m %>% 
  ggplot() +
  theme_bw() +
  geom_polygon(data = m, aes(long, lat, group = group), fill = "grey") +
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_x_continuous(NULL, NULL) +
  scale_y_continuous(NULL, NULL)

# add line to exclude the northern north sea
myline <- data.frame(x=c(-3,8), y=c(52,60))

df <-
  le %>% 
  mutate(b = n * 0.01 * length^3) %>%
  
  # filter(latin %in% c("Trachurus trachurus")) %>% 
  # filter(latin %in% c("Raja clavata")) %>% 
  filter(latin %in% c("Clupea harengus")) %>% 
  filter(length >= 20) %>% 
  
  group_by(id, latin) %>% 
  summarise(N = sum(n),
            b = sum(b)) %>%
  ungroup() %>% 
  
  right_join(st) %>%
  filter(year >= 2003) %>% 
#  filter(year >= 1991) %>% 
#  filter(survey %in% c("NS-IBTS", "FR-CGFS"), quarter %in% c(1,4)) %>% 
#  filter(survey %in% c("NS-IBTS"), quarter %in% c(1)) %>% 
#  filter(survey %in% c("NS-IBTS"), quarter %in% c(3)) %>% 
filter(survey %in% c("IE-IGFS"), quarter %in% c(4)) %>% 
# filter(survey %in% c("FR-CGFS"), quarter %in% c(4)) %>% 
  
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
ns +
  geom_raster(data = df, aes(lon, lat, fill = B)) +
  scale_fill_viridis(option = "B", direction = -1) +
  # geom_line(data=myline, aes(x, y), colour="red") +
  ggtitle("Herring IBTS q3, >= 30 cm") +
  facet_wrap(~ year, ncol = 5)


# plot by length groups
le %>% 
  mutate(b = n * 0.01 * length^3) %>%
  filter(latin %in% c("Clupea harengus")) %>% 
  mutate(length = 5*floor(length/5)) %>% 

  group_by(id, length, latin) %>% 
  summarise(N = sum(n),
            b = sum(b)) %>%
  ungroup() %>% 
  right_join(st) %>%
  filter(year >= 2003) %>% 
  filter(survey %in% c("IE-IGFS"), quarter %in% c(4)) %>% 
  filter(lat < 52) %>% 
  
  mutate(year = year(date),
         N    = ifelse(is.na(N), 0, N),
         B    = ifelse(is.na(b), 0, b),
         sq   = encode_zchords(lon, lat, dx = 1)) %>% 
  
  group_by(length, year, latin) %>% 
  summarise(N = mean(N),
            B = mean(N)) %>% 
  filter(!is.na(latin)) %>% 
  
  filter(length >= 15) %>% 
  group_by(year, latin) %>% 
  summarize(N = mean(N)) %>% 
  
  ggplot(aes(year,N)) +
  geom_line() +
  facet_wrap(~length)


