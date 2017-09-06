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

# ibtsq1_hh <- getDATRAS(record="HH",survey=c("NS-IBTS"), quarters=c(1), years=1990:2017)
# ibtsq3_hh <- getDATRAS(record="HH",survey=c("NS-IBTS"), quarters=c(3), years=1990:2016)
# cgfs_hh   <- getDATRAS(record="HH",survey=c("FR-CGFS"), quarters=c(4), years=1990:2016)
# 
# ibtsq1_hl <- getDATRAS(record="HL",survey=c("NS-IBTS"), quarters=c(1), years=1990:2017)
# ibtsq3_hl <- getDATRAS(record="HL",survey=c("NS-IBTS"), quarters=c(3), years=1990:2016)
# cgfs_hl   <- getDATRAS(record="HL",survey=c("FR-CGFS"), quarters=c(4), years=1990:2016)
# 
# ibtsq1_ca <- getDATRAS(record="CA",survey=c("NS-IBTS"), quarters=c(1), years=1990:2017)
# ibtsq3_ca <- getDATRAS(record="CA",survey=c("NS-IBTS"), quarters=c(3), years=1990:2016)
# cgfs_ca   <- getDATRAS(record="CA",survey=c("FR-CGFS"), quarters=c(4), years=1990:2016)

# save(ibtsq1_hh, file="D:/XXX/DATRAS/ibtsq1_hh.RData")
# save(ibtsq3_hh, file="D:/XXX/DATRAS/ibtsq3_hh.RData")
# save(cgfs_hh  , file="D:/XXX/DATRAS/cgfs_hh.RData")
# 
# save(ibtsq1_hl, file="D:/XXX/DATRAS/ibtsq1_hl.RData")
# save(ibtsq3_hl, file="D:/XXX/DATRAS/ibtsq3_hl.RData")
# save(cgfs_hl  , file="D:/XXX/DATRAS/cgfs_hl.RData")
# 
# save(ibtsq1_ca, file="D:/XXX/DATRAS/ibtsq1_ca.RData")
# save(ibtsq3_ca, file="D:/XXX/DATRAS/ibtsq3_ca.RData")

# -----------------------------------------------------------------------------------------------
# Load raw Datras data
# -----------------------------------------------------------------------------------------------

load("D:/XXX/DATRAS/ibtsq1_hh.RData")
load("D:/XXX/DATRAS/ibtsq3_hh.RData")
load("D:/XXX/DATRAS/cgfs_hh.RData")
load("D:/XXX/DATRAS/ibtsq1_hl.RData")
load("D:/XXX/DATRAS/ibtsq3_hl.RData")
load("D:/XXX/DATRAS/cgfs_hl.RData")
load("D:/XXX/DATRAS/ibtsq1_ca.RData")
load("D:/XXX/DATRAS/ibtsq3_ca.RData")

raw_hh <- 
  rbind(ibtsq1_hh, ibtsq3_hh, cgfs_hh)

raw_le <-
  rbind(ibtsq1_hl, ibtsq3_hl, cgfs_hl)

raw_ca <-
  rbind(ibtsq1_ca, ibtsq3_ca)

# -----------------------------------------------------------------------------------------------
# Create species codes dataset
# -----------------------------------------------------------------------------------------------

species <- 
  raw_le %>% 
  select(speccodetype = SpecCodeType, speccode = SpecCode) %>% 
  distinct() %>% 
  mutate(speccode = as.integer(speccode)) %>% 
  filter(!is.na(speccode))

tsn <- 
  species %>% 
  filter(speccodetype == "T")

out.tsn <- list()
for(i in 1:nrow(tsn)) {
  out.tsn[[i]] <- paste0("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=tsn&code=",
                         tsn$speccode[i]) %>% 
    icesDatras:::readDatras() %>% 
    icesDatras:::parseDatras()
}

aphia <- 
  species %>% 
  filter(speccodetype == "W")

out.aphia <- list()
for(i in 1:nrow(aphia)) {
  out.aphia[[i]] <- 
    paste0("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=aphia&code=",
           aphia$speccode[i]) %>% 
    icesDatras:::readDatras() %>% 
    icesDatras:::parseDatras()
}

species2 <-
  bind_rows(out.tsn) %>% 
  bind_rows(bind_rows(out.aphia)) %>% 
  as_tibble() %>% 
  select(aphia, tsn, latin = latinname) %>% 
  distinct() %>% 
  gather(speccodetype, speccode, aphia:tsn) %>% 
  mutate(speccodetype = ifelse(speccodetype == "aphia", "W", "T")) %>% 
  filter(speccode > 0,
         !is.na(speccode)) %>% 
  mutate(latin = ifelse(latin == "Torpedo (Torpedo) marmorata", "Torpedo marmorata", latin)) %>% 
  distinct()

raw_le.tmp <- raw_le
colnames(raw_le.tmp) <- tolower(colnames(raw_le.tmp))
raw_le.tmp <-
  raw_le.tmp %>% 
  left_join(species2)
nrow(raw_le) - nrow(raw_le.tmp)


valid_aphia <- 
  raw_le.tmp %>% 
  filter(is.na(latin)) %>% 
  select(valid_aphia) %>% 
  mutate(valid_aphia = as.integer(valid_aphia),
         speccodetype = "W") %>% 
  distinct() %>% 
  drop_na()

out.aphia2 <- list()
for(i in 1:nrow(valid_aphia)) {
  out.aphia2[[i]] <- 
    paste0("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSpecies?codename=aphia&code=",
           valid_aphia$valid_aphia[i]) %>% 
    icesDatras:::readDatras() %>% 
    icesDatras:::parseDatras()
}

species3 <-
  out.aphia2 %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  select(valid_aphia = aphia, latin = latinname) %>%
  distinct()

x.defined   <- raw_le.tmp %>% filter(!is.na(latin))
x.undefined <- raw_le.tmp %>% filter( is.na(latin)) %>% select(-latin) %>% left_join(species3)
x           <- bind_rows(x.defined, x.undefined)

# x  %>% filter(is.na(latin)) %>% 
#   group_by(year) %>% 
#   count() %>% as.data.frame()
# 
# x %>% filter(is.na(latin)) %>% 
#   select(speccodetype, speccode, valid_aphia) %>% 
#   as_tibble() %>% 
#   drop_na() %>% 
#   distinct()

species_code <- 
  x %>% 
  select(SpecCodeType = speccodetype, SpecCode = speccode, Valid_Aphia = valid_aphia, latin) %>% 
  filter(!is.na(latin)) %>% 
  distinct()

save(species_code,file="D:/XXX/DATRAS/species_code.RData")
load("D:/XXX/DATRAS/species_code.RData")

# -----------------------------------------------------------------------------------------------
# Make data tidy
# -----------------------------------------------------------------------------------------------

hh <-
  raw_hh%>% 
  tidy_hauls()

hl <-
  raw_le %>% 
  tidy_lengths(hh, species=species_code)

ca <- 
  raw_ca %>% 
  tidy_ages(species = species_code)

# ca <-
#   icesDatras::getDATRAS(record = "CA",
#                         survey = "NS-IBTS",
#                         years = yrs,
#                         quarters = qs) %>%
#   tidy_ages(species = species_code)

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

df <-
  le %>% 
  mutate(b = n * 0.01 * length^3) %>%
  
  filter(latin %in% c("Trachurus trachurus")) %>% 
  
  group_by(id, latin) %>% 
  summarise(N = sum(n),
            b = sum(b)) %>%
  ungroup() %>% 
  
  right_join(st) %>% 
  filter(survey %in% c("NS-IBTS", "FR-CGFS")) %>%
  filter(quarter %in% c(3,4)) %>% 
  mutate(year = year(date),
         N = ifelse(is.na(N), 0, N),
         B = ifelse(is.na(b), 0, b),
         sq = encode_zchords(lon, lat, dx = 1)) %>% 
  filter(year >= 1990) %>% 
  group_by(sq, year, latin) %>% 
  summarise(N = mean(N),
            B = mean(N)) %>% 
  separate(sq, c("lon", "lat"), sep = ":", convert = TRUE) %>% 
  filter(!is.na(latin))

# Create final plot
ns +
  geom_raster(data = df, aes(lon, lat, fill = B)) +
  scale_fill_viridis(option = "B", direction = -1) +
  ggtitle("IBTS q3 + CGFS, q4") +
  facet_wrap(~ year, ncol = 8)

