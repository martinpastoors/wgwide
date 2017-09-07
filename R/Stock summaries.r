# -------------------------------------------------------------------------------------------
# Summaries of stock trends in WGWIDE
#
# 03/09/2017 Martin Pastoors
#
# -------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(cowplot)

options(max.print=999999)

path    <- "Y:/2017 Meeting docs/06. Data/stock summaries"
setwd(path)

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

mac <- read_excel(path="stock summaries wgwide 2017.xlsx", sheet="mac", skip=2, col_names=T) %>% 
  lowcase() %>% 
  mutate(species = "mac") %>% 
  select(species, year, r, ssb, f) 

whb <- read_excel(path="stock summaries wgwide 2017.xlsx", sheet="whb", skip=2, col_names=T) %>% 
  lowcase() %>% 
  mutate(species = "whb") %>% 
  select(species, year, r, ssb, f) 

her <- read_excel(path="stock summaries wgwide 2017.xlsx", sheet="her", skip=2, col_names=T) %>% 
  lowcase() %>% 
  mutate(species = "her",
         ssb     = ssb * 1000,
         r       = r * 1000)%>% 
  select(species, year, r, ssb, f) 

hom <- read_excel(path="stock summaries wgwide 2017.xlsx", sheet="hom", skip=2, col_names=T) %>% 
  lowcase() %>% 
  mutate(species = "hom")%>% 
  select(species, year, r, ssb, f) 

summ <- rbind(mac, whb, her, hom)

# calculating averages
avg <-
  summ %>% 
  gather(key=var, value=value, r:f) %>% 
  group_by(species, var) %>% 
  summarise(mean    = mean(value, na.rm=TRUE),
            geomean = exp(mean(log(value), na.rm=TRUE)),
            median  = median(value, na.rm=TRUE)) %>% 
  mutate(mean = ifelse(var == "r", geomean, mean)) %>% 
  select(-geomean)

# plotting ssb
summ %>% 
  ggplot(aes(year, ssb)) +
  theme_publication() +
  # geom_line(aes(colour=species)) +
  geom_bar(aes(fill=species), colour=NA, stat="identity", position="stack")

summ %>% 
  filter(year >= 1988) %>% 
  
  ggplot(aes(year, ssb)) +
  theme_publication() +
  # geom_line(aes(colour=species)) +
  geom_bar(aes(fill=species), colour=NA, stat="identity", position="fill")

summ %>% 
  ggplot(aes(year, ssb)) +
  theme_publication() +
  # geom_line(aes(colour=species)) +
  geom_bar(aes(fill=species), colour=NA, stat="identity", position="stack") +
  facet_wrap(~species)


# plotting recruitment
summ %>% 
  ggplot(aes(year, r)) +
  theme_publication() +
  geom_bar(aes(fill=species), colour=NA, stat="identity") +
  geom_hline(data=filter(avg, var=="r"), aes(colour=species, yintercept = mean), linetype="dashed") +
  facet_wrap(~species)

# plotting F
summ %>% 
  ggplot(aes(year, f)) +
  theme_publication() +
  geom_line(aes(colour=species)) +
  facet_wrap(~species)



# plotting all together
summ %>% 
  gather(key=var, value=value, r:f) %>% 
  
  ggplot(aes(year, value)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=species)) +
  geom_hline(data=avg, aes(colour=species, yintercept = mean), linetype="dashed") +
  facet_grid(var~species, scales="free_y")
