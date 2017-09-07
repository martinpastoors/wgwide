# library(devtools)
# install.packages("FLSAM", repos="http://flr-project.org/R")
library(FLSAM)
library(tidyverse)

lsf.str("package:FLCore")
lsf.str("package:FLSAM")

load("D:/ZZZ/WKPELA 2014 NEAmac.RData")

f   <- as.data.frame(harvest(NEA.mac)) %>% 
  mutate(var="f") %>%  
  filter(age %in% c(4:8)) %>% 
  group_by(year, unit, season, area, iter, var) %>% 
  summarise(data = mean(data)) %>% 
  mutate(age = "4-8") %>% 
  as.data.frame()
ssb <- as.data.frame(ssb(NEA.mac)) %>% mutate(var="ssb") 
r   <- as.data.frame(stock.n(NEA.mac)) %>% mutate(var="r") %>% filter(age==0) %>% mutate(age=as.character(age))

dplyr::bind_rows(f, ssb, r ) %>% 
  select(-age, -season, -unit, -iter, -area) %>% 
  spread(key=var, value=data) %>% 
  write.csv(., "d:/mac-nea 2014 benchmark.csv")


# sam.out(NEA.mac)
# summary(NEA.mac@stock.n)
# writeFLStock(NEA.mac, output.file="D:/neamac.txt")
