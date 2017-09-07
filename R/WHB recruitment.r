# blue whiting recruitment

library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(cowplot)

options(max.print=999999)

path    <- "D:/WGWIDE/2017/05. Presentations"
# try(setwd(path),silent=TRUE)

setwd(paste(path))

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

t <- read_excel(path=paste(path,"BW_RecruitmentRank17.xlsx", sep="/"),
                sheet="Standardized", range="A1:I38") %>% 
  lowcase() %>% 
  gather(key=survey, value=index,barsea1:sam)

t %>% 
  ggplot(aes(yrclass, index)) +
  theme_publication() +
  geom_point()+
  facet_wrap(~survey)

t %>% 
  filter(yrclass >= 2000) %>% 
  
  ggplot(aes(yrclass, index)) +
  theme_publication() +
  geom_smooth(method = "loess", span=0.1, alpha=0.3) +
  geom_point(aes(colour=survey), alpha=0.6)

