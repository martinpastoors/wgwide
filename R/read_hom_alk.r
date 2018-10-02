# ----------------------------------------------------------------------------------------------
# read_hom_alk.r
# 
# Martin Pastoors 
# 
# 01/10/2018 first coding
# ----------------------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(readxl)
library(ggridges)

source("../mptools/R/my_utils.R")

t  <-
  read_excel("D:/WGWIDE/2018/06. Data/hom.27.2a4a5b6a7a-ce-k8/data/c-a-a-a.downweight.age-comps.xlsx", 
             sheet = "new c-a-a-l",
             range = cell_cols("A:Y"),
             col_names=TRUE)  %>% 
  lowcase() %>% 
  setNames(gsub("#yr","year", names(.))) %>%
  rename(length = lbinlo) %>% 
  gather(key="age", value="catch", a0:a15)   %>% 
  mutate(age = as.integer(gsub("a","",age))) %>%
  # mutate(catch = ifelse(catch==0, NA, catch)) %>% 
  filter(length >= 10) %>% 
  
  group_by(year, length) %>% 
  mutate(prop = catch / sum(catch, na.rm=TRUE))



t %>% 
  ggplot(aes(x=length, y=age)) +
  theme_publication() +
  geom_point(aes(size=prop, colour=as.factor(age))) +
  facet_wrap(~year)

t %>% 
  group_by(length) %>% 
  mutate(prop = catch / sum(catch, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=length, y=prop)) +
  theme_publication() +
  geom_bar(aes(fill=as.factor(age)), stat="identity") 

t %>% 
  filter(year == 2015) %>% 
  
  ggplot(aes(x=length, y=age, height=prop)) +
  theme_publication() +
  geom_ridgeline(stat="identity", scale=6, alpha=0.4) +
  facet_wrap(~year)

