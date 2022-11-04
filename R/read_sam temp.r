# --------------------------------------------------------------------------------
# read_sam.r
#
# Read sam outputs and convert to tidy format
#
# 01/09/2018 First coding. 
# --------------------------------------------------------------------------------

library(stockassessment)

# devtools::install_github("einarhjorleifsson/ramsam")
library(readsam)
library(tidyverse)

# name           <- "WGWIDE2018.1"
# stockname      <- "mac.27.nea"
# assessmentyear <- 2018
# replacerecruit <- TRUE
# savepath       <- "D:/WGWIDE/2018 Meeting Docs/06. Data/mac.27.nea/output/tidy"

name           <- "plaice_m_run"
stockname      <- "ple.27.420"
assessmentyear <- 2021
replacerecruit <- FALSE

# get fit from assessment.org
fit  <- fishvice::sam_get_fit(name)
ibya <- fishvice::sam_ibya(fit) %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
rbya <- fishvice::fv_rbya(fit, scale=1)  %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
opr  <- fishvice::fv_opr(fit)   %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
#spaly <- fishvice::fv_rbx(fit)


ibya %>% 
  dplyr::select(year, age, oC, cW) %>% 
  mutate(catch = oC * cW) %>% 
  group_by(year) %>% 
  mutate(catchprop = catch / sum(catch)) %>% 
  ggplot(aes(x=year, y=catchprop)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line() +
  expand_limits(y=0) +
  facet_wrap(~age, scales="free_y") +
  labs(x="", y="", title=stockname)

rbya %>% 
  dplyr::select(year, age, sW, n) %>% 
  mutate(stock = sW * n) %>% 
  group_by(year) %>% 
  mutate(stockprop = stock / sum(stock)) %>% 
  ggplot(aes(x=year, y=stockprop)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_bar(aes(fill=as.character(age)), stat="identity") +
  expand_limits(y=0)  +
  guides(fill = guide_legend(nrow = 1)) +
  labs(fill='age', title = paste(stockname, "stock weight"))


ibya %>% 
  dplyr::select(year, age, oC, cW) %>% 
  mutate(catch = oC * cW) %>% 
  group_by(year) %>% 
  mutate(catchprop = catch / sum(catch)) %>% 
  ggplot(aes(x=year, y=catchprop)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_bar(aes(fill=as.character(age)), stat="identity") +
  expand_limits(y=0)  +
  guides(fill = guide_legend(nrow = 1)) +
  labs(fill='age', title = paste(stockname, "catch weight"))





