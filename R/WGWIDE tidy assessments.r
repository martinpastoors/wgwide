# --------------------------------------------------------------------------------
# WGWIDE tidy assessment.r
#
# Read assessment output and convert to tidy format
#
# 01/09/2018 First coding.
# 25/08/2021 Updated with new fishvice package
# --------------------------------------------------------------------------------

library(stockassessment)
library(fishvice)  # devtools::install_github("einarhjorleifsson/fishvice")
library(tidyverse)
library(icesSAG)

assessmentyear <- 2022

datapath       <- paste0("C:/Users/Martin/Onedrive - PFA/Documents/iWGWIDE/",assessmentyear,"/06. Data")

lowcase <- function(df) {
  names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|\\(|\\)","",.) 
  df
}

# use token
options(icesSAG.use_token = TRUE)


# -----------------------------------------------------------------------------------------------
# Blue whiting
# -----------------------------------------------------------------------------------------------

name           <- paste0("BW-",assessmentyear) ; 
stockname      <- "whb.27.1-91214"

# get fit from assessment.org
fit <- fishvice::sam_get_fit(name)
# fit <- loadRData("C:/DATA/Onedrive - PFA/Documents/iWGWIDE/2020/06. Data/mac.27.nea/output/model fit.RData")
# stockassessment::saveConf(fit, file="temp.cfg")

# create FLStock object
fls <- FLfse::SAM2FLStock(fit, catch_estimate = FALSE) 

# tidy
ibya <- fishvice::sam_ibya(fit) %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
rbya <- fishvice::fv_rbya(fit, scale=1)  %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
opr  <- fishvice::fv_opr(fit)   %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
rby  <- fishvice::fv_rbx(fit)$rby  %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name) %>% 
  mutate(
    variable = ifelse(variable == "rec", "recruitment", variable),
    variable = ifelse(variable == "fbar", "fishingpressure", variable),
    variable = ifelse(variable == "ssb", "stocksize", variable)
  )

# unique(rby$variable)
# unique(rby2$variable)

# get from SAG
t1 <- getStockDownloadData(icesSAG::findAssessmentKey(stock=stockname, year=assessmentyear))[[1]] 

rby2 <-
  t1 %>% 
  lowcase() %>% 
  dplyr::select(year, 
                recruitment, high_recruitment, low_recruitment, 
                stocksize, high_stocksize, low_stocksize,
                fishingpressure, high_fishingpressure, low_fishingpressure,
                catch=catches) %>% 
  tidyr::pivot_longer(names_to = "var", values_to="value", recruitment:catch) %>% 
  tidyr::separate(var, into=c("estim","variable"), sep="_") %>% 
  dplyr::mutate(variable = ifelse(is.na(variable), estim, variable)) %>% 
  dplyr::mutate(estim = ifelse (estim==variable, "est", estim)) %>% 
  tidyr::pivot_wider(names_from = estim, values_from = value) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

# Or get from SAG
# rby  <- fishvice::fv_rby(fit)   %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

rp <-
  t1 %>% 
  lowcase() %>% 
  dplyr::distinct(flim, fpa, fmsy, bpa, blim, msybtrigger) %>% 
  dplyr::mutate(across(.cols=everything(), as.numeric)) %>% 
  tidyr::pivot_longer(names_to = "refpoint", values_to="value", flim:msybtrigger) %>% 
  dplyr::mutate(refpointtype = ifelse(grepl("^f", refpoint), "fishingpressure","stocksize")) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)



# save file rdata file
save(ibya, rbya, rby, opr, rp, file=file.path(datapath, stockname,"output", "tidy", "tidy.RData"))

# save csv files
write.csv(ibya, file=file.path(datapath, stockname,"output","tidy","ibya.csv"), row.names = FALSE)
write.csv(rbya, file=file.path(datapath, stockname,"output","tidy","rbya.csv"), row.names = FALSE)
write.csv(rby,  file=file.path(datapath, stockname,"output","tidy","rby.csv"), row.names = FALSE)
write.csv(opr,  file=file.path(datapath, stockname,"output","tidy","opr.csv"), row.names = FALSE)
write.csv(rp,   file=file.path(datapath, stockname,"output","tidy","rp.csv"), row.names = FALSE)


bind_rows(
  rby %>% mutate(source="rby"), 
  rby2 %>% mutate(source="rby2")) %>% 
  filter(!(variable %in% c("tsb"))) %>% 
  ggplot(aes(x=year,y=est)) +
  theme_bw() +
  geom_line(aes(colour=source)) +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y")


# -----------------------------------------------------------------------------------------------
# Mackerel
# -----------------------------------------------------------------------------------------------

name           <- paste0("MAC-",assessmentyear)
stockname      <- "mac.27.nea"

# get fit from local drive
fit <- load(file.path(datapath, stockname,"output", "model fit.RData") )
fit <- fit.new
# stockassessment::saveConf(fit, file="temp.cfg")

# tidy
ibya <- fishvice::sam_ibya(fit) %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
rbya <- fishvice::fv_rbya(fit, scale=1)  %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
opr  <- fishvice::fv_opr(fit)   %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)
#spaly <- fishvice::fv_rbx(fit)

# get from SAG
t1  <- getStockDownloadData(icesSAG::findAssessmentKey(stock=stockname, year=assessmentyear))[[1]]

rby <-
  t1 %>% 
  lowcase() %>% 
  dplyr::select(year, 
                recruitment, high_recruitment, low_recruitment, 
                stocksize, high_stocksize, low_stocksize,
                fishingpressure, high_fishingpressure, low_fishingpressure,
                catch=catches) %>% 
  tidyr::pivot_longer(names_to = "var", values_to="value", recruitment:catch) %>% 
  tidyr::separate(var, into=c("estim","variable"), sep="_") %>% 
  dplyr::mutate(variable = ifelse(is.na(variable), estim, variable)) %>% 
  dplyr::mutate(estim = ifelse (estim==variable, "est", estim)) %>% 
  tidyr::pivot_wider(names_from = estim, values_from = value) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

# Or get from SAM
# rby  <- fishvice::fv_rby(fit)   %>% dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

rp <-
  t1 %>% 
  lowcase() %>% 
  dplyr::distinct(flim, fpa, fmsy, bpa, blim, msybtrigger) %>% 
  dplyr::mutate(across(.cols=everything(), as.numeric)) %>% 
  tidyr::pivot_longer(names_to = "refpoint", values_to="value", flim:msybtrigger) %>% 
  dplyr::mutate(refpointtype = ifelse(grepl("^f", refpoint), "fishingpressure","stocksize")) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)


# save file rdata file
save(ibya, rbya, rby, opr, rp, file=file.path(datapath, stockname,"output", "tidy", "tidy.RData"))

# save csv files
write.csv(ibya, file=file.path(datapath, stockname,"output","tidy","ibya.csv"), row.names = FALSE)
write.csv(rbya, file=file.path(datapath, stockname,"output","tidy","rbya.csv"), row.names = FALSE)
write.csv(opr,  file=file.path(datapath, stockname,"output","tidy","opr.csv"), row.names = FALSE)

write.csv(rby,  file=file.path(datapath, stockname,"output","tidy","rby.csv"), row.names = FALSE)
write.csv(rp,   file=file.path(datapath, stockname,"output","tidy","rp.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------------------------
# Norwegian spring spawning herring
# -----------------------------------------------------------------------------------------------

# for now get summary from ICESsag and convert to tidy rby
name           <- paste0("HER-NOSS ", assessmentyear)
stockname      <- "her.27.1-24a514a"

t1  <- getStockDownloadData(icesSAG::findAssessmentKey(stock=stockname, year=assessmentyear))[[1]]

rby <-
  t1 %>% 
  lowcase() %>% 
  dplyr::select(year, 
                recruitment, high_recruitment, low_recruitment, 
                stocksize, high_stocksize, low_stocksize,
                fishingpressure, high_fishingpressure, low_fishingpressure,
                catch=catches) %>% 
  tidyr::pivot_longer(names_to = "var", values_to="value", recruitment:catch) %>% 
  tidyr::separate(var, into=c("estim","variable"), sep="_") %>% 
  dplyr::mutate(variable = ifelse(is.na(variable), estim, variable)) %>% 
  dplyr::mutate(estim = ifelse (estim==variable, "est", estim)) %>% 
  tidyr::pivot_wider(names_from = estim, values_from = value) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

rp <-
  t1 %>% 
  lowcase() %>% 
  dplyr::distinct(flim, fpa, fmsy, bpa, blim, msybtrigger) %>% 
  dplyr::mutate(across(.cols=everything(), as.numeric)) %>% 
  tidyr::pivot_longer(names_to = "refpoint", values_to="value", flim:msybtrigger) %>% 
  dplyr::mutate(refpointtype = ifelse(grepl("^f", refpoint), "fishingpressure","stocksize")) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

# save file rdata file
save(rby, rp, file=file.path(datapath, stockname,"output", "tidy", "tidy.RData"))

# save csv files
write.csv(rby,  file=file.path(datapath, stockname,"output","tidy","rby.csv"), row.names = FALSE)
write.csv(rp,   file=file.path(datapath, stockname,"output","tidy","rp.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------------------------
# Western horse mackerel
# -----------------------------------------------------------------------------------------------

# for now get summary from ICESsag and convert to tidy rby

name           <- paste0("HOM-WEST ",assessmentyear)
stockname      <- "hom.27.2a4a5b6a7a-ce-k8"

t1  <- getStockDownloadData(icesSAG::findAssessmentKey(stock=stockname, year=assessmentyear))[[1]]

rby <-
  t1 %>% 
  lowcase() %>% 
  dplyr::select(year, 
                recruitment, high_recruitment, low_recruitment, 
                stocksize, high_stocksize, low_stocksize,
                fishingpressure, high_fishingpressure, low_fishingpressure,
                catch=catches) %>% 
  tidyr::pivot_longer(names_to = "var", values_to="value", recruitment:catch) %>% 
  tidyr::separate(var, into=c("estim","variable"), sep="_") %>% 
  dplyr::mutate(variable = ifelse(is.na(variable), estim, variable)) %>% 
  dplyr::mutate(estim = ifelse (estim==variable, "est", estim)) %>% 
  tidyr::pivot_wider(names_from = estim, values_from = value) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

rp <-
  t1 %>% 
  lowcase() %>% 
  dplyr::distinct(flim, fpa, fmsy, bpa, blim, msybtrigger) %>% 
  dplyr::mutate(across(.cols=everything(), as.numeric)) %>% 
  tidyr::pivot_longer(names_to = "refpoint", values_to="value", flim:msybtrigger) %>% 
  dplyr::mutate(refpointtype = ifelse(grepl("^f", refpoint), "fishingpressure","stocksize")) %>% 
  dplyr::mutate(stock=stockname, assessmentyear=assessmentyear, assessmentname=name)

# save file rdata file
save(rby, rp, file=file.path(datapath, stockname,"output", "tidy", "tidy.RData"))

# save csv files
write.csv(rby,  file=file.path(datapath, stockname,"output","tidy","rby.csv"), row.names = FALSE)
write.csv(rp,  file=file.path(datapath, stockname,"output","tidy","rp.csv"), row.names = FALSE)
