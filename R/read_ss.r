# ----------------------------------------------------------------------------------------------
# read_ss.r
# 
# Martin Pastoors (adapted from Piera Carpi and Einar Hjorleifsson)
# 
# 03/09/2017 small modifications (MP)
# 07/02/2018 added short term forecast exploration because of PELAC meeting
# 01/09/2018 tried to convert to tidy format
# 07/09/2018 back to using the FLStock intermediate RData file
# ----------------------------------------------------------------------------------------------

rm(list=ls())

library(FLCore)
library(r4ss)
library(tidyverse)

# source("R/SS3toFLStock_function.r")
source("R/SS_readdat_3.30.r")
source("R/SS_readctl_3.30.r")
source("R/SS3toFLStock_v3.30.r")

# Load FLStock converters
source("../fishvise/R/converter.R")

path         <- "D:/WGWIDE/2018/06. Data/hom.27.2a4a5b6a7a-ce-k8" 
datafile     <- "data.ss_new"
ctrlfile     <- "control.ss_new"
wtatagefile  <- "wtatage.ss_new"
rdatafile    <- "WHOM_SS3results.rdata"

dat <- readLines(file.path(path, "output", wtatagefile),warn=FALSE)


header <- strsplit(dat[8]," ")[[1]]
data   <- strsplit(dat[9:length(dat)]," ")
str(data)


# Read the input data
t <- 
  SS_readdat_3.30(file.path(path, "output", datafile)) 

# Read the control file
c <-
  SS_readctl_3.30(file.path(path, "output", ctrlfile))

# load the rdata file
dat <- 
  get(load(file.path(path, "output", rdatafile)))

# create rbya data.frame from FLStock object - this is a shortcut; should be done directly from input files
rbya  <- 
  FLStock2rbya(dat) %>% 
  mutate(
    stock = "hom.27.2a4a5b6a7a-ce-k8",
    assessmentyear= 2018)

write.csv(rbya, file=file.path(path,"output","tidy", "rbya.csv"), row.names = FALSE)
# write.csv(rby,  file=file.path(savepath, "rby.csv"), row.names = FALSE)

library(icesSAG)

# use token
options(icesSAG.use_token = TRUE)

assessmentkey <- max(findAssessmentKey("hom.27.2a4a5b6a7a-ce-k8"))

# getStockDownloadData(7306)

# Download all the stock data - takes a long time
t1  <- getStockDownloadData(assessmentkey)


# Not finalized below 

# get summary data
fy        <- t$styr
ly        <- t$endyr
fa        <- min(t$agebin_vector)
la        <- max(t$agebin_vector)
fl        <- min(t$lbin_vector)
ll        <- max(t$lbin_vector)

# catch in number
oC <-
  t[["agecomp"]] %>% 
  as.data.frame() %>% 
  dplyr::filter(row_number() <= ly-fy & Lbin_lo == -1) %>% 
  dplyr::select(year=Yr, 
                names(.)[grepl("^a", names(.))]) %>% 
  tidyr::gather(key="age", value="oC", a0:a15) %>% 
  dplyr::mutate(age = as.integer(gsub("a","", age))) %>% 
  dplyr::arrange(year, age)

# catch weight and stock weight
if (t[["use_meanbodywt"]] == 0) {
  cW <- data.frame(expand.grid(year = fy:ly, age=fa:la), cW = as.numeric(NA), stringsAsFactors = F)
  dW <- data.frame(expand.grid(year = fy:ly, age=fa:la), dW = as.numeric(NA), stringsAsFactors = F)
  sW <- data.frame(expand.grid(year = fy:ly, age=fa:la), sW = as.numeric(NA), stringsAsFactors = F)
}

# natural mortality: just a fix now; need to find the parameter
m <- data.frame(expand.grid(year = fy:ly, age=fa:la), m = 0.15, stringsAsFactors = F)

# maturity
mat <- 0.1




