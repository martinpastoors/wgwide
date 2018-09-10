# ----------------------------------------------------------------------------------------------
# read_ss.r
# 
# Martin Pastoors (adapted from Piera Carpi and Einar Hjorleifsson)
# 
# 03/09/2017 small modifications (MP)
# 07/02/2018 added short term forecast exploration because of PELAC meeting
# 01/09/2018 converted to tidy format
# ----------------------------------------------------------------------------------------------

rm(list=ls())

library(r4ss)
library(tidyverse)

# source("R/SS3toFLStock_function.r")
source("R/SS_readdat_3.30.r")
source("R/SS_readctl_3.30.r")

path     <- "D:/WGWIDE/2018 Meeting Docs/06. Data/hom.27.2a4a5b6a7a-ce-k8/assess" 
datafile <- "data.ss_new"
ctrlfile <- "control.ss_new"

# Read the input data
t <- 
  SS_readdat_3.30(file.path(path, datafile)) 

# debug(SS_readctl_3.30)
c <-
  SS_readctl_3.30(file.path(path, ctrlfile))
# undebug(SS_readctl_3.30)

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
  dplyr::mutate(age = as.integer(gsub("a","", age)))

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

str(t)
  

names(t)
str(t)

# 2017 not in the datafile?


names((oC[grepl("^a", names(oC))]))
select_(.dots = v)

#-----------------------------------------------------------------------
# Creat and read FLStock object(s)
#-----------------------------------------------------------------------

dirSS3output <- path; stockname<-"WHmackerel"; fbar_amin<-1; fbar_amax<-10 
  
# stockname
stockname <- "WHmackerel"   # SS3 dynamics, 1 morph per year, quarterly time step (Rec in Q1)

# fbar ages
fbar_amin <- 1; fbar_amax <- 10

# Create an FLStock RData file and write to the standard path
SS3toFLStock(dirSS3output = path, 
             stockname    = stockname, 
             fbar_amin    = fbar_amin, 
             fbar_amax    = fbar_amax)

# Load the data
WHOM_2017  <- get(load("WHmackerel_SS3results.rdata"))


comp <- FLStocks(new_assessment = WHOM_2017, benchmark = WHOM_bench)
plot(comp)

# create data.frames
WHOM2017df  <- FLStock2rbya(WHOM_2017) %>% mutate(assess="WHB2017")

rbya <- rbind(WHOM2017df, WHOMbenchdf)

