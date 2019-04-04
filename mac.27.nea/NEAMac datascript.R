# --------------------------------------------------------------------------------
# NEA Mackerel datascript
#
# Thomas Brunel
# 
# 04/04/2019 Commenting the code (Martin Pastoors)
#
# --------------------------------------------------------------------------------

# read data files
cn<-read.ices(file.path("data", "cn.dat"))
cw<-read.ices(file.path("data", "cw.dat"))
dw<-read.ices(file.path("data", "dw.dat"))
lf<-read.ices(file.path("data", "lf.dat"))
lw<-read.ices(file.path("data", "lw.dat"))
mo<-read.ices(file.path("data", "mo.dat"))
nm<-read.ices(file.path("data", "nm.dat"))
pf<-read.ices(file.path("data", "pf.dat"))
pm<-read.ices(file.path("data", "pm.dat"))
sw<-read.ices(file.path("data", "sw.dat"))

# read survey data
surveys<-read.ices(file.path("data", "survey.dat"))

# read steel tagging data; keep only before or equal to recapture year 2006
recap1<-
  read.table(file.path("data", "tag.dat"), header=TRUE) %>% 
  mutate(
    age     = ReleaseY - Yearclass,
    yearout = RecaptureY - ReleaseY) %>% 
  filter(
    Type       == 1,
    age        > 1 & age < 12,
    RecaptureY <= 2006, 
    ReleaseY   >= min(as.numeric(rownames(sw))),
    yearout    > 0
  ) %>% 
  
  # specific condition: exclude recapture year 2018 (for IBP)
  filter(RecaptureY != 2018) 



# read RFID tagging data
recap2 <-
  read.table(file.path("data", "tag4.dat"), header=TRUE) %>% 
  mutate(
    r       = round(r),
    age     = ReleaseY - Yearclass,
    yearout = RecaptureY - ReleaseY) %>% 
  filter(
    Nscan    > 0,
    age      >= 5 & age < 12, 
    ReleaseY >= 2013,
    yearout  %in% c(1,2), 
    RecaptureY != 2018
  )  %>% 
  
  # specific condition: exclude recapture year 2018 (for IBP)
  filter(RecaptureY != 2018) 


# generate total recapture dataset  
recap<-rbind(recap1,recap2)

# Matrix W (for what?)
W <- matrix(NA,nrow=nrow(cn), ncol=ncol(cn))
W[as.numeric(rownames(cn))<2000]<-10
attr(cn,"weight")<-W



