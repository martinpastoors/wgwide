# --------------------------------------------------------------------------------
# compare.r
#
# code to compare input data of two assessments
#
# 20180901 First coding
# --------------------------------------------------------------------------------

# library(stockassessment)

# devtools::install_github("einarhjorleifsson/readsam")
# library(readsam)
library(tidyverse)
library(compareDF)     # install.packages("compareDF")

# source("D:/GIT/mptools/r/my_utils.r")

assnew  <- "WGWIDE2018.1"
assold <- "bw_2017_preliminary_2017_catch"
  
# input-by-year-and-age, from assessment.org
ibya <- readsam::read_ibya(assnew)
ibya2<- readsam::read_ibya(ass2)

ibya_diff <- compare_df(ibya, ibya2, c("year","age"))
ibya_diff$comparison_df

t <-
  ibya %>% 
  mutate(oU1b = oU1,
         oU3b = oU3 * mat * sW) %>%
  group_by(year) %>% 
  summarize(oU1b = sum(oU1b, na.rm=TRUE),
            oU3b = sum(oU3b, na.rm=TRUE)) %>% 
  filter(!(oU1b == 0 & oU3b == 0)) %>% 
  gather(key=var, value=value, oU1b:oU3b) %>%
  filter(value != 0) 

tt <-
  t %>% 
  filter(year >= 2010 & year <= 2016) %>% 
  group_by(var) %>% 
  summarize(mean = mean(value), 
            sd   = sd(value)) 

t %>% 
  left_join(tt, by="var") %>% 
  mutate(value2 = (value - mean) / sd ) %>% 
  
  ggplot(aes(x=year, y=value2, group=var)) +
  theme_publication() +
  geom_path(aes(colour=var)) +
  geom_point(aes(colour=var), size=2) +
  expand_limits(y=0)



path2017 <- "D:/WGWIDE/2017 Meeting docs/06. Data/mac.27.nea/user233-WGWIDE2017.V2/data"
path2018 <- "D:/WGWIDE/2018 Meeting Docs/06. Data/user233-WGWIDE2018.1/data"

#tag 2017
t <- 
  read.table(file.path(path2017, "tag.dat"), header=TRUE) %>% 
  filter(Type==1 & RecaptureY<=2006) %>% 
  filter(RecaptureY >= min(ibya$year, na.rm=TRUE))

tag2017 <- 
  read.table(file.path(path2017, "tag3.dat"), header=TRUE) %>% 
  mutate(r = round(r),
         Nscan = round(Nscan),
         R     = round(R)) %>% 
  filter(Nscan > 0) %>% 
  bind_rows(t)

# tag 2018
t <- 
  read.table(file.path(path2018, "tag.dat"), header=TRUE) %>% 
  filter(Type==1 & RecaptureY<=2006) %>% 
  filter(RecaptureY >= min(ibya$year, na.rm=TRUE))

tag2018 <- 
  read.table(file.path(path2018, "tag3.dat"), header=TRUE) %>% 
  mutate(r = round(r),
         Nscan = round(Nscan), 
         R     = round(R)) %>% 
  filter(Nscan > 0) %>% 
  bind_rows(t)

tag_diff <- compare_df(tag2017, tag2018, c("ReleaseY","RecaptureY", "Yearclass"))

# get fit from assessment.org
f <- readsam:::get_fit(ass)
f2 <- readsam:::get_fit(ass2)

# results-by-year-and-age
rbya <- readsam::read_rbya_sam(f, ibya)
rbya2 <- readsam::read_rbya_sam(f2, ibya2)

# results-by-year

rby <-
  readsam::read_rby_sam(f, ibya) %>% 
  mutate(year = ifelse(variable == "catch", year+1980, year)) %>% 
  filter(variable != "tsb") 
  
rby2 <-
  readsam::read_rby_sam(f2, ibya2) %>% 
  mutate(year = ifelse(variable == "catch", year+1980, year)) %>% 
  filter(variable != "tsb")


rby %>% 
  
  ggplot(aes(x=year, y=Estimate)) +
  theme_publication() +
  theme(legend.position = "none") +

  # last year
  geom_ribbon(data=rby2, aes(ymin = Low, ymax=High, fill=variable), alpha=0.2) +
  geom_line(data=rby2, aes(colour=variable), alpha=0.5) +
  
  # this year
  geom_ribbon(aes(ymin = Low, ymax=High, fill=variable), alpha=0.4) +
  geom_line(aes(colour=variable), size=1) +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y")
