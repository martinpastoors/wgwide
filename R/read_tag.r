# --------------------------------------------------------------------------------
# read_sam.r
#
# Read sam outputs and convert to tidy format
#
# 20180901 First coding. Some aspects can still be automated?
# --------------------------------------------------------------------------------

library(stockassessment)

# devtools::install_github("einarhjorleifsson/readsam")
 library(readsam)
library(tidyverse)

name           <- "WGWIDE2018.1"
stockname      <- "mac.27.nea"
assessmentyear <- 2018
replacerecruit <- TRUE
savepath       <- "D:/WGWIDE/2018 Meeting Docs/06. Data/mac.27.nea/assess/tidy"
  
# input-by-year-and-age, from assessment.org
ibya <- 
  readsam::read_ibya(name) %>% 
  mutate(
    stock = stockname, 
    assessmentyear = assessmentyear
  )

# input by year age and cohort (tagging data)
ibyac <- read_ibyac(name)

# get fit from assessment.org
f <- readsam:::get_fit(name)

# get summary data
fy        <- min(f$data$years)
ly        <- max(f$data$years)
fa        <- f$conf$minAge
la        <- f$conf$maxAge
fbarrange <- paste(f$conf$fbarRange, collapse="-")

# results-by-year-and-age
rbya <- 
  readsam::read_rbya_sam(f, ibya) %>% 
  mutate(
    stock          = stockname, 
    assessmentyear = assessmentyear,
    n              = ifelse(replacerecruit & age == fa & year == ly, NA, n)
  )

# results-by-year
rby <-
  readsam::read_rby_sam(f, ibya) %>% 
  mutate(
    year           = ifelse(variable == "catch", year+min(ibya$year), year),
    variable       = ifelse(variable == "fbar", paste0("fbar",fbarrange), variable),
    Estimate       = ifelse(replacerecruit & variable == "rec" & year == ly, NA, Estimate),
    Low            = ifelse(replacerecruit & variable == "rec" & year == ly, NA, Low),
    High           = ifelse(replacerecruit & variable == "rec" & year == ly, NA, High),
    variable       = ifelse(variable == "rec" , paste0("rec",fa), variable),
    stock          = stockname, 
    assessmentyear = assessmentyear
  ) 

  
# save file
save(ibya, rbya, rby, file=file.path(savepath, "tidy.RData"))

# plot rby
rby %>% 
  dplyr::filter(variable != "tsb") %>% 
  
  ggplot(aes(x=year, y=Estimate)) +
  theme_bw() +
  theme(legend.position = "none") +

  geom_ribbon(aes(ymin = Low, ymax=High, fill=variable), alpha=0.4) +
  geom_line(aes(colour=variable), size=1) +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y") +
  labs(x="", y="", title=stockname)






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

#' @export
read_ibyac <- function(ass, web = TRUE, user = "user3") {
  
  files <- c("tag.dat", "tag3.dat")
  res <- list()
  
  for(j in 1:length(files)) {
    
    if(web) {
      url <- paste("https://www.stockassessment.org/datadisk/stockassessment/userdirs",
                   user,
                   ass,
                   "data",
                   sep="/")
      fil <- readsam::get_file(url, files[j])
    } else {
      fil <- paste0(ass,
                    "/data/",
                    files[j])
    }
    
    res[[j]] <-
      stockassessment::read.ices(fil) %>%
      as.data.frame() %>%
      dplyr::mutate(year = row.names(.) %>% as.integer()) %>%
      tidyr::gather(age, value, -year, convert = TRUE) %>%
      dplyr::mutate(variable = var[j]) %>%
      dplyr::as_tibble()
  }
  
  res %>%
    dplyr::bind_rows() %>%
    dplyr::as_tibble() %>%
    return()
}
