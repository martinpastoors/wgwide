################################################################################
# WGWIDE 2017 HOM assess analysis
#
# "D:\WGWIDE\2017\08. Personal folders\martin\hom\r\HOM assess analysis.r"
# 
# 04/09/2017 first code
################################################################################

rm(list=ls())

# Open libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(cowplot)

# library(devtools); install_github("einarhjorleifsson/ggmisc")
library(ggmisc)

options(max.print=999999)

path    <- "D:/WGWIDE/2017/08. Personal folders/martin/"
# try(setwd(path),silent=TRUE)

setwd(paste(path))

# Load utils code
source("D:/GIT/mptools/r/my_utils.r")

# ============================================================================
# load catch data (catch, )
# ============================================================================

rby <-
  readxl::read_excel("D:/Dropbox/ICES Assessment database/ICES Assessment Summary database.xlsx",
                     sheet = "DATA",
                     col_names = TRUE,
                     col_types = "text",
                     skip = 0) %>%
  lowcase %>%
  rename(assessmentyear = assyear) %>% 
  mutate(fishstock = tolower(fishstock)) %>%
  mutate_at(vars("year","assessmentyear"), funs(as.integer)) %>% 
  mutate_at(vars("lowrecruitment", "recruitment","highrecruitment",
                 "lowssb","ssb","highssb",
                 "lowf", "f","highf",
                 "landings","catches","discards","ibc",
                 "flim","fpa","fmsy", "fmanagement",
                 "blim","bpa","msybtrigger","bmanagement",
                 "recruitmentage","recruitmentlength"), funs(as.numeric)) %>% 
  select(-contains("custom")) %>% 
  rename(scientificname = speciesname,
         commonname     = sgname,
         fmgt           = fmanagement,
         bmgt           = bmanagement,
         faocode        = species) %>% 
  
  mutate_at(vars("stocksizedescription","stocksizeunits","fishingpressuredescription","fishingpressureunits",
                 "commonname", "scientificname"), funs(tolower)) %>% 
  filter(year <= assessmentyear) 
  
# -----------------------------------------------------------------------------
# Plot 
# -----------------------------------------------------------------------------

d <-
  rby %>% 
  filter(grepl("hom-west",fishstock)) %>% 
  filter(assessmentyear >= 2013) %>% 
  mutate(assessmenttype = "assess",
         assessmenttype = ifelse(assessmentyear == max(assessmentyear),"last",assessmenttype),
         assessmenttype = ifelse(grepl("bench",fishstock),"bench",assessmenttype) ) %>% 
  mutate(fishstock = gsub("-bench","",fishstock, fixed=TRUE),
         fishstock = gsub("-old","",fishstock, fixed=TRUE)) %>% 
  mutate(tyear     = ifelse(assessmenttype == "assess", as.character(assessmentyear), NA),
         tyear     = ifelse(assessmenttype == "last", paste(assessmentyear,sep="") ,tyear),
         tyear     = ifelse(assessmenttype == "old", paste(assessmentyear,"-O",sep="") ,tyear),
         tyear     = ifelse(assessmenttype == "bench", paste(assessmentyear,"-B",sep="") ,tyear)) %>% 
  data.frame()

scaler <-
  d %>% 
  filter(year == 2015) %>% 
  mutate(blim  = ifelse(assessmentyear == 2017 | assessmenttype == "bench", ssb, NA),
         btrig = ifelse(assessmentyear == 2017 | assessmenttype == "bench", 1.4*blim, 634577),
         fmsy  = ifelse(assessmentyear == 2017 | assessmenttype == "bench", 0.11, 0.13)) %>% 
  select(assessmentyear, assessmenttype, blim, btrig, fmsy) %>% 
  data.frame()

# plot ssb
p1 <-

  d %>% 
  filter(!is.na(ssb)) %>%  
  filter(assessmentyear >= 2015) %>% 
  left_join(scaler, by=c("assessmentyear", "assessmenttype")) %>% 
  mutate(ssb_btrig     = ssb/btrig,
         ssb_blim      = ssb/blim.y,
         f_fmsy        = f / fmsy.y) %>%
  select(fishstock, assessmentyear, assessmenttype, year, ssb, f, ssb_btrig, ssb_blim, f_fmsy, tyear,
         blim.x, blim.y, btrig, fmsy.x, fmsy.y) %>% 
  
  ggplot(aes(year,ssb_btrig, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  geom_hline(aes(yintercept=1, colour=assessmenttype), linetype="dashed") +
  geom_hline(aes(yintercept=blim.y/btrig, colour=assessmenttype), linetype="dotdash", size=0.8) +

  geom_text(aes(label  = "MSY Btrigger", colour = assessmenttype, x=2007, y=1.1), size=3, hjust=0) +
  geom_text(aes(label  = "Blim", colour = assessmenttype, x=2007, y=0.9*blim.y/btrig), size=3, hjust=0) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  xlim(2000,2018) + ylim(0,3) +
  labs(x = NULL, y = NULL , title = "SSB / MSY Btrigger") +
  facet_wrap(~tyear)


# plot f
p2 <-
  d %>% 
  filter(!is.na(f)) %>%  
  mutate(blim        = ifelse(assessmentyear == 2017, 805000, blim),
         msybtrigger = ifelse(assessmentyear == 2017, 805000*1.4, msybtrigger)) %>% 
  mutate(rel_ssb     = ssb/msybtrigger,
         rel_f       = f / fmsy) %>% 
  
  ggplot(aes(year,rel_f, group=tyear)) +
  
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        # strip.background = element_blank(),
        legend.position = "null") +
  
  geom_line(aes(colour = assessmenttype, size=assessmenttype, linetype=assessmenttype) ) +
  
  geom_dl(aes(label  = tyear, colour = assessmenttype), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  
  scale_colour_manual(values=c(last   = "red",
                               assess = "black",
                               bench  = "blue",
                               old    = "darkgreen")) +
  
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  
  scale_size_manual(values=c(last   = 1.5,
                             assess = 0.8,
                             bench  = 1.2,
                             old    = 0.8)) +
  
  expand_limits(y = 0) +
  xlim(2000,2018) + ylim(0,3) +
  geom_hline(aes(yintercept=1), linetype="dashed", colour="gray30") +
  
  labs(x = NULL, y = NULL , title = "F / Fmsy")   +
  facet_grid(fishstock ~ .)


plot_grid(p1 + theme(legend.position = "none", axis.title      = element_blank()), 
          p2 + theme(axis.title      = element_blank()),
          ncol=2, align = 'h', rel_widths = c(3,3))


# ---------------------------------------------------------------------------------------------
# Calculate Mohn's rho; first in code then in function
# ---------------------------------------------------------------------------------------------
# dataset with last year
d.last <- 
  d %>% 
  filter(assessmenttype %in% c("last")) %>% 
  select(fishstock, year, ssb_last = ssb) 

# dataset from other years
d.assess <- 
  d %>% 
  filter(assessmenttype %in% c("assess")) %>% 
  select(fishstock, assessmentyear, year, ssb) %>% 
  
  # merge the last year to each assessmentyear
  dplyr::left_join(d.last, by=c("fishstock","year")) %>% 
  
  # calculate yeardifference and filter for the number of required years. 
  mutate  (yeardiff = assessmentyear - year,
           rho      = (ssb - ssb_last) / ssb_last,
           rho_abs  = abs(rho)) 

# set number of years to include in calculation
n <- 1

# calculate average Mohn's rho over all selected assessment years
d.assess %>% 
  filter(yeardiff <= (n - 1)) %>% 
  
  # calculate mohn's rho
  group_by(fishstock) %>% 
  summarise(mrho     = sum(rho, na.rm=TRUE),
            mrho_abs = sum(rho_abs, na.rm=TRUE),
            n    = n()) %>% 
  mutate(mrho     = mrho / n,
         mrho_abs = mrho_abs / n)
