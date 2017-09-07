################################################################################
# WGWIDE 2017 HOM catch analysis
#
# "D:\WGWIDE\2017\08. Personal folders\martin\hom\r\HOM catch analysis.r"
# 
# 03/07/2017 Adapted from NSAS herring code
# 28/08/2017 Updated with new data and variables
# 28/08/2017 Updated with new csv output files
# 30/08/2017 Added southern horse mackerel
# 31/08/2017 Now plotting only the sampled catches
# 07/09/2017 Converted to Rmarkdown

################################################################################

rm(list=ls())

# library(devtools)
# install.packages("FLCore", repos="http://flr-project.org/R")

# Open libraries
# library(FLCore)
library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)
library(directlabels)
library(cowplot)

# library(devtools); install_github("einarhjorleifsson/ggmisc")
library(ggmisc)

options(max.print=999999)

path    <- "D:/WGWIDE/2017/08. Personal folders/gersom/"
# try(setwd(path),silent=TRUE)

setwd(paste(path))
# Load utils code
source("D:/XXX/PRF/r/my_utils.r")

# ============================================================================
# load catch data (catch, )
# ============================================================================

# generate file lists
catchfiles1  <- list.files(
  path       = paste(path,"outputs_intercatch_homw/catch_data/",sep=""),
  pattern    = "catch",
  recursive  = T, full.names = TRUE, ignore.case= TRUE)

catchfiles2  <- list.files(
  path       = paste(path,"outputs_intercatch_nshom/catch_data/",sep=""),
  pattern    = "catch",
  recursive  = T, full.names = TRUE, ignore.case= TRUE)

catchfiles <- c(catchfiles1, catchfiles2)
catchfiles <- catchfiles[!grepl("\\~",catchfiles)]

# read the data
# i <- 1
for (i in 1:length(catchfiles)){                                           
  
  print(paste(i, catchfiles[i], sep=" "))
  tmp <-  
    read_csv(file=catchfiles[i], col_names=TRUE) %>% 
    lowcase() %>% 
    mutate_at(names(.),funs(as.character))
  
  print(names(tmp))

  if (i==1) catch <- tmp else 
            catch <- rbind(catch,tmp)
} 

# set column types
catch <-
  catch %>% 
  mutate_at(c("year","season","caton","officiallandings","sampledcatch",
              "nooflengthsamples","nooflengthmeasured","noofagesamples", 
              "noagereadings"), funs(as.numeric) ) %>% 
  mutate(stock = ifelse(stock == "hom.27.3a4bc7d","hom-nsea",stock),
         area  = gsub("^27\\." , "", area),
         area  = gsub("\\.nshm", "", area),
         area  = gsub("\\."    , "", area),
         area  = substr(area,1,2)) %>% 
  data.frame()


# ============================================================================
# load by age files: canum, weight and length data
# ============================================================================

# generate file lists
byagefiles1  <- list.files(
  path       = paste(path,"outputs_intercatch_homw/catch_data/",sep=""),
  pattern    = "length",
  recursive  = T, full.names = TRUE, ignore.case= TRUE)

byagefiles2  <- list.files(
  path       = paste(path,"outputs_intercatch_nshom/catch_data/",sep=""),
  pattern    = "length",
  recursive  = T, full.names = TRUE, ignore.case= TRUE)

byagefiles <- c(byagefiles1, byagefiles2)
byagefiles <- byagefiles[!grepl("\\~",byagefiles)]

# read the data
# i <- 1
for (i in 1:length(byagefiles)){                                           
  
  print(paste(i, byagefiles[i], sep=" "))
  tmp <-  
    read_csv(file=byagefiles[i], col_names=TRUE) %>% 
    lowcase() %>% 
    mutate_at(names(.),funs(as.character))
  
  print(names(tmp))
  
  if (i==1) byage <- tmp else 
            byage <- rbind(byage,tmp)
} 

# set column types
byage <-
  byage %>% 
  mutate_at(c("year","season","caton","officiallandings","ageorlength",
              "canum","weca","leca","sampledcatch", "nooflengthsamples", 
              "nooflengthmeasured", "noofagesamples", 
              "noagereadings"), funs(as.numeric) ) %>% 
  mutate(stock = ifelse(stock == "hom.27.3a4bc7d","hom-nsea",stock),
         area  = gsub("^27\\." , "", area),
         area  = gsub("\\.nshm", "", area),
         area  = gsub("\\."    , "", area),
         area  = substr(area,1,2)) %>% 
  rename(age = ageorlength) %>% 
  data.frame()


# ============================================================================
# optional: read southern horse mackerel data
# ============================================================================

cn <- read_excel("hom-soth.xlsx", sheet = "canum", col_names = TRUE, skip = 0) %>% 
  gather(key=age, value=canum, a0:a11) %>% 
  mutate(age = as.numeric(gsub("a","", age)),
         canum   = 1000 * canum, 
         stock   ="hom-soth",
         area    = "9a")

wc <- read_excel("hom-soth.xlsx", sheet = "weca", col_names = TRUE, skip = 0) %>% 
  gather(key=age, value=weca, a0:a11) %>% 
  mutate(age = as.numeric(gsub("a","", age)),
         weca  = 1000*weca,
         stock   ="hom-soth",
         area    = "9a")

t <- cn %>%
  left_join(wc, by=c("stock","area","year","age")) %>% 
  data.frame()

ct <- read_excel("hom-soth.xlsx", sheet = "caton", col_names = TRUE, skip = 0) %>% 
  mutate(caton    = 1000*as.numeric(caton), 
         stock   ="hom-soth",
         area    = "9a") %>% 
  data.frame()

# ============================================================================
# combine data
# ============================================================================

rbya <-
  rbind.all.columns(byage, t) %>% 
  filter(year >= 2000)

rby <-
  rbind.all.columns(catch, ct) %>% 
  filter(year >= 2000)

# ============================================================================
# colour set
# ============================================================================

PAIRED <- rep(brewer.pal(12, "Paired"), 100)

# ============================================================================
# Catch by year and stock
# ============================================================================

rby %>% 
  group_by(stock, year) %>%
  summarise(catch = sum(caton, na.rm=TRUE)) %>% 
  # View()
  ggplot(aes(x=year, y=catch)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  geom_bar(aes(fill=(stock)), stat="identity") +
  scale_fill_manual(values = PAIRED[1:3]) +
  guides(fill = guide_legend(nrow = 1))

# ============================================================================
# Catch by year, major area and stock
# ============================================================================

rby %>% 
  mutate(area = substr(area,1,1)) %>% 
  group_by(year, area) %>%
  summarise(catch = sum(caton, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=year, y=catch)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  
# geom_bar(aes(fill=(area)), stat="identity") +
  geom_bar(aes(fill=(area)), stat="identity", position="fill") +
  
  scale_fill_manual(values = PAIRED[1:8]) +
  guides(fill = guide_legend(nrow = 1))

# ============================================================================
# Canum by year and stock
# ============================================================================

rbya %>% 
  group_by(stock, year) %>%
  summarise(canum = sum(canum, na.rm=TRUE)) %>% 
  
  # group_by(stock) %>% 
  # summarise(canum = mean(canum))

  ggplot(aes(x=year, y=canum)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  
  # geom_bar(aes(fill=(stock)), stat="identity") +
  geom_bar(aes(fill=(stock)), stat="identity", position="fill") +
  
  scale_fill_manual(values = PAIRED[1:3]) +
  guides(fill = guide_legend(nrow = 1))

# ============================================================================
# Canum by year and major area
# ============================================================================

rbya %>% 
  mutate(area = substr(area,1,1)) %>% 
  group_by(area, year) %>%
  summarise(canum = sum(canum, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=year, y=canum)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +

  # geom_bar(aes(fill=(area)), stat="identity") + 
  # labs(title="canum by division (thousands)")
  geom_bar(aes(fill=(area)), stat="identity", position="fill") +
  labs(title="canum by division (proportion)") + 
  
  scale_fill_manual(values = PAIRED[1:8]) +
  guides(fill = guide_legend(nrow = 1)) 

# ============================================================================
# Canum by year and age
# ============================================================================

rbya %>% 
  group_by(stock, age, year) %>%
  summarise(canum = sum(canum, na.rm=TRUE)) %>% 
  
  ggplot(aes(x=year, y=canum)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  
  # geom_bar(aes(fill=(area)), stat="identity") + 
  # labs(title="canum by division (thousands)")
  geom_bar(aes(fill=factor(age)), stat="identity", position="fill") +
  labs(title="canum by age (proportion)") + 
  
  facet_wrap(~stock) +
  
  scale_fill_manual(values = PAIRED[1:16]) +
  guides(fill = guide_legend(title="age", nrow = 1)) 


# ============================================================================
# Canum by year and group of ages
# ============================================================================

rbyg <-
  rbya %>% 
  mutate(agegroup = ifelse(age <= 3, "00-03",NA),
         agegroup = ifelse(age >= 4 & age <= 10, "04-10",agegroup),
         agegroup = ifelse(age >= 11, "11-15",agegroup)) %>% 
  group_by(stock, agegroup, year) %>%
  summarise(canum = sum(canum, na.rm=TRUE))

p1 <-
  rbyg %>% 
  ggplot(aes(x=year, y=canum)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  
  geom_bar(aes(fill=(agegroup)), stat="identity") +
  labs(title="canum by agegroup (thousands)") +

  facet_wrap(~stock, scales="free_y") +
  
  scale_fill_manual(values = PAIRED[1:16]) +
  guides(fill = guide_legend(nrow = 1)) 

p2 <-
  rbyg %>% 
  ggplot(aes(x=year, y=canum)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  
  geom_bar(aes(fill=(agegroup)), stat="identity", position="fill") +
  labs(title="canum by agegroup (proportion)") +
  
  facet_wrap(~stock, scales="free_y") +
  
  scale_fill_manual(values = PAIRED[1:16]) +
  guides(fill = guide_legend(nrow = 1)) 

plot_grid(p1 + theme(legend.position = "none", 
                     axis.title      = element_blank()), 
          p2 + theme(title           = element_blank(),
                     axis.title      = element_blank()),
          ncol=1, align = 'v', rel_heights = c(3,3))


# ============================================================================
# Crayola's of catch by stock-area
# ============================================================================

# plot by area
rbya %>%
  
  filter(sampledorestimated == "Sampled_Distribution") %>% 
  
  mutate(area = substr(area,1,1)) %>% 
  filter(area %in% c("4","6", "7","8","9")) %>% 
  mutate(area = paste(area, substr(stock,5,8))) %>% 
  group_by(area, year, age) %>% 
  summarise(value = sum(canum, na.rm=TRUE)) %>% 
  group_by(area, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  ungroup() %>% 
  data.frame() %>% 
  
  ggplot() +
  
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Horse mackerel relative catch at age") +
  facet_grid(age ~ area, scale = "free_y", switch = "y")


  
# ============================================================================
# Crayola by stock
# ============================================================================

rbya %>% 
  
  filter(stock == "hom-soth" | sampledorestimated == "Sampled_Distribution") %>% 
  
  group_by(stock, year, age) %>% 
  summarise(value = sum(canum, na.rm=TRUE)) %>% 
  group_by(stock, age) %>% 
  mutate(value = value/mean(value, na.rm=TRUE)) %>% 
  mutate(yc = year - age) %>% 
  ungroup() %>% 
  data.frame() %>% 
  
  ggplot() +
  
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_col(aes(year, value, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Horse mackerel relative catch at age") +
  facet_grid(age ~ stock, scale = "free_y", switch = "y")

# ============================================================================
# select areas for plotting
# ============================================================================

myareas <-
  rbya %>% 
  group_by(area) %>% 
  summarise(catch = sum(canum, na.rm=TRUE)) %>% 
  arrange(-catch) %>% 
  filter(catch > 1017495215) %>% 
  dplyr::select(area)

# ============================================================================
# calculate and plot proportion of 0-2 year HOM in the catch (by area)
# ============================================================================

t <-
  myareas %>% 
  left_join(rbya, by=c("area")) %>% 
  
  filter(stock == "hom-soth" | sampledorestimated == "Sampled_Distribution") %>% 
  
  mutate(age2 = (ifelse(age > 3, "4+",as.character(age)))) %>%
  mutate(area = ifelse(grepl("^8c", area), "8c", area)) %>% 
  group_by(year, area, age2) %>%
  summarise(catch = sum(canum, na.rm=TRUE)) %>% 
  arrange(age2)

p1 <-
  t %>% 
  ggplot(aes(x=year, y=catch)) +
  theme_publication() +
  geom_bar(aes(fill=as.character(age2)), stat="identity") +
  # geom_bar(aes(fill=as.character(age2)), stat="identity", position="fill") +
  scale_fill_manual(values = PAIRED[1:5]) +
  facet_wrap(~area)

p2 <-
  t %>% 
  ggplot(aes(x=year, y=catch)) +
  theme_publication() +
  geom_bar(aes(fill=as.character(age2)), stat="identity", position="fill") +
  scale_fill_manual(values = PAIRED[1:5]) +
  facet_wrap(~area)

plot_grid(p1 + theme(legend.position = "none", 
                     axis.title      = element_blank()), 
          p2 + theme(title           = element_blank(),
                     axis.title      = element_blank()),
          ncol=1, align = 'v', rel_heights = c(3,3))

# ============================================================================
# calculate and plot proportion of 0-2 year HOM in the catch (by area and by stock)
# ============================================================================

i <- 0
for (s in c("hom-nsea","hom-west","hom-soth")) {
  i <- i + 1
  
  # select data
  tmp <- 
    myareas %>% 
    left_join(rbya, by=c("area")) %>% 
    
    filter(stock == s) %>% 
    
    mutate(age2 = (ifelse(age > 3, 4,age))) %>%
    mutate(area = ifelse(grepl("^8c", area), "8c", area)) %>% 
    group_by(stock, year, area, age2) %>%
    summarise(catch = sum(canum, na.rm=TRUE)) %>% 
    arrange(age2)
    
  assign(
    paste("p", i, sep=""),
    
    # plot assigned to variable
    ggplot(data=tmp, aes(x=year, y=catch)) +
      theme_publication() +
      theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
      # geom_bar(aes(fill=as.character(age2)), stat="identity") + ylim(0,400000) +
      geom_bar(aes(fill=as.character(age2)), stat="identity", position="fill") +
      scale_fill_manual(name="age", values = PAIRED[1:5]) +
      facet_wrap(~area, ncol=4) +
      ggtitle(s)
    )
}

# cowplot to tie them together

plot_grid(plot_grid(p1 + theme(legend.position="none") + theme(axis.title=element_blank()), 
                    p3 + theme(legend.position="none") + theme(axis.title=element_blank()),
                    ncol=1),
          p2 + theme(axis.title=element_blank()) + theme(legend.position = c(0.9, 0.2), legend.direction = "vertical"), 
          ncol=2, align = 'h', rel_widths = c(1,4))

  
# ============================================================================
# plotting the mean weight at age by area
# ============================================================================

t <-
  rbya %>% 
  
  filter(stock == "hom-soth" | sampledorestimated == "Sampled_Distribution") %>% 
  filter(weca != 0) %>% 
  
  mutate(area = substr(area,1,1)) %>% 
  filter(area %in% c("4","6", "7","8","9")) %>% 
  group_by(year, age, area) %>% 
  mutate(weca = weighted.mean(weca,canum, na.rm=TRUE))

t %>% 
  ggplot(aes(x=year, y=weca)) +
  theme_publication() +
  geom_line(aes(colour=factor(age))) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~area, scales="free_y") +
  labs(title="mean weight at age by area")

t %>% 
  ggplot(aes(x=year, y=weca)) +
  theme_publication() +
  geom_point(aes(colour=area), size=0.5) +
  geom_line(aes(colour=area), size=0.2) +
  geom_smooth(aes(colour=area), method="lm", alpha=0.3) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~age, scales="free_y") +
  labs(title="mean weight at age by area (displayed by age)")

# ============================================================================
# plotting the mean length at age by area
# ============================================================================

t <-
  rbya %>% 
  
  filter(stock == "hom-soth" | sampledorestimated == "Sampled_Distribution") %>% 
  filter(leca != 0) %>% 
  
  mutate(area = substr(area,1,1)) %>% 
  filter(area %in% c("4","6", "7","8","9")) %>% 
  group_by(year, age, area) %>% 
  mutate(leca = weighted.mean(leca,canum, na.rm=TRUE))
  
t %>% 
  ggplot(aes(x=year, y=leca)) +
  theme_publication() +
  geom_line(aes(colour=factor(age))) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~area, scales="free_y")

t %>% 
  ggplot(aes(x=year, y=leca)) +
  theme_publication() +
  geom_point(aes(colour=area), size=0.5) +
  geom_line(aes(colour=area), size=0.2) +
  geom_smooth(aes(colour=area), method="lm", alpha=0.3) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~age, scales="free_y") +
  labs(title="mean length at age by area (displayed by age)")

# ============================================================================
# plotting the mean length at age vs mean weight at age
# ============================================================================

t <-
  rbya %>% 
  
  filter(stock == "hom-soth" | sampledorestimated == "Sampled_Distribution") %>% 
  filter(leca != 0, weca != 0) %>% 
  
  mutate(area = substr(area,1,1)) %>% 
  filter(area %in% c("4","6", "7","8","9")) %>% 
  group_by(year, season, age, area) %>% 
  # group_by(year, age, area) %>% 
  mutate(leca = weighted.mean(leca,canum, na.rm=TRUE),
         weca = weighted.mean(weca, canum, na.rm=TRUE))

t %>% 
  ggplot(aes(x=leca, y=weca)) +
  theme_publication() +
  geom_point(aes(colour=factor(age)), alpha=0.3) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_grid(season~area)

t %>% 
  ggplot(aes(x=leca, y=weca)) +
  theme_publication() +
  theme(panel.spacing  = unit(1, "mm")) +
  geom_point(aes(colour=factor(area)), alpha=0.3) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_grid(season~age, scales="free")

t %>% 
  ggplot(aes(x=leca, y=weca)) +
  theme_publication() +
  geom_point(aes(colour=area), size=0.5) +
  geom_line(aes(colour=area), size=0.2) +
  geom_smooth(aes(colour=area), method="lm", alpha=0.3) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~age, scales="free_y") 

# ============================================================================
# plotting the mean length at age by area
# ============================================================================

t <-
  rbya %>% 
  
  filter(stock == "hom-soth" | sampledorestimated == "Sampled_Distribution") %>% 
  filter(leca != 0) %>% 
  
  mutate(area = substr(area,1,1)) %>% 
  filter(area %in% c("4","6", "7","8","9")) %>% 
  group_by(year, age, area) %>% 
  mutate(leca = weighted.mean(leca,canum, na.rm=TRUE))

t %>% 
  ggplot(aes(x=year, y=leca)) +
  theme_publication() +
  geom_line(aes(colour=factor(age))) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~area, scales="free_y")

t %>% 
  ggplot(aes(x=year, y=leca)) +
  theme_publication() +
  geom_point(aes(colour=area), size=0.5) +
  geom_line(aes(colour=area), size=0.2) +
  geom_smooth(aes(colour=area), method="lm", alpha=0.3) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~age, scales="free_y") +
  labs(title="mean length at age by area (displayed by age)")


# check NL catch
rby %>% 
  group_by(stock, country, year, season, area) %>%
  summarise(catch = sum(caton, na.rm=TRUE)) %>% 
  filter(country=="Netherlands") %>% 
  filter(year %in% c(2015, 2016)) %>%
  filter(area == "7d") %>% 
  View()

  ggplot(aes(x=year, y=catch)) +
  theme_publication() +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  geom_bar(aes(fill=(stock)), stat="identity") +
  scale_fill_manual(values = PAIRED[1:3]) +
  guides(fill = guide_legend(nrow = 1))
