# -----------------------------------------------------------------------------------------------
# Plot SAG
# 02/02/2018 first coding
# 08/02/2018 fixing lot's of small things in the excel spreadsheet
# 03/09/2018 based on SAGfull
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)
library(FLCore)

install.packages("D:/GIT/wg_HAWG/_Common/Pkgs/FLash_2.0.0.zip")

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# Load data
load(file=file.path(dropboxdir, "rdata/icesSAGfull 20180902.RData"))
# glimpse(icesSAGfull)

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

d <-
  icesSAGfull %>% 
  group_by(stockkey, assessmentyear) %>% 
  filter(assessmentkey == max(assessmentkey, na.rm=TRUE)) %>% 
  filter(purpose %in% c("Advice", "Benchmark")) %>% 
  group_by(stockkey) %>% 
  mutate(
    assessmenttype    = ifelse(assessmentyear == max(assessmentyear) & purpose %in% c("Advice"),
                               "Last",purpose),
    tyear             = substr(as.character(assessmentyear),3,4),
    tyear2            = paste0(substr(as.character(assessmentyear),3,4),substr(assessmenttype,1,1)),
    decade            = 10*floor(assessmentyear/10) ) 
  

mystock             <- "her-noss"
firstyear           <- 2000
firstassessmentyear <- 2010
myvar               <- "recruitment"

# ----------------------------------------------------------------------------------
# function for subplots
# ----------------------------------------------------------------------------------

history_plot <- function(mystock = "hom-west", myvar="stocksize", 
                         firstyear = 2000, firstassessmentyear = 2010,
                         firstrecruitmentyear = 2000) {
  
  mystock2 <- 
    unique(filter(d, stockkeylabelold == mystock)$stockkeylabelnew)
  
  d %>% 
    filter(grepl(mystock, stockkeylabelold)) %>% 
    filter(year >= firstyear) %>% 
    filter(assessmentyear >= firstassessmentyear) %>%
    mutate(check_recruitment = 
             ifelse(myvar == "recruitment", TRUE, FALSE),
           check_recruitment = 
             ifelse(check_recruitment & assessmentyear < firstrecruitmentyear, FALSE, TRUE)) %>% 
    filter(check_recruitment) %>% 

    # View()
    # distinct(unitofrecruitment)
  
    ggplot(aes(year,get(myvar), group=tyear)) +
    theme_publication() +
    theme(legend.title=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
          axis.text.y = element_text(size=9),
          legend.position = "null") +
    geom_ribbon(aes(fill = factor(assessmenttype), 
                    ymin = get(paste0("low", myvar)), 
                    ymax = get(paste0("high", myvar))), alpha=0.2) +
    geom_line(aes(colour = factor(assessmenttype)) ) +
    geom_dl(aes(label  = tyear, colour = factor(assessmenttype)), 
            method = list(dl.combine("last.points"), cex = 0.8)) +
    
    {if (myvar =="stocksize") geom_hline(aes(yintercept=mybtrigger), linetype="dashed", colour="black")} +
    {if (myvar =="stocksize") geom_text(aes(label="MSY Btrigger", x=firstyear, y=mybtrigger), 
                                        colour="black", hjust="inward", vjust=1)} +
    
    {if (myvar =="stocksize") geom_hline(aes(yintercept=myblim), linetype="solid", colour="black")} +
    {if (myvar =="stocksize") geom_text(aes(label="Blim", x=firstyear, y=myblim), 
                                        colour="black", hjust="inward", vjust = 1)} +
    
    {if (myvar =="fishingpressure") geom_hline(aes(yintercept=myfmsy), linetype="solid", colour="black")} +
    {if (myvar =="fishingpressure") geom_text(aes(label="Fmsy", x=firstyear, y=myfmsy), 
                                              colour="black", hjust="inward", vjust=1)} +
    
    scale_colour_manual  (values=c(Last = "red",Advice="black",Benchmark = "blue")) +
    scale_fill_manual    (values=c(Last = "red",Advice="white",Benchmark = "white")) +
    scale_linetype_manual(values=c(Last="solid",Advice="solid",Benchmark = "solid")) +
    scale_size_manual    (values=c(Last= 1.5,   Advice=0.8    ,Benchmark = 0.8)) +
    
    expand_limits(y = 0) +
    labs(x = NULL, y = NULL , title = paste0(myvar)) 
  
} # End of history_plot

mystock     <- "hom-west"
myblim      <- as.numeric(unique(filter(d, stockkeylabelold == mystock & assessmenttype == "Last")$blim))
mybtrigger  <- as.numeric(unique(filter(d, stockkeylabelold == mystock & assessmenttype == "Last")$msybtrigger))
myfmsy      <- as.numeric(unique(filter(d, stockkeylabelold == mystock & assessmenttype == "Last")$fmsy))


p1 <- history_plot(mystock = mystock, myvar = "stocksize", firstyear = 1980, firstassessmentyear = 2008)
p2 <- history_plot(mystock = mystock, myvar = "fishingpressure")
p3 <- history_plot(mystock = mystock, myvar = "recruitment", firstrecruitmentyear = 2017)
mystock2 <- unique(filter(d, stockkeylabelold == mystock)$stockkeylabelnew)

title_theme <- ggplot() + labs(title=mystock2)
plot_theme  <- plot_grid(p1, p2, p3, ncol=3, align = 'h')
plot_grid(title_theme, plot_theme, nrow=2, align = 'v', rel_heights = c(1, 20))

# sort(unique(icesSAGfull$stockkeylabelold))
# filter(d, grepl("whb", stockkeylabelold)) %>% View()

# plotting the end points of each assessment
p <-
  d %>% 
  filter(!is.na(stocksize)) %>%  
  filter(year >= 1990) %>%
  filter(assessmentyear >= 1991) %>% 
  filter(assessmenttype == "Advice") %>% 
  filter(grepl("mac-nea", stockkeylabelold)) %>% 
  filter(year == assessmentyear-1) %>% 
  select(stockkeylabelold, assessmentyear, year, tyear, stocksize, fishingpressure, recruitment) %>% 
  gather(key=variable, value=value, stocksize:recruitment) %>% 
  filter(variable %in% c("stocksize","fishingpressure")) %>% 
  
  # View()
  
  ggplot(aes(year, value)) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line() +
  geom_dl(aes(label  = tyear, colour = tyear), 
          method = list(dl.combine("last.points"), cex = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "ssb and F endpoints of assessments") +
  facet_wrap(~variable, scales="free_y")


d %>% 
  group_by(stockkey) %>% 
  filter(grepl(mystock, stockkeylabelold)) %>% 
  filter(year >= firstyear) %>% 
  filter(assessmentyear == max(assessmentyear)) 
