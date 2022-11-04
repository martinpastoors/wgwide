# ============================================================================
# 00 SAM HOM-WEST + simulator
# ============================================================================

rm(list=ls())
gc()

# install.packages("FLCore", repos="http://flr-project.org/R")
library(FLCore)

# install.packages("ggplotFL", repos="http://flr-project.org/R")
library(ggplotFL)

library(TMB)

# devtools::install_github('fishfollower/SAM/stockassessment') 
library(stockassessment)

library(tidyverse)

source("D:/GIT/wk_WKREBUILD/EqSimWHM/R/sam_fit2.r")
source("D:/GIT/wk_WKREBUILD/EqSimWHM/R/utilities.r")
source("D:/GIT/wk_WKREBUILD/EqSimWHM/R/get_dropbox.r")

basedir  <- "C:/TEMP"
sao.name <- "WHOM_2022"

tempdir  <- file.path(basedir,sao.name) 

dir.create(tempdir)
dir.create(file.path(tempdir, "run"))

fit            <- fishvice::sam_get_fit(sao.name)

dat            <- fit$dat
conf           <- fit$conf
par            <- defpar(fit$dat,conf)
rho            <- 0.999999 
par$itrans_rho <- -log(2/(1+rho)-1)/2

# set simulator properties
nsim     <- 1000
set.seed(123)

# re estimate fit
print("running initial SAM")
start_time <- Sys.time()
fit        <- sam.fit(dat,
                      conf,
                      par, 
                      map = list(itrans_rho = as.factor(NA))) # fix catch selectivity to be the same (rho=1)
end_time <- Sys.time()
duration <- as.numeric(difftime(end_time, start_time, units="secs"))
d        <- ((nsim-1) * duration)/3600

simruns <- base::data.frame(
  assess    = sao.name,
  iter       = 1,
  starttime = start_time,
  endtime   = end_time,
  duration  = duration,
  aic       = as.numeric(AIC(fit)),
  convergence = fit$opt$convergence,
  allSDnotNA  = all(!is.na(unlist(fit$plsd[which(names(fit$plsd)%in%names(fit$sdrep$par.fixed))]))),
  maxgradient = max(fit$sdrep$gradient.fixed),
  inFLStock   = TRUE,
  stringsAsFactors = FALSE)
save(simruns, file=file.path(tempdir, "run", paste0(sao.name,"_simruns.RData")))


retro          <- retro(fit, year=5)

rby  <- 
  fishvice::fv_rbx(fit)$rby  %>% 
  mutate(
    variable = ifelse(variable == "rec", "recruitment", variable),
    variable = ifelse(variable == "fbar", "fishingpressure", variable),
    variable = ifelse(variable == "ssb", "stocksize", variable)
  ) %>% 
  mutate(source="SAO") 

rby_ret <- rby %>% mutate(ly=2022)

for (i in 1:length(retro)) {
  rby_ret  <- 
    bind_rows(
      rby_ret,
      fishvice::fv_rbx(retro[[i]])$rby  %>% 
        mutate(
          variable = ifelse(variable == "rec", "recruitment", variable),
          variable = ifelse(variable == "fbar", "fishingpressure", variable),
          variable = ifelse(variable == "ssb", "stocksize", variable)
        ) %>% 
        mutate(source="SAO") %>% 
        mutate(i = i) %>% 
        mutate(ly = max(year, na.rm=TRUE))
    )
}

rp <-
  rby %>% 
  filter(year == 2003, variable=="stocksize") %>% 
  rename(bpa = est) %>% 
  mutate(blim = bpa/1.4)

rby_ret %>% 
  filter(variable == "stocksize") %>% 
  ggplot(aes(x=year, y=est)) +
  theme_publication() +
  geom_line(aes(colour=as.character(ly)), size=0.8) +
  # geom_line(data=rby %>% filter(variable=="stocksize"), colour="black") +
  geom_hline(data=rp, aes(yintercept = blim), linetype="solid", colour="black") +
  geom_hline(data=rp, aes(yintercept = bpa), linetype="dashed", colour="black") +
  geom_text (data=rp, aes(x=2022, y = blim), label="blim", colour="black", vjust=1) +
  geom_text (data=rp, aes(x=2022, y = bpa), label="bpa", colour="black", vjust=0, nudge_y=100000) +
  expand_limits(y=0) +
  labs(y="ssb", colour="last year") +
  guides(colour = guide_legend(nrow=1))
  




# # should be 0 if the model has converged
# fit$opt$convergence 
# # You can extract the standard errors of the fixed parameters with:
# all(!is.na(unlist(fit$plsd[which(names(fit$plsd)%in%names(fit$sdrep$par.fixed))])))
# # The max gradient should be closed to 0, the threshold is a bit subjective but < 1e-4 is already good:
# max(fit$sdrep$gradient.fixed) 

# read input in VPA file format
inputs   <- c(landings.fraction='lf.dat', catch.n='cn.dat', catch.wt='cw.dat',
              discards.wt='dw.dat', landings.wt='lw.dat', stock.wt='sw.dat',
              mat='mo.dat', m='nm.dat', harvest.spwn='pf.dat', m.spwn='pm.dat')
fqs <- as.list(inputs)
for (i in seq(inputs)) {
  file <- file.path(tempdir, "data",inputs[i])
  fqs[[names(inputs[i])]] <- readVPAFile(file)
}
  
minage       <- fit$data$minAge[[1]]
maxage       <- fit$data$maxAge[[1]]
pg           <- ifelse(fit$conf$maxAgePlusGroup[1]==1,fit$data$maxAge[[1]],NA)
minyear      <- min(fit$data$years)
maxyear      <- max(as.numeric(colnames(fqs[[1]])))  # max year from data
maxyearsam   <- max(fit$data$years)            # max year from assessment
minfbar      <- fit$conf$fbarRange[1]
maxfbar      <- fit$conf$fbarRange[2]

# Generate FLStock object
FLS  <- FLStock()

# set generate properties
FLS@desc               <- paste("FLStock object generated from SAM:", date(), sep=" ")
FLS@name               <- sao.name
FLS@range["min"]       <- minage
FLS@range["max"]       <- maxage
FLS@range["plusgroup"] <- pg
FLS@range["minyear"]   <- minyear
FLS@range["maxyear"]   <- maxyear
FLS@range["minfbar"]   <- minfbar
FLS@range["maxfbar"]   <- maxfbar

units(FLS)             <- FLCore::standardUnits(FLS)

FLS@catch.n            <- fqs[["catch.n"]] 
FLS@landings.n         <- fqs[["catch.n"]] * fqs[["landings.fraction"]]
FLS@discards.n         <- fqs[["catch.n"]] * (1-fqs[["landings.fraction"]])
FLS@m                  <- fqs[["m"]] %>% FLCore::window(., end=maxyear)
FLS@mat                <- fqs[["mat"]] %>% FLCore::window(., end=maxyear)
FLS@catch.wt           <- fqs[["catch.wt"]]
FLS@landings.wt        <- fqs[["landings.wt"]]
FLS@discards.wt        <- fqs[["discards.wt"]]
FLS@stock.wt           <- fqs[["stock.wt"]] %>% FLCore::window(., end=maxyear)
FLS@harvest.spwn       <- fqs[["harvest.spwn"]] %>% FLCore::window(., end=maxyear) 
FLS@m.spwn             <- fqs[["m.spwn"]] %>% FLCore::window(., end=maxyear)
FLS@landings <- FLS@discards <- FLS@catch  <- FLS@stock <-
                         FLQuant(NA, dimnames=list(age="all", year=minyear:maxyear))
FLS@landings          <- quantSums(FLS@catch.n * FLS@catch.wt)
FLS@discards          <- quantSums(FLS@discards.n * FLS@discards.wt)
FLS@catch             <- quantSums(FLS@catch.n * FLS@catch.wt)

# stock numbers
FLS@stock.n          <- FLQuant(NA, dimnames=list(age=minage:maxage, year=minyear:maxyearsam))
FLS@stock.n[,]       <- exp(fit$pl$logN) 
FLS@stock.n          <- FLCore::window(FLS@stock.n, end=maxyear)

# harvest
n.ages               <- nrow(fit$pl$logF)
FLS@harvest          <- FLQuant(NA, dimnames=list(age=minage:maxage, year=minyear:maxyearsam))
FLS@harvest[minage:(minage+n.ages),] <- exp(fit$pl$logF)
FLS@harvest[(n.ages+1),]    <- FLS@harvest[n.ages,]
FLS@harvest          <- FLCore::window(FLS@harvest, end=maxyear)
units(FLS@harvest)   <-  "f"

# ssb
FLS@stock            <- ssb(FLS)

# Save FLstock object
# save(FLS, file="run/WHOM_SAM20_FLS_WGWIDE.RData")
save(FLS, file=file.path(tempdir, "run",paste0(sao.name,"_FLS_WGWIDE.RData")))

# validObject(FLS)
# plot(FLS)
# plot(FLS@catch)

# Generate FLstock object with iterations
FLSs <- propagate(FLS, nsim)

# Simulate the observations
simdata <- simulate(fit, nsim=2*nsim,  full.data=TRUE)
simruns <- data.frame(stringsAsFactors = FALSE)

# iterations. start with 2, 1 is the real observation.
i  <- 2
it <- 2

while (i <= nsim) {
  
  start_time <- Sys.time()
  
  # sim_fit <-  sam.fit(simdata[[it]], conf, par)
  sim_fit  <- try(sam.fit2(simdata[[it]], conf, par, silent=TRUE), TRUE)
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units="secs"))
  d        <- (it/i * (nsim-i) * mean(simruns$duration))/3600
  
  if(grepl("Error",sim_fit)[1]==TRUE) {
    
    # error
    print(paste("trial", it, ";",
                "iter", i, "of", nsim, ";",  
                round(as.numeric(duration), digits=0), "sec;",
                as.character(sim_fit), 
                sep=" "))
    
    df       <- base::data.frame(
      assess    = sao.name,
      iter       = i,
      starttime = start_time,
      endtime   = end_time,
      duration  = round(end_time - start_time, digits=0),
      error     = as.character(sim_fit),
      inFLStock = FALSE,
      stringsAsFactors = FALSE)
    simruns <- bind_rows(simruns, df)
    # save(simruns, file="run/simruns.RData")
    save(simruns, file=file.path(tempdir, "run", paste0(sao.name,"_simruns.RData")))
    # save(simruns, file=paste0("run/",namestring,"_simruns.RData"))

  } else {
      
    if(sim_fit$opt$convergence==0) {
      
      # stock numbers
      stock.n                 <- FLQuant(NA, dimnames=list(age=minage:maxage, year=minyear:maxyearsam))
      stock.n[,]              <- exp(sim_fit$pl$logN) 
      stock.n                 <- FLCore::window(stock.n, end=maxyear)
      FLSs[,,,,,i]@stock.n    <- stock.n
      
      # harvest
      n.ages                  <- nrow(sim_fit$pl$logF)
      harvest                 <- FLQuant(NA, dimnames=list(age=minage:maxage, year=minyear:maxyearsam))
      harvest[minage:(minage+n.ages),] <- exp(sim_fit$pl$logF)
      harvest[(n.ages+1),]    <- harvest[n.ages,]
      FLSs[,,,,,i]@harvest    <- FLCore::window(harvest, end=maxyear)
      units(FLSs[,,,,,i]@harvest)   <-  "f"
      
      # ssb
      FLSs[,,,,,i]@stock            <- ssb(FLSs[,,,,,i])
      
      # save(FLSs, file="run/FLSs.RData")
      # save(FLSs, file=paste0("run/",namestring,"_FLS_converged.RData"))
      save(FLSs, file=file.path(tempdir, "run", paste0(sao.name,"_FLSs.RData")))
      save(FLSs, file=file.path(tempdir, "run", paste0(sao.name,"_FLS_converged.RData")))
      
      print(paste("trial", it, ";",
                  "iter", i, "of", nsim, ";",  
                  round(as.numeric(duration), digits=0), "sec;",
                  "converged = ", sim_fit$opt$convergence, ";",
                  round(d, digits=2), "hours remaining", sep=" "))
      
      i = i + 1
      
    } else {
      
      # non converged
      print(paste("trial", it, ";",
                  "iter", i, "of", nsim, ";",  
                  round(as.numeric(duration), digits=0), "sec;",
                  "converged = ", sim_fit$opt$convergence, ";",
                  round(d, digits=2), "hours remaining", sep=" "))
      
    } # end of if converged statement
    
    df       <- base::data.frame(
      assess    = sao.name,
      iter       = i,
      starttime = start_time,
      endtime   = end_time,
      duration  = round(end_time - start_time, digits=0),
      aic       = as.numeric(AIC(sim_fit)),
      convergence = sim_fit$opt$convergence,
      allSDnotNA  = all(!is.na(unlist(sim_fit$plsd[which(names(sim_fit$plsd)%in%names(sim_fit$sdrep$par.fixed))]))),
      maxgradient = max(sim_fit$sdrep$gradient.fixed),
      inFLStock   = ifelse(sim_fit$opt$convergence==0,TRUE,FALSE),
      stringsAsFactors = FALSE)
    simruns <- bind_rows(simruns, df)
    # save(simruns, file="run/simruns.RData")
    save(simruns, file=file.path(tempdir, "run", paste0(sao.name,"_simruns.RData")))
    # save(simruns, file=paste0("run/",namestring,"_simruns.RData"))
    
    rm(sim_fit)
    invisible(gc())
    
  } # end of if exists statement
  
  # increase i trials in all cases
  it = it + 1
  
} # end of while statement

# spdir <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/wkwhmrp/2021 Meeting Docs/06. Data"

# df2019a <- as.data.frame(loadRData(file=file.path(spdir, "WHOM_SAM19/run/WHOM_SAM19_FLS_all.RData")))  %>%  
#   mutate(assesstype="SAM", assessyear="2019", runs="all")
# df2019c <- as.data.frame(loadRData(file=file.path(spdir, "WHOM_SAM19/run/WHOM_SAM19_FLS_converged.RData")))  %>%  
#   mutate(assesstype="SAM", assessyear="2019", runs="converged")
# df2020a <- as.data.frame(loadRData(file=file.path(spdir, "WHOM_SAM20/run/WHOM_SAM20_FLS_all.RData")))  %>%  
#   mutate(assesstype="SAM", assessyear="2020", runs="all")
# df2020c <- as.data.frame(loadRData(file=file.path(spdir, "WHOM_SAM20/run/WHOM_SAM20_FLS_converged.RData")))  %>%  
#   mutate(assesstype="SAM", assessyear="2020", runs="converged")

# df <-
#   bind_rows(df2019a, df2019c, df2020a, df2020c) %>%
#   filter(slot=="harvest", age >= minage, age <= maxage) %>%
#   group_by(assesstype, assessyear, runs, year, unit, season, area, iter) %>%
#   summarise(data = mean(data, na.rm=TRUE)) %>%
#   mutate(
#     slot="meanf",
#     age ="1-10"
#   ) %>%
#   bind_rows(df2019a, df2019c, df2020a, df2020c) 

# a <-
#   df %>% 
#   filter(iter == "1", runs == "all") %>% 
#   mutate(year=factor(year)) %>% 
#   filter(slot %in% c("stock", "meanf")) 

# df %>%
#   mutate(year=factor(year)) %>% 
#   filter(slot %in% c("stock", "meanf")) %>%
#   filter(runs=="converged") %>% 
#   
#   ggplot(aes(year, data, fill=runs, colour=runs)) +
#   theme_publication() +
#   theme(axis.text.x=element_text(angle =90, vjust = 0.5)) +
#   geom_boxplot(position=position_dodge()) +
#   geom_line(data=a, aes(year, data), group=1, inherit.aes = FALSE, colour="black") +
#   labs(x="", y="") +
#   facet_grid(slot~assessyear, scales="free_y")

# a %>%
#   ungroup() %>% 
#   ggplot(aes(year, data)) +
#   theme_publication() +
#   theme(axis.text.x=element_text(angle =90, vjust = 0.5)) +
#   # geom_boxplot(position=position_dodge()) +
#   # geom_point(colour="black") +
#   geom_line(aes(colour=assessyear, group=assessyear)) +
#   labs(x="", y="") +
#   facet_wrap(~slot, scales="free_y")

# df %>%
#   mutate(year=factor(year)) %>% 
#   filter(slot=="stock.n") %>%
#   filter(age==5) %>% 
#   ggplot(aes(year, data, fill=assess, colour=assess)) +
#   theme(axis.text.x=element_text(angle =90, vjust = 0.5)) +
#   geom_boxplot(position=position_dodge()) +
#   scale_y_continuous(trans = 'log10')

# df %>%
#   mutate(year=factor(year)) %>% 
#   filter(slot=="meanf") %>%
#   ggplot(aes(year, data, fill=assess, colour=assess)) +
#   theme(axis.text.x=element_text(angle =90, vjust = 0.5)) +
#   geom_boxplot(position=position_dodge())

# df2020c %>%
#   mutate(year=factor(year)) %>% 
#   filter(slot %in% c("stock.wt", "catch.wt")) %>%
#   ggplot(aes(year, data, colour=slot)) +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle =90, vjust = 0.5)) +
#   geom_boxplot(position=position_dodge()) +
#   facet_wrap(~age)

# df %>% 
#   filter(run<=2) %>% 
#   ggplot(aes(x=year,y=data, group=run)) +
#   theme_bw() +
#   geom_line(colour="gray")

# plot(FLSs)

t <- 
  as.data.frame(FLSs@stock[,,,,,1:1000]) %>% 
  mutate(variable = "ssb") 

ggplot() +
  theme_publication() +
  geom_line(data=t %>% filter(iter>1), aes(x=year, y=data, group=iter), colour="gray" ) +
  geom_line(data=t %>% filter(iter==1), aes(x=year, y=data, group=iter), colour="black", size=1.5 ) 

  
