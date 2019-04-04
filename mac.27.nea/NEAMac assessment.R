# --------------------------------------------------------------------------------
# NEA Mackerel assessment
#
# Thomas Brunel
# 
# 04/04/2019 Commenting the code (Martin Pastoors)
#
# --------------------------------------------------------------------------------

library(tidyverse)
library(stockassessment)
library(TMB)

# set working directory to mackerel folder
setwd("./mac.27.nea")

# source the datascript
source("NEAMac datascript.R")

recap$splitPhi=rep(1,nrow(recap))

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf,
                    recapture=recap)

# load model configuration
conf <- stockassessment::loadConf(dat,file="modelnew.cfg", patch=TRUE)

# create parameter object
par  <-stockassessment::defpar(dat,conf)

# fit sam model
fit.new <-sam.fit(dat,conf,par, newtonsteps=0)

# save fit object
save(fit.new, file="VarObsCatches2FvarNEW.RData")

# where is the simstudy method coming from? method not working for me. 
set.seed(12345)
sim <- simstudy(fit.new, nsim=10)
ji  <- jit(fit.new, nojit=10)

# do retrospective analysis (method not working for me)
ret <- retro(fit.new, year=5)

# leave one out analysis (method not working for me)
fit.new.wo <- runwithout(fit.new, year=2013:2016, fleet=c(5,5,5,5))
lo<-leaveout(fit.new)
lo[4]<-NULL
lo$"w.o. RFID"<-fit.new.wo

# set number of cores for calculations
options(mc.cores=2)
# options(mc.cores=4)


dat<-fit.new$data
dis<-dat$fleetTypes[dat$aux[,"fleet"]]==5
res<-TMB::oneStepPredict(fit.new$obj, observation.name="logobs", data.term.indicator="keep", discrete=FALSE, subset=which(!dis), parallel=TRUE)
res2<-TMB::oneStepPredict(fit.new$obj, observation.name="logobs", data.term.indicator="keep", discrete=TRUE, conditional=which(!dis),
                          subset=which(dis), method ="oneStepGeneric", range=c(0,Inf), parallel=TRUE)

totalres<-rep(NA,nrow(dat$aux))
totalres[!dis]<-res$residual
totalres[dis]<-res2$residual
myres<-data.frame(dat$aux, residual=totalres)

TAGRES<-myres[myres$fleet==5,]
myres$residual[myres$fleet==5]<-NA
class(myres)<-"samres"
attr(myres, "fleetNames")<-attr(fit.new$data, "fleetNames")
RES<-myres
RESP<-procres(fit.new)

pdf("newdiag.pdf", 12, 10)
ssbplot(fit.new)
fbarplot(fit.new)
recplot(fit.new)
catchplot(fit.new)
fitplot(fit.new, fleet=1)
fitplot(fit.new, fleet=2, ylim=c(14.5, 16))
fitplot(fit.new, fleet=3)
fitplot(fit.new, fleet=4)
fitplot(fit.new, fleet=5)
plot(RESP)
plotby(TAGRES$year, TAGRES$RecaptureY-TAGRES$year, TAGRES$residual, by=TAGRES$age, xlab="Year", ylab="No years out"); title(paste(round(range(TAGRES$residual),1), collapse=" - "))
plot(RES)
plot(ret)
plot(lo)
plot(sim); title("10 sim and re-estimates")
plot(ji); title("10 jitters")
dev.off()
