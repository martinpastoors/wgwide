# IGFS analysis

install.packages('DATRAS',repos='http://www.rforge.net/',type='source')
library(devtools)
install_github("casperwberg/surveyIndex/surveyIndex")

library(DATRAS)
library(surveyIndex)
library(tidyverse)

dAll<-readExchange("D:/XXX/DATRAS/IGFS.zip",strict=FALSE)
mc.cores<-1; library(parallel)
d<-subset(dAll, Species=="Clupea harengus",Quarter==4,HaulVal=="V",StdSpecRecCode==1, Gear=="GOV",
          ShootLat <= 52)
dAll<-NULL; gc(); ## lose dAll because it takes up a lot of memory
d<-addSpectrum(d,by=1)


## get idea about number of age groups to include
agetab<-xtabs(NoAtALK~Year+Age,data=d[[1]])
agetab.df<-as.data.frame(agetab)

ages <- c(0:4)

## require at least 1 aged individual in each year
for(a in ages){
  if(any(agetab.df$Freq[agetab.df$Age==a]<1))
    d<-fixAgeGroup(d,age=a,fun=ifelse(a==min(ages),"min","mean"))
}

d<-subset(d,Age>=min(ages))

###############################
## Convert to numbers-at-age
###############################

d.ysplit <- split(d, d$Year)
ALK<-mclapply(d.ysplit,fitALK,minAge=min(ages),maxAge=max(ages),autoChooseK=TRUE,useBIC=TRUE,varCof=FALSE,maxK=50,mc.cores=mc.cores)
Nage<-mclapply(ALK,predict,mc.cores=mc.cores)
for(i in 1:length(ALK)) d.ysplit[[i]]$Nage=Nage[[i]];
dd <- do.call("c",d.ysplit)

##############
## Fit model
##############

grid <- getGrid(dd, nLon=40)
## set max basis dim for spatial smooths by age, P=positive and Z=zero/absence.
## These are set relatively low here to speed up the example
kvP <- c(50,50,50,40,30,rep(10,length(ages)-5))
kvZ <- kvP / 2;
mP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts')+s(Depth,bs='ts',k=6)+offset(log(HaulDur))",length(ages)  );
mZ <- rep("Year+s(lon,lat,k=kvecZ[a],bs='ts')+s(Depth,bs='ts',k=6)+offset(log(HaulDur))",length(ages)  );

SIQ1 <- getSurveyIdx(dd,ages=ages,myids=grid[[3]],cutOff=0.1,kvecP=kvP,kvecZ=kvZ,modelZ=mZ,modelP=mP,mc.cores=mc.cores) ## if errors are encountered, debug with mc.cores=1

strat.mean<-NULL

## plot indices, distribution map, and estimated depth effects
surveyIdxPlots(SIQ1,dd,cols=ages+1,alt.idx=strat.mean,grid[[3]],par=list(mfrow=c(3,3)),legend=FALSE,select="index",plotByAge=FALSE)

surveyIdxPlots(SIQ1,dd,cols=ages+1,alt.idx=NULL,grid[[3]],par=list(mfrow=c(3,3)),legend=FALSE,colors=rev(heat.colors(8)),select="map",plotByAge=FALSE)

surveyIdxPlots(SIQ1,dd,cols=ages+1,alt.idx=NULL,grid[[3]],par=list(mfrow=c(3,3)),legend=FALSE,select="2",plotByAge=FALSE)

## Calculate internal concistency and export to file
internalCons(SIQ1$idx)
exportSI(SIQ1$idx,ages=ages,years=levels(dd$Year),toy=mean(dd$timeOfYear),file="out.dat",nam="Survey index demo example")