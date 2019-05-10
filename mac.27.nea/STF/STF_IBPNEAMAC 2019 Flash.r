#R version 3.4.2 (2017-09-28)
#Platform: i386-w64-mingw32/i386 (32-bit)
#Running under: Windows 7 x64 (build 7601) Service Pack 1
#
#Matrix products: default
#
#locale:
#[1] LC_COLLATE=Dutch_Netherlands.1252  LC_CTYPE=Dutch_Netherlands.1252   
#[3] LC_MONETARY=Dutch_Netherlands.1252 LC_NUMERIC=C                      
#[5] LC_TIME=Dutch_Netherlands.1252    
#
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#[1] FLSAM_2.1.0           stockassessment_0.0.5 FLAssess_2.6.1        FLash_2.5.8          
#[5] FLCore_2.6.6          lattice_0.20-35       MASS_7.3-47          
#
#loaded via a namespace (and not attached):
#[1] compiler_3.4.2 Matrix_1.2-11  parallel_3.4.2 TMB_1.7.14     ellipse_0.4.1  grid_3.4.2    
#[7] stats4_3.4.2   FLFleet_2.6.0 
#> 

rm(list=ls())

library(FLCore)
# library(FLAssess)
library(stockassessment) 
library(FLasher)

# setwd("M:/WGWIDE/WGWIDE2018/R assessment/")
# setwd("D:/WGWIDE/2018/07. Software/Mackerel/STF/")
setwd("D:/GIT/wgwide/mac.27.nea/STF")

# assess.name <- "MAC FLStock"

# load(paste(assess1,"/fitAR-2.RData",sep=""))
# load("fitAR-2.RData")

# source('SAM2FLRtmb.r')

# work.dir    <-  "M:/WGWIDE/WGWIDE2018/STF/"
# output.dir  <-  "M:/WGWIDE/WGWIDE2018/STF/STFresults" 

# work.dir    <-  "D:/WGWIDE/2018/07. Software/Mackerel/STF/"
# output.dir  <-  "D:/WGWIDE/2018/07. Software/Mackerel/STFresults" 

work.dir    <-  getwd()
output.dir  <-  paste0(work.dir, "/ibp_STFresults")

# save(Mac,file=paste(assess.name,"_stock.RData",sep=""))
# load(file=paste(assess.name,"_stock.RData",sep="")) 

load(file="MAC FLStock.RData") 
# load(file="WIDE2018.2 noRFID.RData") 

TaY <- 2017
ImY <- TaY + 1
AdY <- TaY + 2
CtY <- TaY + 3
tbl.yrs <- ImY:CtY

# prepare init file for RCT3
R             <- round(stock.n(Mac)[ac(0),ac(1990:2017)],0)
R             <- c(R@.Data)
R[length(R)]  <- -11

t <- c((Mac.tun[names(Mac.tun) == "R-idx(sqrt transf)"]@.Data)[[1]]@index)

IBTS.index<-c(rep(-11,8), t(t))

# IBTS.index<-c(rep(-11,8),c(
# 0.015720899	,	
# 0.017996206,		
# 0.012743674,		
# 0.022164525,		
# 0.023618634,		
# 0.013230785,	
# 0.024607411,		
# 0.038156211,		
# 0.037598707,		
# 0.020352249,		
# 0.018292615,		
# 0.015170405,		
# 0.027764032,		
# 0.036979005,		
# 0.02420564,		
# 0.023257095,		
# 0.025778066,
# 0.023169671,
# 0.023169671,
# 0.023169671))     # some fake value for 2016-2017 just to get it running and get the weighted tapper mean                   

years<-1990:2017

# remove files in the RCT3 folder !!!!
file.name<-"ibp_RCT3/RCT3init.txt"
file.remove(file=file.name)

# Generate new RCTinit file
write.table(data.frame("RCT3 for NEA Mackerel"),
            file="ibp_RCT3/RCT3init.txt",
            quote=F,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t")
write.table(data.frame(1,length(R),2,"SAM","IBTS.index"),
            file="ibp_RCT3/RCT3init.txt",
            quote=F,col.names=FALSE,row.names=FALSE,append=TRUE,sep="\t")
write.table(data.frame(years,R,IBTS.index),
            file="ibp_RCT3/RCT3init.txt",
            col.names=FALSE,quote=F,row.names=FALSE,append=TRUE,sep="\t")
write.table(data.frame(c("SAM","IBTS.index")),
            file="ibp_RCT3/RCT3init.txt",
            col.names=FALSE,quote=F,row.names=FALSE,append=TRUE,sep="\t")

# Source the RCT3 code
# source("RCT3/RCT3v4a.r")
  source("RCT3/RCT3v4a.modifAndy.r")

# run RCT3
Rct3<-RCT3("ibp_RCT3/RCT3init.txt",logged=T)
Rct3$plot()
Rct3$input()
RCT3res<-Rct3$output()
Rct3$summary()
Rct3$printout("ibp_RCT3/RCT3output.txt")

# ==================================================================================
# Deal with recruitment :
# ==================================================================================

# terminal year recruitment to be replaced by output of RCT3
stock.n(Mac)[1,ac(TaY)]     <-      RCT3res$Years$WAPred  # RCT3 output using a predictor the IBTS index for the terminal assessmnet year

cat("use the tapered.time.weighted.mean due to the absence of 2016-2017 index")
stock.n(Mac)[1,ac(TaY)]     <-      exp(RCT3res$Intercept[2])

# recompute the survivors (year ImY) at age 1
stock.n(Mac)[2,ac(ImY)]    <- stock.n(Mac)[1,ac(TaY)] * exp (-(harvest(Mac)[1,ac(TaY)]+m(Mac)[1,ac(TaY)]))

# define the recruitment for the short term forecast (starting at ImY )
rec.years <- (1990:(TaY-1))

Mac.srr <- FLSR(model = geomean, ssb = ssb(trim(Mac,year=rec.years)), rec = rec(trim(Mac,year=rec.years)) )
Mac.srr <- fmle(Mac.srr)
 
# if desired, replace the geomean by the tapered time weighted mean from RCT 3
replace.by.tapered.time.weighted.mean <- F
tapered.time.weighted.mean       <- exp(RCT3res$Intercept[2])
if(replace.by.tapered.time.weighted.mean) params(Mac.srr)[1]  <- tapered.time.weighted.mean


# ==================================================================================
# Prepare forecast
# ==================================================================================

#Expand stock object

#NEA.Mac.proj <- stf(NEA.Mac,nyears=4,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE)
Mac.proj <- FLasher::stf(trim(Mac, year=(TaY-5):TaY),nyears=3,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE,fbar.nyears=3) # to use object saved before rounding

# this fills in N until TaY
# this fills in the F until the end projection years where Fs until TaY-1 are the same as assessment and F later years is the mean of the last 3 years. 
# let's change it to the definition given in the stock annex :
#  - compute the selection pattern (F divided by Fbar)
#  - average of the selection pattern of the last 3 years
#  - scaled to the Fbar average over the last 3 years

# selection pattern
sel     <-sweep(harvest(Mac.proj),c(2:6),fbar(Mac.proj),"/")    

# mean selection pattern of the last 3 years
selmean <-yearMeans(sel[,ac((TaY-2):TaY)])                  

# mean of  Fbars of the last 3 years
fb      <-yearMeans(fbar(Mac.proj)[,ac((TaY-2):TaY)])            

# update fishing mortality at age for 
harvest(Mac.proj)[,ac(ImY:(AdY+1))]   <-sweep(selmean,c(2:6),fb,"*") 

# ok!

# update the survivors in the stf object
Mac.proj@stock.n[,ac(ImY)]  <- stock.n(Mac)[,ac(ImY)]

# use the geometric mean for recruitment in the fwdion period
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# MP: This is not used???
Rgm <- gm_mean(stock.n(Mac)[1,ac(rec.years)]@.Data)

# Sewt recruitment in forecast years
Mac.proj@stock.n[1,ac(c(ImY,AdY,CtY))] <-  params(Mac.srr)[1]

# keep a copy of the original stf object
Mac.proj.org  <- Mac.proj         

#Setup options to suite the advice sheet  the choice here is based on the following:
#0 catch,  role over + and - 20% on TAC, F=0.20,0.21,0.22 from management plan  
ImY.catch <- 1000559
 
# estimated catches expected due to TAC, discards, payback, overfishing, unilateral quotas etc.
ImY.TAC   <- 1000559
 
# Function return Catch
returnCatch<-function(Fmult,stff,yr,Cobs){
  ns <- stff@stock.n[,ac(yr)]
  F. <- stff@harvest[,ac(yr)]*Fmult
  M. <- m(stff)[,ac(yr)]
  Y  <- (F./(F.+M.)) * ns *(1 - exp (-F.-M.))
  C  <- Y*catch.wt(stff)[,ac(yr)]
  C  <- quantSums(C)
  return((C-Cobs)^2)
}

# Function Project the first year
proj1stY<-function(stf,y,Catch) {
  Fm                   <- optimise(returnCatch,c(0.5,1.5),stff=stf,yr=y,Cobs=Catch,tol=0.0000000001)$minimum
  F.                   <- harvest(stf)[,ac(y)]*Fm
  M.                   <- m(stf)[,ac(y)]
  Y                    <- (F./(F.+M.)) * stock.n(stf)[,ac(y)] *(1 - exp (-F.-M.))
  harvest(stf)[,ac(y)] <- F.
  catch.n(stf)[,ac(y)] <- Y
  return(stf)
}

# do the forecast over the intermediate year first
Mac.proj<-proj1stY(Mac.proj,ImY,ImY.catch)

# Control objects
ctrl_target <- data.frame(year = c(AdY,CtY), 
                          quant = "f", 
                          value = c(0.24,0.24))
ctrl_f      <- FLasher::fwdControl(ctrl_target)

cc          <- FLasher::fwd(Mac.proj, 
                            control = ctrl_f, 
                            sr      = Mac.srr)

# define the option for the advice sheet
options.l <- list( 
                  #Zero catch
                  "Catch(2019) = Zero"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant="catch",
                                          value=c(0,0))),
                  #2010 Catch is XXX, followed by -20% Catch reduction => XXX
                  "Catch(2019) = 2018 catch  -20%"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("catch","f"),
                                          relYear=c(NA,NA),
                                          value=c(ImY.TAC*0.80,0.498062592))),

                     #2009 and 2010 Catch is XXX
                  "Catch(2019) = 2018 catch"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("catch","f"),
                                          relYear=c(NA,NA),
                                          value=c(ImY.TAC,0.664823432))),
                  #2010 Catch is XXX, followed by +20% Catch increase => 2011 Catch XXX
                  "Catch(2019) = 2018  +25%"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("catch","f"),
                                          relYear=c(NA,NA),
                                          value=c(ImY.TAC*1.25,0.911582621))),
                  #2010 Catch is XXX, followed Fbar= 0.20
                  "Fbar(2019) = 0.24 (Fmp)"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.24,0.24))),
                  #2010 Catch is XXX, followed Fbar= 0.21
                  "Fbar(2019) = 0.23 "=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.23,0.23))),
                  #2010 Catch is XXX, followed Fbar= 0.21
                  "Fbar(2019) = Fbar(2018) "=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f"),
                                          value=c(fbar(Mac)[,ac(TaY)]))),                                          
                  #2010 Catch is XXX, followed Fbar= 0.21,
                  "Fbar(2019) = 0.31 (Fpa)"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.35,0.35))),                                          
                  #2010 Catch is XXX, followed Fbar= 0.21,
                  "Fbar(2019) = F2018"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.455264667,0.455264667)))  ,  
                  #2010 Catch is XXX, followed Fbar= 0.21,
                  "Fbar(2019) = 0.43 (Flim)"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.48,0.48))),  
                  #2010 Catch is XXX, followed Fbar= 0.21
                  "Fbar(2019) = 0.21 (Fmsy)"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.21,0.21))),
                  #2010 Catch is XXX, followed Fbar= 0.22
                  "Fbar(2019) = 0.26"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.26,0.26)))   ,#,
                                          
                 #2010 Catch is XXX, followed Fbar= 0.22
                  "Fbar(2019) = 0.27"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.27,0.27)))   ,
                                          
                 #2010 Catch is XXX, followed Fbar= 0.22
                  "Fbar(2019) = 0.28"=
                    fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(0.28,0.28)))                                                #, ,
                                         
) #End options list

# Calculate forecasts for options list
Mac.options   <- lapply(options.l,function(ctr_op) {fwd(Mac.proj,control=ctr_op,sr = Mac.srr)})
names(Mac.options)<-names(options.l)


#Multi-options table - standard one to show wider range of options for the report; F multipliers from 0 to 2 *roll over F
fmult.targs  <- seq(0,2,by=0.01)
mult.opts.l  <- lapply(as.list(fmult.targs),function(fmult) {
                          fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          relYear=c(ImY,AdY),
                                          value=c(fmult,fmult)))
                  })
names(mult.opts.l) <- sprintf("Fmult(2017) = %4.3f",fmult.targs)

#Calculate forecasts for multi-option table
Mac.mult.opts <- lapply(mult.opts.l,function(ctr_op) {fwd(Mac.proj,control=ctr_op,sr = Mac.srr)})
names(Mac.mult.opts)<-names(mult.opts.l)


### ======================================================================================================
### Write Options Tables
### ======================================================================================================

output.base <- output.dir

#Document input settings
input.tbl.file <-paste(output.base,"/options - input.csv",sep="")
write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY)){
    col.dat <- sapply(input.tbl.list,function(slt) slot(Mac.proj.org,slt)[,as.character(yr),drop=TRUE])
    write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
    write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
    write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}

#Detailed (multi-) options table
options.file <-paste(output.base,"/options - details.csv",sep=".")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(Mac.options)) {
    opt <- names(Mac.options)[i]
    stk <- Mac.options[[opt]]
    #Now the F and N by age
    nums.by.age           <- stk@stock.n[,ac(tbl.yrs),drop=TRUE]
    colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
    f.by.age              <- stk@harvest[,ac(tbl.yrs),drop=TRUE]
    colnames(f.by.age)    <- sprintf("F(%s)",tbl.yrs)
    age.tbl               <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
    #And now the summary tbl
    sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,ac(tbl.yrs)],
                        F.bar=fbar(stk)[,ac(tbl.yrs)],Yield=computeCatch(stk)[,ac(tbl.yrs)])
    #Now, bind it all together
    sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
    comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
    #And write it - hdr first, then the rest
    write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
    write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

#Options summary table
opt.sum.tbl <- function(stcks,fname) {
  options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
    opt <- names(stcks)[i]
    stk <- stcks[[opt]]
    #Build up the summary
    sum.tbl <- data.frame(Rationale=ac(names(stcks)[i]),
    F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
    Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
    SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
    
    TSB.ImY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],
    
    F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
    Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
    SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
    
    TSB.AdY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,
    
    SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
    TSB.CtY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )
  }) # end of sapply on function i
  
  options.sum.tbl <- t(options.sum.tbl)
  options.sum.tbl[,1]<-names(stcks)
  colnames(options.sum.tbl) <- c("Rationale",
  sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),sprintf("TSB (%i)",ImY),
  sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),sprintf("TSB (%i)",AdY),
  sprintf("SSB (%i)",CtY),sprintf("TSB (%i)",CtY) )
  write.csv(options.sum.tbl,file=fname,row.names=FALSE)
} # end of function opt.sum.tbl

opt.sum.tbl(stcks=Mac.options,fname=paste(output.base,"/options - summary.csv",sep="."))
opt.sum.tbl(stcks=Mac.mult.opts,fname=paste(output.base,"/multi-options - summary.csv",sep="."))





### ======================================================================================================
### finally, apply the ICES MSY advice rule
# ============================================================================


 #  if projections indicate that SSB advice is below MSY Btrigger, then find the F that would lead an SSB on the MSY HCR
 

# load function that measures the difference between the Ftarget you put in and the F msy hcr that corresponds to the resulting SSB
# this function needs to be minimised 
source('FindFtarget.r')

# reference point for the MSY HCCR
msyref <- c(Ftarg=0.21,Btrig=2.57e6)

#Find the Fmultiplier to apply relative to ImY
Ft<-optimise(FindFtarget,
             c(0.1,1.5),
             stk=Mac.proj,
             srr=Mac.srr,
             ady=AdY,
             cty=CtY,
             op=msyref,
             tol=0.0000000001)$minimum  

# uupdate F advice
FAdY  <- c(Ft*fbar((Mac.proj)[,ac(ImY)]))


# do the forecast for this option
ctrl_MSY<-fwdControl(data.frame(year=c(AdY,CtY),
                                          quant=c("f","f"),
                                          value=c(FAdY,FAdY)))

Mac.options<-(fwd(Mac.proj,control = ctrl_MSY,sr =Mac.srr))
names(Mac.options)<-names("MSY AR")



### Write Option Table


output.base <- output.dir
opt <- names(Mac.options)
stk <- Mac.options
#Build up the summary
options.sum.tbl <- data.frame(Rationale="MSY AR",
F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],

TSB.ImY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],

F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],

TSB.AdY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,

SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
TSB.CtY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )


#options.sum.tbl <- t(options.sum.tbl)
options.sum.tbl[,1]<-"MSY AR"
colnames(options.sum.tbl) <- c("Rationale",
sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),sprintf("TSB (%i)",ImY),
sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),sprintf("TSB (%i)",AdY),
sprintf("SSB (%i)",CtY),sprintf("TSB (%i)",CtY) )
write.csv(options.sum.tbl,file=paste(output.base,"/options - summary applying Btrigger.csv",sep="."),row.names=FALSE)


# Now for the new reference points

# reference point for the MSY HCCR
msyref <- c(Ftarg=0.23,Btrig=2.50e6)

#Find the Fmultiplier to apply relative to ImY
Ft<-optimise(FindFtarget,
             c(0.1,1.5),
             stk=Mac.proj,
             srr=Mac.srr,
             ady=AdY,
             cty=CtY,
             op=msyref,
             tol=0.0000000001)$minimum  

# uupdate F advice
FAdY  <- c(Ft*fbar((Mac.proj)[,ac(ImY)]))


# do the forecast for this option
ctrl_MSY<-fwdControl(data.frame(year=c(AdY,CtY),
                                quant=c("f","f"),
                                value=c(FAdY,FAdY)))

Mac.options<-(fwd(Mac.proj,control = ctrl_MSY,sr =Mac.srr))
names(Mac.options)<-names("MSY AR")

### Write Option Table

output.base <- output.dir
opt <- names(Mac.options)
stk <- Mac.options
#Build up the summary
options.sum.tbl <- data.frame(Rationale="MSY AR",
                              F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                              Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                              SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                              
                              TSB.ImY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(ImY),drop=TRUE],
                              
                              F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                              Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                              SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                              
                              TSB.AdY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(AdY),drop=TRUE] ,
                              
                              SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
                              TSB.CtY=quantSums(stock.wt(stk)*stock.n(stk))[,as.character(CtY),drop=TRUE] )


#options.sum.tbl <- t(options.sum.tbl)
options.sum.tbl[,1]<-"MSY AR"
colnames(options.sum.tbl) <- c("Rationale",
                               sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),sprintf("TSB (%i)",ImY),
                               sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),sprintf("TSB (%i)",AdY),
                               sprintf("SSB (%i)",CtY),sprintf("TSB (%i)",CtY) )
write.csv(options.sum.tbl,file=paste(output.base,"/options - summary applying new Btrigger.csv",sep="."),row.names=FALSE)




#### make a plot to check if the MSY AR is computed fine

# opt <- read.csv("./STFresults/multi-options - summary.csv" )
# plot(0,0,xlim=c(0,4e6),ylim=c(0,0.4),cex=0,xlab = "SSB in 2019" , ylab = "Fbar in 2019",main="ICES MSY Advice Rule")
# points(opt$SSB..2019. , opt$Fbar..2019. , pch=19,cex=0.6)
# segments(0,0,2.57e6,0.21)
# segments(5e6,0.21,2.57e6,0.21)
# points(options.sum.tbl[8],options.sum.tbl[6],pch=20,col="red",cex=2)