# =======================================================================================
# Convert SAM object to FLStock object
#
# Thomas Brunel, sept 2018
# 
# 20190310 adapted for IBPNEAMAC 2019
# =======================================================================================

library(FLCore)
library(FLSAM)
library(stockassessment)   # devtools::install_github("fishfollower/SAM/stockassessment")
library(TMB)

rm(list=ls())

# load SAM object
load("//community.ices.dk/DavWWWRoot/ExpertGroups/benchmarks/2019/IBPNEAMac/2019 Meeting docs/07. Software/final run/VarObsCatches2FvarNEW.RData")

#Create FLSAM object
res       <- new("FLSAM")

# admb.stem="sam"
# #Read report file
# rept.fname    <-  file.path(run.dir,sprintf("%s.rep",admb.stem))
# rept          <-  scan(rept.fname,what=integer(), quiet=TRUE)

assess.name   <- "//community.ices.dk/DavWWWRoot/ExpertGroups/benchmarks/2019/IBPNEAMac/2019 Meeting docs/07. Software/final run/"

yrs           <-  (fit$data$years)
res@n.states  <-  length(yrs)
 
res@name      <- assess.name
res@desc      <- date()

#Figure out whether the hessian has been estimated
# res@nohess <- F   # !file.exists(sprintf("%s/%s.cor",run.dir,admb.stem))

#Work out the contents of the range object from the contents of the directory. First the ages
#mdl.cfg <- readLines(sprintf("%s/model.cfg",run.dir))
#    res@range["min"] <- scan(text=mdl.cfg[grep("Min Age",mdl.cfg, ignore.case=TRUE)+1],quiet=TRUE)
    res@range["min"]<-0
#    max.ages <- scan(text=mdl.cfg[grep("Max Age",mdl.cfg,
#                                       ignore.case=TRUE)+1],quiet=TRUE)
#    res@range["max"] <- max.ages[1]
    res@range["max"] <-12
#    res@range["plusgroup"] <- ifelse(max.ages[2]==1,max.ages[1],NA)
    res@range["plusgroup"] <- 12
    #Extract the bindings
    #comment.line <- grepl("^ *#",mdl.cfg)
    #num.block <- ifelse(!comment.line,cumsum(comment.line),NA)
    #state.line    <- grep("STATES",mdl.cfg,ignore.case=FALSE)
    #state.block <- min(num.block[-(1:state.line)],na.rm=TRUE)
#    res@states <- as.matrix(read.table(text=mdl.cfg[which(num.block==state.block)]))
    res@states <- fit$conf$keyLogFsta   
    
    colnames(res@states) <- res@range["min"]:res@range["max"]
    rownames(res@states) <- c("catch",seq(nrow(res@states))[-1])
    
    #And now the years (already read in from sam.rep)
    res@range["minyear"] <- min(yrs)
    res@range["maxyear"] <- max(yrs)
    
    #And now the fbar
#    fbar.vals <- scan(text=mdl.cfg[grep("Fbar",mdl.cfg)+1],quiet=TRUE)
    res@range["minfbar"] <- 4
    res@range["maxfbar"] <- 8
    #Work out the fishing mortality couplings
    
    
    
    
    
#---------------------------------------------------
# Read the results from the assessment
#---------------------------------------------------
  #Read parameter file
 # par.fname    <- file.path(run.dir,sprintf("%s.par",admb.stem))
#  par.hdr     <-  as.numeric(scan(par.fname,what="", n=16, quiet=TRUE)[c(6,11,16)])
#  #  res@nopar   <-  as.integer(par.hdr[1])
  res@nopar   <-  length(names(fit$sdrep$par.fixed))
  res@nlogl   <-  fit$opt$objective
  #  res@maxgrad <-  par.hdr[3]

  #Read residual files
#  res.fname    <- file.path(run.dir,"sam.res") #Not ideal, but easier than changing it in tpl
#  res@residuals  <-  read.table(res.fname,header=FALSE,
#                       col.names=c("year","fleet","age","log.obs","log.mdl","std.res"))
#  res@residuals$fleet <- factor(sprintf("Fleet %s",res@residuals$fleet))
#

#---------------------------
#The model can occasionally be run in "nohess" mode, where 
#the hessian is not estimated. This can create problems as 
#neither the vcov matrix nor the std.errs are estimated
#---------------------------

  # if(res@nohess) {
#     #Load and parse par file
#     par.txt     <- readLines(par.fname)[-1]
#     n.lines     <- length(par.txt)
#     par.chunks  <- paste(par.txt[seq(1,n.lines,by=2)],par.txt[seq(2,n.lines,by=2)])
#     par.lst     <- lapply(par.chunks,function(x) {
#                       var.name <- gsub("# (.*?):.*","\\1",x) 
#                       var.vals <- gsub("# .*?:(.*)","\\1",x) 
#                       var.dat  <- scan(con <- textConnection(var.vals),quiet=TRUE) 
#                       close(con)
#                       data.frame(name=var.name,value=var.dat,std.dev=NA)
#                    })
#     par.df <- do.call(rbind,par.lst)
#     res@params <- par.df
#  
#     #Create synthetic (empty) vcov matrix
#     res@vcov <- matrix(NA)
#  
#  } else { #Else read the results
#  
   







     #Read standard deviation report (if it exists)
      p <- summary(fit$sdrep)
      p<-data.frame(1:dim(p)[1],rownames(p),p)
      names(p) <-   c("index","name","value","std.dev")
     res@params <-p
     res@params$index <- NULL
   
     #Now read the variance-covariance matrix. However, the problem is
     #is that the result return by ADMB is not a square file, but only
     #the lower triangle
     
     dim(fit$sdrep)
     
     
#     cor.fname    <- file.path(run.dir,sprintf("%s.cor",admb.stem))
#     cor.lines.in <- readLines(cor.fname)[-c(1:2)] #First lines are headers
#     cor.lines    <- strsplit(cor.lines.in," +") #Split into elements 
#     vcov.dim     <- length(cor.lines) #Size of (squqare) vcov matrix 
#     #Write individual lines into a matrix for further processing. I can't
#     #see a cleaner way to do this apart from a for-loop :-(
#     cor.file.mat      <- matrix(as.character(NA), vcov.dim, vcov.dim+5)
#     for(i in 1:vcov.dim){ 
#       cor.file.mat[i,seq(cor.lines[[i]])]<- cor.lines[[i]]
#     }
#     #Extract correlation matrix
#     cor.mat.lt  <- apply(cor.file.mat[,-c(1:5)],2,as.numeric)
#     cor.mat.lt[upper.tri(cor.mat.lt,diag=FALSE)] <- 0
#     cor.mat <- cor.mat.lt + t(cor.mat.lt)
#     diag(cor.mat) <- diag(cor.mat)/2   #Diagonals get included twice, so halve them
#   
#     #Convert correlation matrix to vcov matrix
#     stds <- as.numeric(cor.file.mat[,5]) 
#     vcov.mat <- cor.mat*(stds%o%stds)
#     dimnames(vcov.mat) <- list(cor.file.mat[,3],cor.file.mat[,3])
#     
#     
# jointrep<-TMB::sdreport(fit$obj, getJointPrecision=TRUE)
#  COV<-solve(jointrep$jointPrecision)
#
#
#    res@vcov <- COV
#  
 
  
  
  
  #Extract the state variables
  u<-subset(res@params,name=="logN")  
  

  stateEst<-matrix(u$value,ncol=res@n.states,
                   dimnames=list(state=NULL,
                                 year=res@range["minyear"]:res@range["maxyear"]))
  stateSd <-matrix(u$std.dev,ncol=res@n.states,
                   dimnames=list(state=NULL,
                                 year=res@range["minyear"]:res@range["maxyear"]))

  #And copy into the appropriate slots as data.frames or FLQuants
  flq <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
                                   year=res@range["minyear"]:res@range["maxyear"]))
  n.ages <- dims(flq)$age
  res@stock.n <- flq
  res@stock.n[,] <- exp(stateEst[1:n.ages,])
  
 
 
  #Populate the harvest slot using the state bindings
 
u<-subset(res@params,name=="logF")  
n.yrs <- length(res@range["minyear"]:res@range["maxyear"])
n.ages<- dim(u)[1]/n.yrs

    stateEst<-matrix(u$value,ncol=res@n.states,
                   dimnames=list(state=NULL,
                                 year=res@range["minyear"]:res@range["maxyear"]))
  stateSd <-matrix(u$std.dev,ncol=res@n.states,
                   dimnames=list(state=NULL,
                                 year=res@range["minyear"]:res@range["maxyear"]))

  #And copy into the appropriate slots as data.frames or FLQuants
  flq <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
                                   year=res@range["minyear"]:res@range["maxyear"]))

  res@harvest <- flq
  res@harvest[1:n.ages,] <- exp(stateEst[1:n.ages,])
  res@harvest[(n.ages+1):dim(flq)[1],]   <- res@harvest[n.ages,]
  units(res@harvest) <-  "f"
 
 
 
 
  
  #Populate the info slot as last step
  #info <- data.frame(FLSAM.version=packageDescription("FLSAM")$Version,
#                     FLCore.version=packageDescription("FLCore")$Version,
#                     R.version=R.version$version.string,
#                     platform=R.version$platform,
#                     run.date=Sys.time())
#  res@info <- t(info)
#  colnames(res@info) <- "_"


# defines the time frame
stY <- dims(res)$minyear                    
TaY <- dims(res)$maxyear-1  #Terminal assessment year
ImY <- TaY+1                #Intermediate Year
AdY <- TaY+2                #Advice year
CtY <- TaY+3                #Continuation year - not of major concern but used in calculations in places
tbl.yrs     <- as.character(c(ImY,AdY,CtY))   #Years to report in the output table



# creates an FLStock based on assessment input and output data
Mac                                 <-FLStock(stock.n(res))
Mac                                 <- Mac+res
Mac@catch.n@.Data[,ac(stY:TaY),,,,] <- t(read.table(paste(assess.name,"/cn.dat",sep=""),skip=5))
Mac@catch.wt@.Data[,ac(stY:TaY),,,,]<- t(read.table(paste(assess.name,"/lw.dat",sep=""),skip=5))
Mac@catch                           <- computeCatch(Mac)
Mac@landings.n                      <- Mac@catch.n
Mac@landings.wt                     <- Mac@catch.wt
Mac@discards.wt                     <- Mac@catch.wt
Mac@discards.n@.Data[]              <- 0
Mac@landings                        <- Mac@catch
Mac@stock                           <- computeStock(Mac)
Mac@stock.wt@.Data[,ac(stY:TaY),,,,]<- t(read.table(paste(assess.name,"/sw.dat",sep=""),skip=5))[,-dim(Mac@stock.wt)[2]]
m(Mac)                              <- 0.15
Mac@mat@.Data[,ac(stY:TaY),,,,]     <- t(read.table(paste(assess.name,"/mo.dat",sep=""),skip=5))[,-dim(Mac@stock.wt)[2]]
Mac@harvest.spwn@.Data[,ac(stY:ImY),,,,] <- t(read.table(paste(assess.name,"/pf.dat",sep=""),skip=5))#[,-dim(Mac@stock.wt)[2]]
Mac@m.spwn@.Data[,ac(stY:ImY),,,,]  <- t(read.table(paste(assess.name,"/pm.dat",sep=""),skip=5))
Mac@range[6]                        <-4
Mac@range[7]                        <-8

Mac.tun   <- readFLIndices(file.path(assess.name,"survey.dat"))
Mac.tun   <- lapply(Mac.tun,function(x) {x@type <- "number"; return(x)})

# Save FLStock object
save(Mac, Mac.tun, file=paste(assess.name, "MAC FLStock.RData", sep=""))



stockassessment::fitplot(fit)
