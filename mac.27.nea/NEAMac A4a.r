# ==========================================================================
# A4A NEA Mackerel
#
# 01/04/2019 First coding, based on IPBNEAMAC 2019 data
# ==========================================================================

# install.packages(c("copula","triangle", "coda", "XML", "reshape2", "latticeExtra"))
# install.packages(c("copula","triangle", "XML", "latticeExtra"))

# install.packages(c("FLCore", "FLa4a"), repos="http://flr-project.org/R")
# install.packages(c("FLa4a"), repos="http://flr-project.org/R")

# To do: model selection
# To do: automatic running of assessments OK
# To do: error in summary plot
# To do: how to plot multiple assessments

library(FLCore)
library(FLa4a)
library(latticeExtra)
library(tidyverse)
library(directlabels)  # for printing labels at end of geom lines

# setwd("D:/GIT/wg_HAWG/VIa")


setwd("D:/GIT/wgwide/mac.27.nea")

# Load FLStock object
load("./stf/MAC FLStock.RData")
STK <- Mac
# STK <- trim(Mac, year=1990:2017)

rm("Mac")
units(catch(STK))    <- units(discards(STK))    <- units(landings(STK))    <- units(stock(STK)) <- 'tonnes'
units(catch.n(STK))  <- units(discards.n(STK))  <- units(landings.n(STK))  <- units(stock.n(STK)) <- '1000'
units(catch.wt(STK)) <- units(discards.wt(STK)) <- units(landings.wt(STK)) <- units(stock.wt(STK)) <- 'kg'
# Create the FLIndices object
STK.index <- readFLIndices("./data/survey.dat")
STK.index[[1]]@index[STK.index[[1]]@index == -1] <- NA
STK.index[[2]]@index[STK.index[[2]]@index == -1] <- NA
STK.index[[3]]@index[STK.index[[3]]@index == -1] <- NA

# Create FLIndexBiomass object
dnms           <- list(age="all", year=range(STK.index[[1]])["minyear"]:range(STK.index[[1]])["maxyear"])
bioidx         <- FLIndexBiomass(FLQuant(NA, dimnames=dnms))
index(bioidx)  <- as.numeric(STK.index[[1]]@index)
range(bioidx)[c("startf","endf")] <- c(0.1,0.5)
as.data.frame(idxs)


idxs <- FLIndices(c(Eggidx = bioidx, Ridx = STK.index[[2]], Sweptidx = STK.index[[3]]))
qmod <- list(~1, ~1, ~s(age, k=3))

# idxs <- FLIndices(c(Ridx = STK.index[[2]], Sweptidx = STK.index[[3]]))
# qmod <- list(~1, ~s(age, k=3))

# fmod <- ~ s(age, k=4) + s(year, k = 3)
fmod <- ~ factor(age)
srmod <- ~s(year, k=3)
# vmod <- list(~1, ~s(age, k=3))

fit1 <- FLa4a::sca(STK, idxs, fmodel=fmod, qmodel=qmod, srmodel=srmod) 
# fit1 <- FLa4a::sca(trim(STK,year=1998:2018 ), idxs, fmodel=fmod, qmodel=qmod, srmodel=srmod, vmodel=vmod) 


STK1 <- STK + fit1
res1 <- residuals(fit1, STK, idxs)

plot(STK1)
plot(res1)
bubbles(res1)
plot(STK1)
tst <- FLIndices(bioidx, STK.index[[3]])
class(tst[[2]])

dimnames(STK.index[[3]])
dimnames(bioidx)

bioidx + STK.index[[3]]

# units(harvest(STK)) <- 'f'
# plot(STK)

# Why is the readFLStock only giving NA  values?
# STK <- readFLStock(file.path("data/index.dat"))

# STK <- FLStock()
# STK@catch.n  <- readVPAFile("data/cn.dat")
# STK@catch.wt <- readVPAFile("data/cw.dat")
# STK@catch    <- readVPAFile("data/ct.dat")
# STK@stock.wt <- readVPAFile("data/sw.dat")
# STK@m        <- readVPAFile("data/nm.dat")
# STK@mat      <- readVPAFile("data/mo.dat")
# STK@harvest.spwn  <- readVPAFile("data/pf.dat")
# STK@m.spwn        <- readVPAFile("data/pm.dat")

# range(STK)[c('minfbar','maxfbar')] <- c(4,8)
# units(STK)[c('harvest')] <- 'f'

plot(STK.index[[3]]@index)
STK.index1 <- STK.index[[1]]
STK.index3 <- STK.index[[3]]
STK.index2 <- STK.index[[2]]
length(STK.index)

# simplistic approach
fit1 <- FLa4a::sca(STK, STK.index2)
STK1 <- STK + fit1
res1 <- residuals(fit1, ple4, ple4.indices)
plot(STK1)

plot(stk1)
wireframe(harvest(fit1), zlab="F")
plot(res1)
bubbles(res1)
qqmath(res1)

plot(ple4@catch)
plot(ple4@landings)
plot(fit1, ple4)
as.data.frame(fitSumm(fit1)) %>% 
  rownames_to_column() %>% 
  setNames(c("metric","value"))


logLik(fit1)
AIC(fit1)
BIC(fit1)

f   <- as.formula("~ factor(age)")
s   <- as.formula("~ factor(year)")
q   <- rep(list(as.formula("~ factor(age)")),length(ple4.indices))

fit <- sca(stock = ple4, indices = ple4.indices, fmodel=f, qmodel=q, srmodel=s)
stk <- ple4 + fit
res <- residuals(fit, ple4, ple4.indices)
aic <- AIC(fit)
bic <- BIC(fit)

plot(res)
bubbles(res1)

df <- 
  as.data.frame(fit) %>% 
  bind_rows(data.frame(slot="aic", data=aic, stringsAsFactors = FALSE)) %>% 
  bind_rows(data.frame(slot="bic", data=bic, stringsAsFactors = FALSE)) %>%
  mutate(
    fmodel=as.character(f)[2],
    qmodel=as.character(q)[1],
    srmodel=as.character(s)[2]
  )

f   <- "~ factor(age)"
q   <- "~ factor(age)"
s   <- "~ factor(year)"

store <- list()
i     <- 0

for (f in c("~ factor(age)", "~ s(age, k=4) + s(year, k = 20)" )) {
  for (q in c("~ factor(age)", "~ s(age, k=4)")) {
    for (s in c("~ factor(year)", "~ s(year, k=20)")) {
      i <- i + 1
      print(paste(i,", f=",  f, ", q=", q, ", s=", s, sep=""))
      
      f1                 <- as.formula(f)
      q1                 <- rep(list(as.formula(q)),length(ple4.indices))
      s1                 <- as.formula(s)

      store[[i]]        <- list()
      store[[i]]$fit    <- sca(stock = ple4, indices = ple4.indices, fmodel=f1, qmodel=q1, srmodel=s1)
      store[[i]]$stk    <- ple4 + store[[i]]$fit
      store[[i]]$res    <- residuals(store[[i]]$fit, ple4, ple4.indices)
      store[[i]]$fitsumm<- fitSumm(store[[i]]$fit)
      store[[i]]$ssb    <- ssb(store[[i]]$stk)
      store[[i]]$fbar   <- fbar(store[[i]]$stk)
      
      store[[i]]$aic    <- AIC(store[[i]]$fit)
      store[[i]]$bic    <- BIC(store[[i]]$fit)
      
      store[[i]]$fmodel <- f
      store[[i]]$qmodel <- q
      store[[i]]$srmodel<- s
      
    } # end of s loop
  } # end of q loop
} # end of f loop

for (i in 1:length(store)) {
  store[[i]]$df  <- 
    as.data.frame(store[[i]]$fit) %>% 
    bind_rows(mutate(as.data.frame(store[[i]]$ssb), slot="ssb")) %>% 
    bind_rows(mutate(as.data.frame(store[[i]]$fbar), slot="fbar")) %>% 
    mutate(
      aic = store[[i]]$aic,
      bic = store[[i]]$bic,
      npar = store[[i]]$fitsumm["nopar",],
      fmodel = store[[i]]$fmodel,
      qmodel = store[[i]]$qmodel,
      srmodel = store[[i]]$srmodel
    )
}



df <- store[[i]]$df %>% mutate(i = 1)
for (i in 2:length(store)) {
  df <- bind_rows(df, mutate(store[[i]]$df, i = i))
}  

t <-
  df %>% 
  distinct(i, fmodel, qmodel, srmodel) %>% 
  mutate(desc = paste0("fmodel=",fmodel, ", qmodel=", qmodel, ", srmodel=",srmodel))

df %>% 
  filter(slot=="ssb") %>% 
  ggplot(aes(x=year, y=data, group=i)) + 
  theme_bw() +
  geom_line(aes(colour=as.factor(i))) +
  geom_dl(aes(label  = i, colour = as.factor(i)),
          method = list(dl.combine("first.points","last.points"), cex = 0.8)) 


df %>% 
  filter(slot=="ssb") %>% 
  distinct(i, aic, bic, npar) %>% 
  gather(key=var, value=data, aic:npar) %>% 
  ggplot(aes(x=i, y=data)) +
  theme_bw() +
  geom_line(aes(colour=var)) +
  facet_wrap(~var, scales="free_y")



as.data.frame(STK.index) %>% View()


