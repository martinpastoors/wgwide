# translate SAM data object to FLR stok object for 

# install.packages("FLCore", repos="http://flr-project.org/R")
# library(usethis)
# library(devtools)
# install_github("flr/FLCore")
# library(lattice)
# library(iterators)
#devtools::install_github("fishfollower/SAM/stockassessment", #INSTALL_opts=c("--no-multiarch"))
# install.packages("FLSAM", repos="http://flr-project.org/R")
devtools::install_github("shfischer/FLfse/FLfse")

library(stockassessment)
library(FLCore)
library(FLfse)
#library(FLSAM)

##Name of the model run in stockassessment.org
fitSAM <- fitfromweb('WBSS_HAWG_2021_sf')
WBH <- FLfse::SAM2FLStock(fitSAM, catch_estimate = FALSE) 
# save(WBH, file = "C:/Users/massi/OneDrive/Dokument/Max files for backup/Documents/Commitees/ICES/WKREF1-2/Database/HAWG/WBSS_2021.RData")
plot(WBH)

