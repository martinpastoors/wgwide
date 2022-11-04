library(devtools)
library(pkgbuild)

sessionInfo()

check_build_tools()

check_compiler()

rtools_needed()
check_rtools()
rtools_path()
find_rtools()

install.packages("Matrix", type = "source")
install.packages("Rcpp", type = "source")
install.packages("RcppEigen", type = "source")
install.packages("TMB", type = "source")
install.packages("ellipse", type = "source")


tools::Rcmd("config --ldflags")
tools::Rcmd("config --cppflags")

tools::Rcmd("config --all")



## TRY1, Using devtools, mutiarch    # NOT WORKING
# devtools::install_github("fishfollower/SAM/stockassessment")


## TRY2, Using devtools, 64bit?
devtools::install_github("fishfollower/SAM/stockassessment", INSTALL_opts=c("--no-multiarch"))

## TRY3, Using devtools, 64bit?
devtools::install_github("fishfollower/SAM/stockassessment", build_opts = c("--no-multiarch"))

## TRY4, Using base R, multiarch
source("https://calbertsen.dk/installFromGithub/fishfollower/SAM")

## TRY5, Using base R, 64bit
source("https://calbertsen.dk/installFromGithub/fishfollower/SAM,installArgs=c('--no-multiarch')")

