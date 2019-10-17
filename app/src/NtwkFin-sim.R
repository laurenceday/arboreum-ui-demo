################################
# Author: Gaurav Singhal
# e-mail: gs.singhal@gmail.com
# Collaborators:
#	Dr Laurence E. Day
#	Pranav Ramkumar
#	Dr Mack Ramachandran
#################################


#### Used R libraries ####
library(MASS)
library(igraph)
detach("package:igraph")
library(network)
library(sna)

#library(dplyr)
#library(Matrix)
# #For utilities
# library(intergraph)
# #For generation
# library(xtable)
# library(MCMCpack)
# library(extraDistr)
# library(predictionet)
# #for optimization
# library(nleqslv)
# library(nloptr)
# #For propagation
# library(doParallel)
# library(bigmemory)

library(modules)
library(here)

source(here::here("app/src/Utils.R"), local=TRUE)

#### Functions to generate Ntwk #####

source(here::here("app/src/Generate.R"), local=TRUE)

#### Functions to perform essential computations on Ntwk ####

source(here::here("app/src/PortfolioCorrelation.R"), local=TRUE)

source(here::here("app/src/Traverse.R"), local=TRUE)

source(here::here("app/src/Propagate.R"), local=TRUE)

#### Example Graph ####
ntwk <- buildCorePeri(N = 100, K = 10)
rsltInit <- initializeSheets(ntwk, K = 10, A0 = 10000)
blnc.DF <- rsltInit[[1]]

rsltCalc <- calcRiskArray(rsltInit[[2]])
ntwk2 <- rsltCalc[[2]]

#borrowing potential of node X1
#start_time <- Sys.time()
brw.Potential <- loan.backProp(ntwk2, 3, algorithm ="NLOPT_GN_ISRES", browse = FALSE,
                               controls = list(controls = list(xtol_rel = 0.1, xtol_abs = c(rep(0.1,2),0.01,0.01),
                                                          relax = FALSE, maxeval = 1000, risk.coef = 'Bernoulli',
                                                          span = 0.5)))
#end_time <- Sys.time()