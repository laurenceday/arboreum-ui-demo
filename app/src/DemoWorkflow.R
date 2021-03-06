################################
# Author: Gaurav Singhal
# e-mail: gs.singhal@gmail.com
# Collaborators:
#	Dr Laurence E. Day
#	Pranav Ramkumar
#	Dr Mack Ramachandran
#################################

#### Used R libraries ####
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

library(MASS)
library(igraph)
detach("package:igraph")
library(network)
library(sna)
library(modules)
library(here)

#### Functions to generate Ntwk #####

generate <- modules::use(here::here("ShinyApps/Arboreum/app/src/Generate.R"))
#### Functions to perform essential computations on Ntwk ####
addNode <- modules::use(here::here("ShinyApps/Arboreum/app/src/AddNode.R"))

traverse <- modules::use(here::here("ShinyApps/Arboreum/app/src/Traverse.R"))

propagate <- modules::use(here::here("ShinyApps/Arboreum/app/src/Propagate.R"))

ntwkPlot <- modules::use(here::here("ShinyApps/Arboreum/app/src/NtwkPlot.R"))

#### Generate Example Graph ####
peri <- generate$buildCorePeri(N = 100, K = 10)
saveRDS(peri, file = here::here("ShinyApps/Arboreum/app/pregenerated/CorePeriNetwork.rds"), version = 2)
rslt <- generate$initializeSheets(peri, K = 10, A0 = 10000)
saveRDS(rslt, file = here::here("ShinyApps/Arboreum/app/pregenerated/InitialisedSheets.rds"), version = 2)

rslt0 <- traverse$calcRiskArray(rslt[[2]])
saveRDS(rslt0, file = here::here("ShinyApps/Arboreum/app/pregenerated/CalculatedRiskArrayComposite.rds"), version = 2)
risk.array <- rslt0[[1]]
saveRDS(risk.array, file = here::here("ShinyApps/Arboreum/app/pregenerated/CalculatedRiskArray.rds"), version = 2)
ntwk <- rslt0[[2]]
saveRDS(ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/CalculatedRiskNetwork.rds"), version = 2)
rm(rslt)

saveRDS(networkMovie, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyNetworkMovie.rds"), version = 2)


#### Add Node ####
rslt <- addNode$addNode2Ntwk(ntwk,risk.array,750,
                             out.DF=data.frame(list(nodes=c(1,2,3,4), #this dataframe would be input by UI
                                                    names=c('Mack','Gaurav','Laurence','Pranav'),
                                                    trust=c(325,225,175,275))))
saveRDS(rslt, file = here::here("ShinyApps/Arboreum/app/pregenerated/NodeAddedNetworkComposite.rds"), version = 2)
ntwk <- rslt$ntwk
saveRDS(ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/NodeAddedNetwork.rds"), version = 2)
risk.array <- rslt$risk.array
saveRDS(risk.array, file = here::here("ShinyApps/Arboreum/app/pregenerated/NodeAddedRiskArray.rds"), version = 2)
rm(rslt)
##### Borrow Funds/Propogate Loan ####

#borrowing potential of node X1 -
##NOTE: We can have this stored-results not really affected by AddNode,
##For UI: simply must store results of In.DF which defined nodes that 111 can borrow from (right now randomly generated by AddNode)
start_time <- Sys.time()
ntwk <- rslt$ntwk
brw.Potential <- propagate$loan.backProp(ntwk, 111, algorithm ="NLOPT_GN_ISRES", browse = FALSE,
                                         controls = list(controls = list(xtol_rel = 0.1, xtol_abs = c(rep(0.1,2),0.01,0.01),
                                                                         relax = FALSE, maxeval = 1000,
                                                                         span = 0.5)))
saveRDS(brw.Potential, file = here::here("ShinyApps/Arboreum/app/pregenerated/SelfPotentialComposite.rds"))
saveRDS(brw.Potential$root.S, file = here::here("ShinyApps/Arboreum/app/pregenerated/SelfPotentialFunction.rds"))
saveRDS(brw.Potential$S.list, file = here::here("ShinyApps/Arboreum/app/pregenerated/SelfPotentialSList.rds"))

thirdParty.Potential <- propagate$loan.backProp(ntwk, 69, algorithm ="NLOPT_GN_ISRES", browse = FALSE,
                                              controls = list(controls = list(xtol_rel = 0.1, xtol_abs = c(rep(0.1,2),0.01,0.01),
                                                                              relax = FALSE, maxeval = 1000,
                                                                              span = 0.5)))
saveRDS(thirdParty.Potential, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyPotentialComposite.rds"))
saveRDS(thirdParty.Potential$root.S, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyPotentialFunction.rds"))
saveRDS(thirdParty.Potential$S.list, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyPotentialSList.rds"))

neighbor.Potential <- propagate$loan.backProp(ntwk, 1, algorithm ="NLOPT_GN_ISRES", browse = FALSE,
                                              controls = list(controls = list(xtol_rel = 0.1, xtol_abs = c(rep(0.1,2),0.01,0.01),
                                                                              relax = FALSE, maxeval = 1000,
                                                                              span = 0.5)))
saveRDS(neighbor.Potential, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborPotentialComposite.rds"))
saveRDS(neighbor.Potential$root.S, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborPotentialFunction.rds"))
saveRDS(neighbor.Potential$S.list, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborPotentialSList.rds"))
end_time <- Sys.time()-start_time

#display result
library(plotly)
##NOTE: Issue here is that as Node 111 has 750 of uncommitted capital, it can obviously always borrow >750
z.loess <- matrix(predict(brw.Potential$root.S, expand.grid(R = seq(1.01, 2.5,0.01), Z = seq(0.01, 099,0.01))),
                  length(seq(1.01, 2.5,0.01)), length(seq(0.01, 0.99,0.01)))
self.z.loess <- z.loess
neighbor.z.loess <- matrix(predict(neighbor.Potential$root.S, expand.grid(R = seq(1.01, 2.5,0.01), Z = seq(0.01, 099,0.01))),
                  length(seq(1.01, 2.5,0.01)), length(seq(0.01, 0.99,0.01)))
thirdParty.z.loess <- matrix(predict(thirdParty.Potential$root.S, expand.grid(R = seq(1.01, 2.5,0.01), Z = seq(0.01, 099,0.01))),
                  length(seq(1.01, 2.5,0.01)), length(seq(0.01, 0.99,0.01)))
saveRDS(self.z.loess, file = here::here("ShinyApps/Arboreum/app/pregenerated/LoanFunctionMatrix.rds"))
saveRDS(neighbor.z.loess, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborLoanFunctionMatrix.rds"))
saveRDS(thirdParty.z.loess, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyLoanFunctionMatrix.rds"))

plotly::plot_ly(x=~seq(1.01, 2.5,0.01),y=~seq(0.01, 099,0.01),z=~z.loess) %>% add_surface()

#Forward prop of loan by node X1
##NOTE: We choose borrow 865 (750+115) @ 42% collateral and 10% Interest
start_time <- Sys.time()
self.postBrw.ntwk <- propagate$loan.frwdProp(ntwk,111,brw.Potential$S.list,1020,1.08,0.22,rLim=2.5)

neighbor.postBrw.ntwk <- propagate$loan.frwdProp(ntwk,1,neighbor.Potential$S.list,400,1.10,0.30,rLim=2.5)

thirdParty.postBrw.ntwk <- propagate$loan.frwdProp(ntwk,69,thirdParty.Potential$S.list,150,1.10,0.30,rLim=2.5)


saveRDS(self.postBrw.ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/LoanComposite.rds"))
saveRDS(self.postBrw.ntwk$transactions, file = here::here("ShinyApps/Arboreum/app/pregenerated/LoanTransactions.rds"))
saveRDS(self.postBrw.ntwk$ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/LoanNetwork.rds"))
saveRDS(neighbor.postBrw.ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborLoanComposite.rds"))
saveRDS(neighbor.postBrw.ntwk$transactions, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborLoanTransactions.rds"))
saveRDS(neighbor.postBrw.ntwk$ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/NeighborLoanNetwork.rds"))
saveRDS(thirdParty.postBrw.ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyLoanComposite.rds"))
saveRDS(thirdParty.postBrw.ntwk$transactions, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyLoanTransactions.rds"))
saveRDS(thirdParty.postBrw.ntwk$ntwk, file = here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyLoanNetwork.rds"))
end_time <- Sys.time()-start_time

#### Plot loan  ####
