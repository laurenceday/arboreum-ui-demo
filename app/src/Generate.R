import(dplyr)
import(MCMCpack)
import(extraDistr)
import(predictionet)
import(network)

kumaraswamy <- modules::use(here::here("src/Kumaraswamy.R"))
correlation <- modules::use(here::here("src/PortfolioCorrelation.R"))
trustRisk <- modules::use(here::here("src/TrustToRisk.R"))

#' Initialize Trust, Equity, amounted entrusted, rececived, and borrowed for network
#'
#' @param ntwk : network object
#' @param K : number of core nodes
#' @param A0 : total amount of desired "wealth" in network
#' @param phi.a : shape parameter for beta distribution, generates ratio of assets user decides to "entrust"
#' @param phi.b : shape parameter for beta distribution, generates ratio of assets user decides to "entrust"
#' @param eps : min capitalization ratio
#'
#' @return
#' @export
#'
#' @examples
initializeSheets <- function(ntwk, K, A0 = 1000, phi.a = 3, phi.b = 5, eps = 0.1) {
  
  Bank <- ntwk %v% "vertex.names"
  ntwk.i <- intergraph::asIgraph(ntwk)
  
  n.edg <- igraph::gsize(ntwk.i)    #edges
  vrt.Deg <- igraph::degree(ntwk.i) #degree of vertex
  vrt.Deg.in  <- igraph::degree(ntwk.i,mode='in') #degree of vertex
  vrt.Deg.out <- igraph::degree(ntwk.i,mode='out') #degree of vertex
  n.vrt <- length(vrt.Deg)    #vertices
  
  ##For external balances (based on total links)
  #compute inverse log weighted similarity between vertices -> comp
  sim.mtx <- igraph::similarity(ntwk.i, mode = 'all', method = 'invlogweighted')
  krn.mtx <- as.matrix(Matrix::nearPD(exp(-sim.mtx), ensureSymmetry = TRUE, keepDiag = TRUE, maxit = 1000)[[1]])
  
  #Compute mean value of A
  A_mu <- vrt.Deg/n.edg*A0
  
  #Simulate
  A <- c(MASS::mvrnorm(n = 1, A_mu[1:K], sqrt(A0)*krn.mtx[c(1:K), c(1:K)]), #simulate Core seperately
         MASS::mvrnorm(n = 1, A_mu[(K+1):n.vrt], krn.mtx[c((K+1):n.vrt), c((K+1):n.vrt)]))
  A <- A/sum(A)*A0 #Force centering
  
  ##For debt ratio (based on outgoing nodes)
  sim.mtx <- abs(outer(vrt.Deg.out, vrt.Deg.out, '-'))
  krn.mtx <- as.matrix(Matrix::nearPD(exp(-sim.mtx), ensureSymmetry = TRUE, keepDiag = TRUE, maxit = 1000)[[1]])
  L.krn.mtx <- chol(krn.mtx) #Compute cholesky decomposition
  
  #Generate vector of random draws from beta distribution
  phi <- t(stats::rbeta(n.vrt, phi.a, phi.b)%*%L.krn.mtx)
  
  #Determine Deposits from others/Amount entrusted
  H <- A*(1-eps)*(1+eps*phi)^-1
  E <- A-H #equity
  C <- D <- phi*H
  set.vertex.attribute(ntwk, 'Assets', A)
  set.vertex.attribute(ntwk, 'Equity', E[,1])
  set.vertex.attribute(ntwk, 'PtflAtRisk', H[,1])
  
  #Simulate distribution
  sim.kum.dist <- function(corr.mtx, df) {
    if(is.matrix(corr.mtx)) {
      pvars <- pnorm(mvrnorm(n = 10e4, mu = rep(0, nrow(corr.mtx)),
                             Sigma = corr.mtx))
      binvars <- matrix(nrow = nrow(pvars), ncol = ncol(pvars))
      for(i in c(1:ncol(pvars))) {
        binvars[, i] <- qbinom(pvars[, i], 1, df$Risk.coef[i])
      }
      rm(pvars)
    } else {
      binvars <- as.matrix(rbern(10e4, df$Risk.coef), ncol = 1)
    }
    x <- as.numeric(binvars%*%df$lent/sum(df$lent))
    x[x>= 1] <- 0.9999
    x[x<= 0] <- 0.0001
    
    # Plot
    muHat  <- df$lent%*%(df$Risk.coef/sum(df$lent))
    varHat <- t(sqrt(df$Risk.coef*(1-df$Risk.coef))*df$lent/sum(df$lent))%*%
      corr.mtx%*%
      (sqrt(df$Risk.coef*(1-df$Risk.coef))*df$lent/sum(df$lent))
    fit2.kumar <- kumaraswamy$kumar.MoM(muHat, varHat)
    plot(density(binvars%*%df$lent/sum(df$lent)))
    lines(density(rkumar(10e4, fit2.kumar[1], fit2.kumar[2])), col = 'orange')
  }
  
  #Distribute Trust amongst outgoing vertices (changed to incoming edges)
  set.edge.attribute(ntwk, 'Trust', NA)
  set.edge.attribute(ntwk, 'Trust.coef', NA)
  set.edge.attribute(ntwk, 'Utilized', NA)
  set.edge.attribute(ntwk, 'Risk', NA)
  set.edge.attribute(ntwk, 'Risk.coef', NA)
  set.vertex.attribute(ntwk, 'Risk.aversion', NA)
  set.vertex.attribute(ntwk, 'Portfolio', NA)
  set.vertex.attribute(ntwk, 'Portfolio.corr', NA)
  set.vertex.attribute(ntwk, 'Ptfl.kumPDF.a', NA)
  set.vertex.attribute(ntwk, 'Ptfl.kumPDF.b', NA)
  trustLnt <- rep(0, n.vrt)
  for(v in c(1:n.vrt)) {
    
    # outgoing edges & vertices
    edges.out <- get.edgeIDs(ntwk, v = v, neighborhood = 'in')
    i.vrtc.out <- as.numeric(sapply(get.edges(ntwk, v = v, neighborhood = 'in'), function (x)  x$outl)) #incoming vertices
    
    h <- H[v] #trust
    
    #assign trust to edge if only one edge
    if(length(edges.out) == 1) {
      set.edge.attribute(ntwk, 'Trust', h, edges.out[1])
      set.edge.attribute(ntwk, 'Trust.coef', h, edges.out[1])
      
    } else if(length(edges.out)>1) {
      
      #distribute trust according to total equity
      vrtc.out.e <- E[i.vrtc.out] #corresponding equity of vertices
      for(i in c(1:length(i.vrtc.out))) {
        
        #add incoming equity from other nodes
        j.vrtc.in <- setdiff(which(ntwk[, i.vrtc.out[i]]== 1), v) #changed to incoming
        vrtc.out.e[i] <-  vrtc.out.e[i]+sum(E[j.vrtc.in])
      }
      
      #use equity to draw from dirichlet
      eta <- MCMCpack::rdirichlet(1, vrtc.out.e)
      eta <- (eta/max(eta))*stats::rbeta(1, phi.b+phi.a, abs(phi.b-phi.a))
      
      #distribute accordingly and set values
      set.edge.attribute(ntwk, 'Trust',   h*eta[1,], edges.out)
      set.edge.attribute(ntwk, 'Trust.coef', eta[1,], edges.out)
    }
    
    #Calculate amount utilized
    amt.trst.out <- get.edge.attribute(ntwk,"Trust")[edges.out]
    phi.trst.out <- amt.trst.out*phi[i.vrtc.out]
    if(sum(phi.trst.out)>(1-phi[v])*h) { #.75 should not be hardcoded
      phi.trst.out <- phi.trst.out*(1-phi[v])*h/sum(phi.trst.out)
    }
    set.edge.attribute(ntwk, 'Utilized', phi.trst.out, edges.out)
    trustLnt[v] <- sum(phi.trst.out)
    
    #calculate correlation matrix
    corr.mtx <- correlation$correlationUpdate(ntwk, v, direction = 'in')
    ntwk[['val']][[v]]$Portfolio.corr <- corr.mtx
    
    #convert trust to risk (binomial probability)
    rslt <- trustRisk$trust2Risk.solve(ntwk, v, A[v], E[v], direction = 'in')
    df <- rslt[[1]]
    L  <- rslt[[2]] #risk aversion coefficient
    if(is.na(L) ) {
      browser()
    }
    
    #set risk attributes
    set.vertex.attribute(ntwk, 'Risk.aversion', L[1,1], v = v)
    if(!any(is.na(df)) & nrow(df)>= 1) {
      set.edge.attribute(ntwk, 'Risk.coef', df$Risk.coef, df$Edge.To)
      set.edge.attribute(ntwk, 'Risk',   h*df$Risk.coef, df$Edge.To)
      
      #calculate portfolio and set attributes
      df <- as.data.frame(list('to' = i.vrtc.out,
                               'tot.trust' = amt.trst.out,
                               'lent' = phi.trst.out)) %>%
        left_join(rslt[[1]][, c('Vrtx.To', 'Risk.coef')], by = c('to' = 'Vrtx.To')) %>%
        arrange(to)
      ntwk[['val']][[v]]$Portfolio <- df[, c(1,4,2,3)]
      ntwk[['val']][[v]]$Portfolio.corr <- corr.mtx
      
      #set interest rates on amounts borrowed - define utility function to optimize
      
      ##solve optimization
      soln <- kumaraswamy$optim.Kumar(corr.mtx, df$Risk.coef, W.in = df$lent, penalty.RZ = 10, Rlim = 2.5,
                                      algorithm = 'NLOPT_LN_COBYLA',
                                      controls = list(xtol_rel = 0.1,  xtol_abs = c(rep(0.001, length(df$lent)),0.01,0.01)))
      ntwk[['val']][[v]]$Portfolio$rate <- soln$R
      ntwk[['val']][[v]]$Portfolio$security <- soln$Z
      ntwk[['val']][[v]]$Ptfl.kumPDF.a <- soln$K[1]
      ntwk[['val']][[v]]$Ptfl.kumPDF.b <- soln$K[2]
    }
  }
  
  #Create Assets/Liabilities Dataframes from Portfolios
  set.vertex.attribute(ntwk, 'Assets', NA)
  set.vertex.attribute(ntwk, 'Liabilities', NA)
  for(v in c(1:n.vrt)) {
    if(!is.data.frame(ntwk[['val']][[v]]$Assets)){
      ntwk[['val']][[v]]$Assets <- data.frame(borrower = numeric(),
                                              via = numeric(),
                                              via.trust = numeric(),
                                              risk.coef=numeric(),
                                              amount = numeric(),
                                              rate = numeric(),
                                              security = numeric(),
                                              type=character())
    }
    
    if(!is.data.frame(ntwk[['val']][[v]]$Liabilities)){
      ntwk[['val']][[v]]$Liabilities <- data.frame(borrower = numeric(),
                                                   via = numeric(),
                                                   lender = numeric(),
                                                   amount = numeric(),
                                                   rate = numeric(),
                                                   security = numeric(),
                                                   type=character())
    }
    #if(!is.data.frame(ntwk[['val']][[v]]$Portfolio)){
    #  ntwk[['val']][[v]]$Portfolio <- data.frame(to=numeric(), lent=numeric(), rate=numeric(),security=numeric())
    #}
    
    if(!all(is.na(ntwk[['val']][[v]]$Portfolio))){
      ntwk[['val']][[v]]$Assets[c(1:nrow(ntwk[['val']][[v]]$Portfolio)),
                                c('borrower','via.trust','risk.coef','amount','rate','security')] <-
        ntwk[['val']][[v]]$Portfolio[,c('to','tot.trust','Risk.coef','lent','rate','security')]
      ntwk[['val']][[v]]$Assets$via <- ntwk[['val']][[v]]$Assets$borrower #v
      ntwk[['val']][[v]]$Assets$type <- 'Bond'
    }
  }
  
  for(v in c(1:n.vrt)) {
    if(nrow(ntwk[['val']][[v]]$Assets)>0){
      for(i in c(1:nrow(ntwk[['val']][[v]]$Assets))){
        borrower <- ntwk[['val']][[v]]$Assets$borrower[i]
        newrow <- list('borrower'=borrower,
                       'via'=borrower, #v
                       'lender'=v,
                       'amount'=ntwk[['val']][[v]]$Assets$amount[i],
                       'rate'=ntwk[['val']][[v]]$Assets$rate[i],
                       'security'=ntwk[['val']][[v]]$Assets$security[i],
                       'type'='loan-origin')
        ntwk[['val']][[borrower]]$Liabilities <- rbind(ntwk[['val']][[borrower]]$Liabilities,newrow)
      }
    }
    ntwk[['val']][[v]]$Liabilities$type <- as.character(ntwk[['val']][[v]]$Liabilities$type)
  }
  
  trustLnt[is.na(trustLnt)] <- 0
  
  #Sum entrusted amounts to find total amount entrusted
  trustRcv <- rep(0, n.vrt)
  for(v in c(1:n.vrt)) {
    trustRcv[v] <- sum(get.edge.attribute(ntwk,"Trust")[get.edgeIDs(ntwk, v = v, neighborhood = 'out')])
  }
  
  trustRcv[is.na(trustRcv)] <- 0
  trustBrw <- phi[1,]*trustRcv # Borrowed from others (trust channel used)
  
  #Build Balance Sheet
  Balance.Sheets <- as.data.frame(list('A' = A, 'H' = H, 'E' = E, 'C' = C, 'D' = D, 'trustGvn' = H, 'trustRcv' = trustRcv, 'trustBrw' = trustBrw, 'trustLnt' = trustLnt))
  
  return (list(Balance.Sheets, ntwk))
  
}


#' Create Core-Periphery network where edges generated via preferential attachment
#'
#' @param N : n periphery nodes
#' @param K : n core nodes
#' @param a.peri : power of preferential attachment for periphery, where P(attach) ~ (in-degree)^a
#' @param a.core : power of preferential attachment for core
#' @param a.coreperi : power of preferential attachment from core to peri and vise versa
#' @param b.peri : shape parameter for beta distribution of edges per node for periphery,
#'                 increasing values skew distribution rightwards
#' @param b.core : shape parameter for beta distribution of edges per node for core,
#'                 increasing values skew distribution rightwards, negative values makes distribution more uniform
#'
#' @return
#' @export
#'
#' @examples
buildCorePeri <-  function(N = 50, K = 5,
                           a.peri = 1, a.core = 1, a.coreperi = 1,
                           b.peri = 0, b.core = 0 ) {
  
  # Merge Base and Core
  merge.barabasi <- function(net.core, net.base, alpha) {
    
    #calculate in-degree
    deg.core <- igraph::degree(net.core, mode = 'in')
    deg.base <- igraph::degree(net.base, mode = 'in')
    
    #compute probability of attachment using Barabasi-Albert model
    deg.core <- cbind(deg.core,
                      ((deg.core^(alpha)+1)/(rep(sum(deg.core), length(deg.core))-deg.core)))
    deg.base <- cbind(deg.base,
                      ((deg.base^(alpha)+1)/(rep(sum(deg.base), length(deg.base))-deg.base)))
    deg.core[,2] <- deg.core[,2]/sum(deg.core[,2])
    
    #generate edges from core to base:
    edges.core2base <- apply(deg.base,1, function (x)  stats::rbinom(n = igraph::gorder(net.core), size = 1, p = x[2]))
    edges.core2base <- reshape2::melt(edges.core2base)
    edges.core2base <- edges.core2base[edges.core2base[,3]== 1,]
    edges.core2base[,2] <- edges.core2base[,2]+igraph::gorder(net.core)
    
    #generate edges from base to core:
    edges.base2core <- apply(deg.core,1, function (x)  stats::rbinom(n = igraph::gorder(net.base), size = 1, p = x[2]))
    edges.base2core <- reshape2::melt(edges.base2core)
    edges.base2core <- edges.base2core[edges.base2core[,3]== 1,]
    edges.base2core[,1] <- edges.base2core[,1]+igraph::gorder(net.core)
    
    #Merge networks, add edges
    net.all <- net.core+net.base
    net.all <- igraph::add.edges(net.all, c(t(as.matrix(edges.core2base[, c(1,2)]))))
    net.all <- igraph::add.edges(net.all, c(t(as.matrix(edges.base2core[, c(1,2)]))))
    
    #Remove cycles
    ntwk <- predictionet::adj.remove.cycles(igraph::as_adjacency_matrix(net.all, sparse = FALSE), maxlength = 10) #round(igraph::gorder(net.all)/2)
    return (as.network(ntwk$adjmat.acyclic, directed = TRUE))
  }
  
  # Generate Base
  net.base <- igraph::barabasi.game(N, power = a.peri, out.dist = extraDistr::dbbinom(c(0:N), size = N, alpha = log(N)*2*(1+b.peri), beta = N*2*1/(1+b.peri)))
  
  # Generate Core
  net.core <- igraph::barabasi.game(K, power = a.core, out.dist = extraDistr::dbbinom(c(0:K), size = K, alpha = K^((K-1)/K)*(1+b.core), beta = K^((K-1)/K)*1/(1+b.core)))
  
  # Merge base and core graphs
  net.cp <- merge.barabasi(net.core, net.base, a.coreperi)
  
  # Collect Information About Generated Network
  Degree.Distribution <- sna::degree(as.matrix(net.cp), gmode = "digraph")
  Centralization <- sna::centralization(as.matrix(net.cp), sna::degree)
  Connectivity <- sna::gden(as.matrix(net.cp))
  
  
  net.cp %v% "deg" <- Degree.Distribution
  net.cp %n% "Centralization" <- Centralization
  net.cp %n% "Connectivity" <- Connectivity
  
  return (net.cp)
}