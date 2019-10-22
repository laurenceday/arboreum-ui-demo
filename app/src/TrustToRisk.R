
library(stats)

source(here::here("app/src/PortfolioCorrelation.R"), local=TRUE)

#' Computes a binomial probability for "Trust" of a node given the below information
#'
#' @param ntwk : network object of class 'network' 
#' @param v    : vertex ID
#' @param assets : amount of assets held by vertex
#' @param equity : amount of equity held by vertex
#' @param p.bwidth : bandwidth parameter to compute RBF kernel matrix for vertex correlation
#' @param p.scale  : scale parameter to compute RBF kernel matrix for vertex correlation
#' @param r.riskfree : risk-free rate of return
#' @param ntwk.attr  : edge attribute containing trust values
#' @param direction  : direction of edge representing extension of trust
#'
#' @return dataframe containing |vrtx from|vrtx to|edge id|risk coef|
#' @export
#'
#' @examples
trust2Risk.solve <- function(ntwk, v, assets, equity, p.bwidth = NULL, p.scale = 1, r.riskfree = 0.05, ntwk.attr = 'Trust', direction = 'out') {
  
  if(is.matrix(ntwk[['val']][[v]]$Portfolio.corr)) {
    corr.mtx <- ntwk[['val']][[v]]$Portfolio.corr
  } else {
    corr.mtx <- correlationUpdate(ntwk, v, p.bwidth = p.bwidth, p.scale = p.scale, direction = direction)
  }
  
  #outgoing neighbor vertices
  v.out <- network::get.neighborhood(ntwk, v, direction)
  
  #stop if no neighbors
  if(length(v.out) == 0) {
    return (list('risk.coef.df' = as.data.frame(list('Vrtx.From' = v, 'Vrtx.To' = NA, 'Edge.To' = NA, 'Risk.coef' = NA)),
                 'risk.aversion' = matrix(1,1,1)))
  }
  if(is.na(corr.mtx)) {
    browser()
  }
  
  #corresponding edge IDs
  edges.out <- sapply(v.out, function (x)  network::get.dyads.eids(ntwk, v, x, neighborhood = direction)[[1]])
  
  #corresponding network attribute
  trust.out <- sapply(ntwk$mel[edges.out],function(x) x$atl[[ntwk.attr]]) #network::get.edge.attribute(ntwk, ntwk.attr)[edges.out]
  
  #compute current trust weights, append entry for equity earning risk-free rate
  weights <- trust.out/sum(trust.out)*(assets-equity)/assets
  weights[length(weights)+1] <- equity/assets
  corr.mtx <- cbind(rbind(corr.mtx,0),0)
  diag(corr.mtx) <- 1
  
  #minimize lagrangian
  w0 <- 1-weights[length(weights)] #portfolio dedicated to lending
  w  <- weights[c(1:(length(weights)-1))]/sum(weights[c(1:(length(weights)-1))]) #normalizing weights to exclude risk-free
  
  #lagrangian to minimize
  lagrangeMVT <- function(R) {
    
    #risk aversion parameter and derivative
    L     <- 1/(w0-2*t(w)%*%R*w0)
    dL.dR <- w*((2*w0)/(w0-2*w0*t(w)%*%R)^2)
    
    #variance and derivative
    if(length(R) == 1) {
      sigma <- sqrt(R*(1-R))
    } else {
      sigma   <- chol(diag(R*(1-R)))
    }
    Var     <- t(w)%*%t(sigma)%*%corr.mtx[c(1:(nrow(corr.mtx)-1)), c(1:(ncol(corr.mtx)-1))]%*%sigma%*%w
    dVar.dR <- t(w)%*%corr.mtx[c(1:(nrow(corr.mtx)-1)), c(1:(ncol(corr.mtx)-1))]%*%w*(1-2*R)    
    
    #objective function (1/2 added later assuming returns = securitization = 0)
    delta <- 1/2*(dL.dR*Var+L*dVar.dR)-w #initially just L*dVar.dR-w
    if(!is.finite(delta)) {
      browser()
    }
    t(delta)%*%delta
  }
  
  #compute results -> solve for R
  x <- tryCatch({
    optim(rbeta(length(weights)-1,2,2)/2, lagrangeMVT, method = 'L-BFGS-B',
          lower = rep(0.01, length(weights)-1), upper = rep(0.49, length(weights)-1))
  }, error = function(e, env = parent.frame()) {
    browser()
  })
  
  R <- 1-x$par #1-Risk = p(no default)
  L <- 1/(w0-2*t(w)%*%R*w0) #risk aversion
  r0 <- (L*w0-1)/(2*L*w0) #proportion of returns from lending vs risk-free
  
  if(any(is.na(R))) {
    browser()
  }
  
  #return dataframe
  if(any(R<0.5)) {
    browser()
  }
  return (list('risk.coef.df' = as.data.frame(list('Vrtx.From' = v, 'Vrtx.To' = v.out, 'Edge.To' = edges.out, 'Risk.coef' = R, 'Weights' = w)), 'risk.aversion' = L))
  
}
