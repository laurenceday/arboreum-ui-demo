import(stats)
import(dplyr)
import(doParallel)
import(bigmemory)
import(foreach)

utils    <- modules::use(here::here("src/Utils.R"))

kumaraswamy <- modules::use(here::here("src/Kumaraswamy.R"))

correlation <- modules::use(here::here("src/PortfolioCorrelation.R"))

#' Solves individual (node-specific) consumption function for a hypothetical loan originated by a borrower somewhere in the network
#' by solving portfolio optimization over a mesh of Interest Rates and Securitization ratios
#' 
#' @param ntwk - network object (of class network)
#' @param v - node for whom to solve optimization problem
#' @param orgn.brw.mtx - matrix with the pairs of loan-propagators within the neighborhood of v and the original borrower (not necessarily in neighborhood of v)
#' @param S.out - list of loess functions that output amount demanded downstream given rate (R) and securitization (Z)
#' @param zLim - maximum securitization rate
#' @param rLim - maximum loan rate
#' @param r.mesh - coarseness of grid along R dimension (optimization solved for every R,Z combination)
#' @param z.mesh - coarseness of grid along Z dimension (optimization solved for every R,Z combination)
#' @param algorithm - non-linear optimization algorithm, either NLOPT_LN_COBYLA or NLOPT_GN_ISRES (see documentation for nloptr for details)
#' @param controls - optimization control parameters (see documentation for nloptr for details)
#' @param browse - TRUE/FALSE to invoked browser (for debugging purposes)
#'
#' @return Matrix of amount of debt purchased for grid returned by r.mesh and z.mesh
#' @export
#'
#' @examples
cnsm.ZR.backsolve <- function(ntwk, v, orgn.brw.mtx, S.out = list(),
                              zLim = 0.99, rLim = 2.5, r.mesh = 0.01, z.mesh = 0.01,
                              algorithm ="NLOPT_LN_COBYLA", controls = list(), browse = FALSE) { #Zs.vec, Rs.vec
  
  #Key intuition: solve for C for every combination of Zc, Rc and Zs, Rs, S given
  # As we know optimization is convex, topology of surface is smooth
  # Also solution space is nicely bounded
  # Ergo solving C for sufficient Zc and Rc allows to linearly interpolate optimum
  
  #Rc := rate at which v-1 sells loan of v.brw to v
  Rc <- seq(1.01, rLim, r.mesh)
  
  #Zc:= layer of securitization added by v-1 before loan of v.brw propogated along to v
  Zc <- seq(0.01, zLim, z.mesh)
  
  #orgn.brw.mtx is a matrix with pairs of loan-originators amongst the neighborhood of v, 
  #and borrowers(not necessarily in neighborhood of v)
  v.orgn <- unique(orgn.brw.mtx[,1]) 
  v.brw  <- unique(orgn.brw.mtx[,2])
  i.brw  <- match(orgn.brw.mtx[,2], v.brw)
  n.brw  <- length(v.brw)
  
  #Retrieve existing correlation matrix for portfolio of v
  corr.mtx <- correlation$correlationUpdate(ntwk, v, v.brw, direction = 'in')
  
  #check if borrower currently in loan portfolio
  v.brw.in.ptfl <- intersect(v.brw, as.numeric(ntwk$val[[v]]$Assets$borrower))
  if(length(v.brw.in.ptfl)>0) { #convert identifier to negative number if so
    indx.b <- match(v.brw.in.ptfl, as.numeric(rownames(corr.mtx)))
    rownames(corr.mtx)[indx.b] <- -as.numeric(rownames(corr.mtx)[indx.b])
    v.brw <- -1*v.brw
  }
  v.corr <- as.numeric(rownames(corr.mtx))
  #check if first loan in portfolio (as asset)
  if(all(corr.mtx==1)){v.corr <- v.brw}
  
  #update correlation matrix to include purchases (consumption) from v.orgn and sales of v.brw
  indx.c <- match(v.brw, v.corr) #consumption vectors #orgn.brw.mtx[,2]
  indx.s <- match(v.brw, v.corr) #sales vectors
  indx.v <- c(which(!(v.corr %in% v.brw)), indx.c, indx.s) #original portfolio
  corr.mtx <- corr.mtx[indx.v, indx.v]
  
  #update indices denoting c and v in new matrix
  indx.c <- c((nrow(corr.mtx)-length(indx.c)-length(indx.s)+1):(nrow(corr.mtx)-length(indx.s)))
  indx.s <- c((nrow(corr.mtx)-length(indx.s)+1):nrow(corr.mtx))
  corr.mtx[indx.s,] <- corr.mtx[indx.s,]*-1
  corr.mtx[-indx.s, indx.s] <- corr.mtx[-indx.s, indx.s]*-1
  diag(corr.mtx) <- 1
  
  #Retrieve existing portfolio of v
  ptfl.DF <- ntwk[['val']][[v]]$Portfolio
  if(!is.data.frame(ptfl.DF)){ptfl.DF <- data.frame(to=numeric(), lent=numeric(), rate=numeric(),security=numeric())}
  ptfl.DF <- ptfl.DF[match(ptfl.DF$to, setdiff(v.corr, v.brw)),]
  lent.Vec <- c(ptfl.DF$lent, rep(NA,2*length(v.brw))) #nrow(orgn.brw.mtx)+
  rate.Vec <- c(ptfl.DF$rate, rep(NA,2*length(v.brw)))
  scrt.Vec <- c(ptfl.DF$security, rep(NA,2*length(v.brw)))
  v.brw <- abs(v.brw)
  
  #Retrieve subjective risk of borrower
  #Below routine should be replaced with routine that computes risk on fly from all paths to borrower
  #retrieve current risk, lent, rate, securitization vectors, update to include new borrower
  risk.Mtx <- ntwk[['val']][[v]]$Subj.risk
  risk.Mtx <- risk.Mtx[match(c(ptfl.DF$to, v.brw, v.brw), risk.Mtx$to),] #orgn.brw.mtx[,2]
  risk.Mtx <- as.matrix(risk.Mtx[, c(2,3,4)])
  risk.Mtx[risk.Mtx[,'Risk.coef']>0.995,'Risk.coef'] <- 0.995
  P.indx <- c(1,2,3)
  if(any(is.na(risk.Mtx))){browser()}
  
  #limit amount that can be lent
  amt.lent <- ptfl.DF[match(orgn.brw.mtx[,1], ptfl.DF$to),'lent']
  amt.lent[is.na(amt.lent)] <- 0
  tot.trust <- sapply(orgn.brw.mtx[,1],
                      function(x) ntwk$mel[[network::get.dyads.eids(ntwk, v, x, neighborhood = 'in')[[1]]]]$atl$Trust)
  lend.lim <- tot.trust-amt.lent
  names(lend.lim) <- orgn.brw.mtx[,2]
  lend.df <- as.data.frame(list(Orgn = orgn.brw.mtx[,1], Brw = orgn.brw.mtx[,2], lend.lim = lend.lim))%>%
    group_by(Brw)%>%mutate(lend.pct = lend.lim/sum(lend.lim)) #to divy up consumption by originators
  lend.lim <- rowsum(lend.lim, names(lend.lim)) #sum by borrower
  open.fnd <- ntwk[['val']][[v]]$PtflAtRisk-sum(ptfl.DF$lent) #total unencumbered funds
  lend.lim <- pmin(open.fnd*lend.lim/sum(lend.lim), lend.lim) #proportionally allocate by lend.lim (choose lowest)
  lend.lim <- lend.lim[match(rownames(lend.lim), v.brw)] #put in correct order
  
  #parse controls
  if('relax' %in% names(controls)) {
    relax <- controls$relax
    controls <- controls[!(names(controls) == 'relax')]
  } else {
    relax <- FALSE
  } 
  if('risk.coef' %in% names(controls)) {
    if(controls$risk.coef == 'Bernoulli') {
      P.indx <- 1
    }
    if(controls$risk.coef == 'Hybrid') {
      risk.Mtx[-c(indx.c, indx.s), c(2,3)] <- NA
    }
    controls <- controls[!(names(controls) == 'risk.coef')]
  }
  if('span' %in% names(controls)) {
    span <- controls$span
    controls <- controls[!(names(controls) == 'span')]
  } else {
    span <-0.25
  }
  
  #S is the downstream supply function for v.brw -> must coalesce & turn into LOESS smoothing functions
  #smoothing function generator
  smooth.loess <- function(C.ZR) {
    C.ZR <- round(C.ZR,2)
    df <-  reshape2::melt(add_rownames(as.data.frame(C.ZR, row.names = rownames(C.ZR)))) %>% 
      mutate(R = as.numeric(rowname), Z = as.numeric(as.character(variable)))
    
    #add border cases
    df <- rbind(df, c(0,0,0), c(1,1, max(df$value)))
    loess.fit <-loess(value~R+Z, df, control = loess.control(surface = 'direct'), span = span, degree = 2) #tune
    return (loess.fit)
  }
  S.TF <- c()
  if(length(S.out) == 0) {
    S.TF <- rep(FALSE, n.brw)
    S.FN <- c()
  } else {
    #coalesce if multiple lists so length = n.brw
    v.s <- as.numeric(sapply(strsplit(names(S.out), "\\."), "[", 2)) #loan supply functions in list
    S.TF <- v.brw %in% v.s #for functions that don't match -> eliminate from S
    v.brw.s <- v.brw[S.TF]
    S.FN <- list()
    for(i in c(1:length(v.brw.s))) { #alternative option is to create separate model for each matrix
      #S.FN[[i]] <- lapply(S.out[v.s %in% v.brw.s[i]],smooth.loess)  
      S.FN[[i]] <- smooth.loess(Reduce('+', S.out[v.s %in% v.brw.s[i]])) #reduce and create one model
    }
    names(S.FN) <- v.brw.s
  }
  rm(S.out)
  
  #Iterate through S.mtx, Rc, & Zc, update wgt.Vec, rate.Vec, and scrt.Vec, optimize missing value (C)
  S.indx <- which(!S.TF)
  optim.C <- function(r, z) {
    P <- risk.Mtx
    W <- lent.Vec
    R <- rate.Vec
    Z <- scrt.Vec
    
    #set new values
    W[indx.s] <- NA
    R[indx.c] <- r
    Z[indx.c] <- z
    
    #eliminate rows with 0 amounts to be sold
    if(length(S.indx)>0) {
      P <- P[-indx.s[S.indx],]
      W <- W[-indx.s[S.indx]]
      R <- R[-indx.s[S.indx]]
      Z <- Z[-indx.s[S.indx]]
      indx.mtx <- -indx.s[S.indx]
    } else {
      indx.mtx <- c(1:nrow(corr.mtx))
    }
    #solve
    soln <- kumaraswamy$optim.Kumar(corr.mtx[indx.mtx, indx.mtx],
                                    P.in = P[, P.indx], W.in = W, R.in = R, Z.in = Z, 
                                    Wlim = lend.lim, Rlim = rLim, S.FN = S.FN,
                                    algorithm = algorithm, controls = controls, relax = relax, browse = browse)
    
    #FIX - at the moment only returns C (amount consumed), and not C+S (amount consumed+sold)
    # -> FIXED! amount sold is soln$S
    if(!is.na(soln$S)){
      return (soln$W+soln$S) 
    } else {
      return (soln$W)
    }
  }
  
  #for debugging purposes
  if(browse) {
    C.ZR <- matrix(nrow = length(Rc), ncol = length(Zc))
    browse <- TRUE
    browser()
    C.ZR[1,1] <- optim.C(Rc[1], Zc[1])
    for(i in c(1:length(Rc))) {
      for(j in c(1:length(Zc))) {
        tryCatch({
          browse <- FALSE
          C.ZR[i, j] <- optim.C(Rc[i], Zc[j])
        }, error = function(e) {
          browse <- TRUE
          browser()
          C.ZR[i, j] <- optim.C(Rc[i], Zc[j])
        })
      }
    }
    browser()
  }
  
  #parallelize
  no_cores <- parallel::detectCores() - 2  
  registerDoParallel(cores = no_cores)  
  ostype <- if (Sys.info()[['sysname']] == 'Windows') {"PSOCK"} else {"FORK"}
  clst <- parallel::makeCluster(no_cores, type = ostype)
  on.exit(parallel::stopCluster(clst))
  #data storage object
  V <- big.matrix(length(Rc), length(Zc)*n.brw)
  desc <- describe(V)
  #parallel for loop
  result = foreach(i = c(1:length(Rc)))%:%foreach(j = c(1:length(Zc)))%dopar%
    {
      V <- bigmemory::attach.big.matrix(desc)
      V[i, c(j:(j+n.brw-1))] <- optim.C(Rc[i], Zc[j])
    }
  V <- as.matrix(V)
  
  #Divy results by lend.DF and store
  cnt <- 1
  rslt <- list()
  for(i in c(1:n.brw)) {
    C.ZR <- V[, c(seq(i,(length(Zc)-1)*n.brw, n.brw),(length(Zc)-1)*n.brw+i)]
    rownames(C.ZR) <- Rc
    colnames(C.ZR) <- Zc
    i.brw <- which(lend.df$Brw == v.brw[i])
    for(j in i.brw) {
      C.ZR.i <- C.ZR*lend.df$lend.pct[j]
      rslt[[cnt]] <- C.ZR.i
      names(rslt)[cnt] <- paste0(v, '_', lend.df$Orgn[j], '.', lend.df$Brw[j])
      cnt <- cnt+1
    }
  }
  return (rslt)
  
  #Smooth result (for debugging purposes)
  # z.loess <- matrix(predict(loess.fit.2, expand.grid(R = seq(1.01, rLim,0.01), Z = seq(0.01, zLim,0.01))),
  #                   length(seq(1.01, rLim,0.01)), length(seq(0.01, zLim,0.01)))
  # gam.fit <- gam(value ~ te(R, Z, bs = c("cs", "cs")), data = df)
  # z.gam <- matrix(predict(gam.fit, expand.grid(R = seq(1.01, rLim,0.01), Z = seq(0.01, zLim,0.01))),
  #                 length(seq(1.01, rLim,0.01)), length(seq(0.01, zLim,0.01)))
  # persp3d(seq(1.01, rLim,0.01), seq(0.01, zLim,0.01), z.gam, col = 'skyblue')
  # persp3d(seq(1.01, rLim,0.01), seq(0.01, zLim,0.01), z.gam-z.loess, col = 'skyblue')
  # persp3d(seq(1.01, rLim,0.01), seq(0.01, zLim,0.01), z.loess, col = 'skyblue')
}

#' For a given root node who desires to borrow funds, will solve for the maximum amounts borrowable
#' for every combination of interest rate (R) and securitization rate (Z) 
#' via interrogating the network of the root and back-solving
#'
#' @param ntwk - network object (of class network)
#' @param root - node that desires to borrow funds
#' @param zLim - maximum securitization rate
#' @param rLim - maximum loan rate
#' @param r.mesh - coarseness of grid along R dimension (optimization solved for every R,Z combination)
#' @param z.mesh - coarseness of grid along Z dimension (optimization solved for every R,Z combination)
#' @param algorithm - non-linear optimization algorithm, either NLOPT_LN_COBYLA or NLOPT_GN_ISRES (see documentation for nloptr for details)
#' @param controls - control parameters including span for loess estimator, 
#' risk.coef for risk coefficients are calculated (Bernoulli sets variance to p(1-p), Beta uses beta-distribution estimator, Hybrid uses Bernoulli for root loan and Beta for all else ), 
#' and optimization controls (see documentation for nloptr for details)
#' @param browse - T/F invokes browser (for debugging)
#'
#' @return LOESS function of amount borrowable for every combination of R and Z
#' @export
#'
#' @examples
loan.backProp <- function(ntwk, root,
                          zLim = 0.99, rLim = 2.5, r.mesh = 0.025, z.mesh = 0.025,
                          algorithm ="NLOPT_GN_ISRES",
                          controls = list(maxeval = 1000, span = 0.5,xtol_rel=0.1,
                                          xtol_abs=c(0.5,0.01,0.01,0.001,0.001), span=0.5),
                          browse = FALSE) {
  
  subntwk.EL <- utils$rtrv.lcl.Edgelist(ntwk,root,remove.cycles.len = 0)
  back.order <- setdiff(utils$postorder.DFS(subntwk.EL, root), root)
  
  #If back.order is NULL then return
  if(length(back.order)==0){
    return(NULL)
  }
  
  if('relax' %in% names(controls)) {
    relax <- controls$relax
    controls <- controls[-(names(controls) == 'relax')]
  } else {
    relax <- FALSE
  } 
  if('span' %in% names(controls)) {
    span <- controls$span
  } 
  
  #begin back-propogation
  S.out <- list()
  
  for(v in back.order) {
    
    #required vertices for backsolving
    v.out <- paste0(subntwk.EL[subntwk.EL[, 'from']== v, 'to'], '_', v)
    
    print(paste0('Back-Prop to ',v,' from {',paste(subntwk.EL[subntwk.EL[, 'from']== v, 'to'],collapse=',') ,'}'))
    
    #current vertices in S.out
    if(length(S.out) == 0) {
      v.S <- integer(0)
    } else {
      v.S <-  sapply(strsplit(names(S.out), "\\."), "[", 1)
    }
    
    if(sum(v.S %in% v.out) == 0 & algorithm!= 'NLOPT_LN_COBYLA') {
      relax.cntl <- FALSE
    } else {
      relax.cntl <- relax
    }
    
    #create orgn.brw.mtx
    orgn.brw.mtx <- subntwk.EL[subntwk.EL[, 'to']== v, 'from']
    orgn.brw.mtx <- as.matrix(cbind('orgn' = orgn.brw.mtx, 'brw' = rep(root, length(orgn.brw.mtx))))
    
    #invoke backsolve
    S <- tryCatch({cnsm.ZR.backsolve(ntwk = ntwk, v = v, orgn.brw.mtx = orgn.brw.mtx,
                                     S.out = S.out[v.S %in% v.out],
                                     zLim = zLim, rLim = rLim, r.mesh = r.mesh, z.mesh = z.mesh,
                                     algorithm = algorithm, controls = c(controls, list(relax = relax.cntl)),
                                     browse = browse)
    }, error = function(e) {
      browser()
      cnsm.ZR.backsolve(ntwk = ntwk, v = v, orgn.brw.mtx = orgn.brw.mtx,
                        S.out = S.out[v.S %in% v.out],
                        zLim = zLim, rLim = rLim, r.mesh = r.mesh, z.mesh = z.mesh,
                        algorithm = algorithm, controls = c(controls, list(relax = relax.cntl)),
                        browse = TRUE)
      browser()
    })
    
    S.out <- c(S.out, S)
  }
  
  #vertices directly lending to root
  v.lend2root <- paste0(subntwk.EL[subntwk.EL[, 'from']== root, 'to'], '_', root)
  
  #current vertices in S.out
  v.S <-  sapply(strsplit(names(S.out), "\\."), "[", 1)
  
  #reduce S.out to vertices strictly lending to root
  C.ZR <- Reduce('+', S.out[v.S %in% v.lend2root])
  
  #smooth  
  df <-  reshape2::melt(add_rownames(as.data.frame(round(C.ZR,2), row.names = rownames(C.ZR)))) %>% 
    mutate(R = as.numeric(rowname), Z = as.numeric(as.character(variable)))
  loess.fit <-loess(value~R+Z, df, control = loess.control(surface = 'direct'), span =.5, degree = 2) #tune
  return (list('root.S' = loess.fit, 'S.list' = S.out))
}

#' After borrower chooses loan terms, propagates amounts and solves for portfoliios of rest of nodes, along with 
#' amounts transferred/propagated to other nodes
#' 
#' @param ntwk - network object (of class network)
#' @param v - node for whom to solve optimization problem
#' @param prop.mtx - matrix with the pairs of loan-propagators within the neighborhood of v and the original borrower (not necessarily in neighborhood of v)
#' @param S.out - list of loess functions that output amount demanded downstream given rate (R) and securitization (Z)
#' @param rLim - maximum loan rate
#' @param algorithm - non-linear optimization algorithm, either NLOPT_LN_COBYLA or NLOPT_GN_ISRES (see documentation for nloptr for details)
#' @param controls - optimization control parameters (see documentation for nloptr for details)
#' @param browse - TRUE/FALSE to invoked browser (for debugging purposes)
#'
#' @return Matrix of amount of debt purchased for grid returned by r.mesh and z.mesh
#' @export
#'
#' @examples
cnsm.ZR.frwdsolve <- function(ntwk, v, prop.mtx, S.out = list(),zLim = 0.99, rLim = 2.5,
                              algorithm ="NLOPT_GN_ISRES", 
                              controls = list(maxeval=5000,xtol_rel=0.1,xtol_abs=c(0.01,0.01,0.01,0.001,0.001)), 
                              browse = FALSE) {
  
  #orgn.brw.mtx is a matrix with pairs of loan-originators amongst the neighborhood of v, 
  #and borrowers(not necessarily in neighborhood of v)
  v.brw  <- unique(prop.mtx[,1])
  v.orgn <- unique(prop.mtx[,2]) 
  i.brw  <- match(prop.mtx[,1], v.brw)
  n.brw  <- length(v.brw)
  
  #Combine if multiple sources
  if(nrow(prop.mtx)<2){
    Consumed <- prop.mtx[,'Amt.C']
    Rate <- prop.mtx[,'Rate.C']
    Collateral <- prop.mtx[,'Scrt.C'] 
  } else {
    Consumed <- sum(prop.mtx[,'Amt.C'])
    Rate <- weighted.mean(prop.mtx[,'Rate.C'],prop.mtx[,'Amt.C'])
    Collateral <- weighted.mean(prop.mtx[,'Scrt.C'],prop.mtx[,'Amt.C'])
  }
  
  #Retrieve existing correlation matrix for portfolio of v
  corr.mtx <- correlation$correlationUpdate(ntwk, v, v.brw, direction = 'in')
  
  #check if borrower currently in loan portfolio
  v.brw.in.ptfl <- intersect(v.brw, as.numeric(ntwk$val[[v]]$Assets$borrower))
  if(length(v.brw.in.ptfl)>0) { #convert identifier to negative number if so
    indx.b <- match(v.brw.in.ptfl, as.numeric(rownames(corr.mtx)))
    rownames(corr.mtx)[indx.b] <- -as.numeric(rownames(corr.mtx)[indx.b])
    v.brw <- -1*v.brw
  }
  v.corr <- as.numeric(rownames(corr.mtx))
  #check if first loan in portfolio (as asset)
  if(all(corr.mtx==1)){v.corr <- v.brw}
  
  #update correlation matrix to include purchases (consumption) from v.orgn and sales of v.brw
  indx.c <- match(v.brw, v.corr) #consumption vectors #orgn.brw.mtx[,2]
  indx.s <- match(v.brw, v.corr) #sales vectors
  indx.v <- c(which(!(v.corr %in% v.brw)), indx.c, indx.s) #original portfolio
  corr.mtx <- corr.mtx[indx.v, indx.v]
  
  #update indices denoting c and v in new matrix
  indx.c <- c((nrow(corr.mtx)-length(indx.c)-length(indx.s)+1):(nrow(corr.mtx)-length(indx.s)))
  indx.s <- c((nrow(corr.mtx)-length(indx.s)+1):nrow(corr.mtx))
  corr.mtx[indx.s,] <- corr.mtx[indx.s,]*-1
  corr.mtx[-indx.s, indx.s] <- corr.mtx[-indx.s, indx.s]*-1
  diag(corr.mtx) <- 1
  
  #Retrieve existing portfolio of v
  ptfl.DF <- ntwk[['val']][[v]]$Portfolio
  if(!is.data.frame(ptfl.DF)){ptfl.DF <- data.frame(to=numeric(), lent=numeric(), rate=numeric(),security=numeric())}
  ptfl.DF <- ptfl.DF[match(ptfl.DF$to, setdiff(v.corr, v.brw)),]
  lent.Vec <- c(ptfl.DF$lent, rep(NA,2*length(v.brw))) #nrow(orgn.brw.mtx)+
  rate.Vec <- c(ptfl.DF$rate, rep(NA,2*length(v.brw)))
  scrt.Vec <- c(ptfl.DF$security, rep(NA,2*length(v.brw)))
  v.brw <- abs(v.brw)
  
  #Retrieve subjective risk of borrower
  #Below routine should be replaced with routine that computes risk on fly from all paths to borrower
  #retrieve current risk, lent, rate, securitization vectors, update to include new borrower
  risk.Mtx <- ntwk[['val']][[v]]$Subj.risk
  risk.Mtx <- risk.Mtx[match(c(ptfl.DF$to, v.brw, v.brw), risk.Mtx$to),] #orgn.brw.mtx[,2]
  risk.Mtx <- as.matrix(risk.Mtx[, c(2,3,4)])
  risk.Mtx[risk.Mtx[,'Risk.coef']>0.995,'Risk.coef'] <- 0.995
  P.indx <- c(1,2,3)
  if(any(is.na(risk.Mtx))){browser()}
  
  #limit amount that can be lent
  amt.lent <- ptfl.DF[match(prop.mtx[,2], ptfl.DF$to),'lent']
  amt.lent[is.na(amt.lent)] <- 0
  tot.trust <- sapply(prop.mtx[,2],
                      function(x) ntwk$mel[[network::get.dyads.eids(ntwk, v, x, neighborhood = 'in')[[1]]]]$atl$Trust)
  lend.lim <- tot.trust-amt.lent
  names(lend.lim) <- prop.mtx[,1]
  lend.df <- as.data.frame(list(Orgn = prop.mtx[,2], Brw = prop.mtx[,2], lend.lim = lend.lim))%>%
    group_by(Brw)%>%mutate(lend.pct = lend.lim/sum(lend.lim)) #to divy up consumption by originators
  lend.lim <- rowsum(lend.lim, names(lend.lim)) #sum by borrower
  open.fnd <- ntwk[['val']][[v]]$PtflAtRisk-sum(ptfl.DF$lent) #total unencumbered funds
  lend.lim <- pmin(open.fnd*lend.lim/sum(lend.lim), lend.lim) #proportionally allocate by lend.lim (choose lowest)
  lend.lim <- lend.lim[match(rownames(lend.lim), v.brw)] #put in correct order
  
  #parse controls
  if('relax' %in% names(controls)) {
    relax <- controls$relax
    controls <- controls[!(names(controls) == 'relax')]
  } else {
    relax <- FALSE
  } 
  if('risk.coef' %in% names(controls)) {
    if(controls$risk.coef == 'Bernoulli') {
      P.indx <- 1
    }
    if(controls$risk.coef == 'Hybrid') {
      risk.Mtx[-c(indx.c, indx.s), c(2,3)] <- NA
    }
    controls <- controls[!(names(controls) == 'risk.coef')]
  }
  if('span' %in% names(controls)) {
    span <- controls$span
    controls <- controls[!(names(controls) == 'span')]
  } else {
    span <-0.25
  }
  
  #S is the downstream supply function for v.brw -> must coalesce & turn into LOESS smoothing functions
  #smoothing function generator
  smooth.loess <- function(C.ZR) {
    C.ZR <- round(C.ZR,2)
    df <-  reshape2::melt(add_rownames(as.data.frame(C.ZR, row.names = rownames(C.ZR)))) %>% 
      mutate(R = as.numeric(rowname), Z = as.numeric(as.character(variable)))
    
    #add border cases
    df <- rbind(df, c(0,0,0), c(1,1, max(df$value)))
    loess.fit <-loess(value~R+Z, df, control = loess.control(surface = 'direct'), span = span, degree = 2) #tune
    return (loess.fit)
  }
  S.TF <- c()
  if(length(S.out) == 0) {
    S.TF <- rep(FALSE, n.brw)
    S.FN <- c()
  } else {
    #coalesce if multiple lists so length = n.brw
    v.s <- as.numeric(sapply(strsplit(names(S.out), "\\."), "[", 2)) #loan supply functions in list
    S.TF <- v.brw %in% v.s #for functions that don't match -> eliminate from S
    v.brw.s <- v.brw[S.TF]
    S.FN <- list()
    for(i in c(1:length(v.brw.s))) { #alternative option is to create separate model for each matrix
      S.FN[[i]] <- lapply(S.out[v.s %in% v.brw.s[i]],smooth.loess)  #reduce and create one model
      #S.FN[[i]] <- smooth.loess(Reduce('+', S.out[v.s %in% v.brw.s[i]]))
    }
    names(S.FN) <- v.brw.s
  }
  rm(S.out)
  
  #Iterate through S.mtx, Rc, & Zc, update wgt.Vec, rate.Vec, and scrt.Vec, optimize missing value (C)
  S.indx <- which(!S.TF)
  optim.C <- function(r, z) {
    P <- risk.Mtx
    W <- lent.Vec
    R <- rate.Vec
    Z <- scrt.Vec
    
    #set new values
    W[indx.s] <- NA
    R[indx.c] <- r
    Z[indx.c] <- z
    
    #eliminate rows with 0 amounts to be sold
    if(length(S.indx)>0) {
      P <- P[-indx.s[S.indx],]
      W <- W[-indx.s[S.indx]]
      R <- R[-indx.s[S.indx]]
      Z <- Z[-indx.s[S.indx]]
      indx.mtx <- -indx.s[S.indx]
    } else {
      indx.mtx <- c(1:nrow(corr.mtx))
    }
    
    #In below case we know W.in = Consumed = Amount purchased and kept
    if(sum(is.na(W))==1 & sum(is.na(R))==0 & sum(is.na(Z))==0){
      
    }
    #Else solve
    soln <- tryCatch({
      kumaraswamy$optim.Kumar(corr.mtx[indx.mtx, indx.mtx],
                              P.in = P[, P.indx], W.in = W, R.in = R, Z.in = Z, 
                              Wlim = lend.lim, Rlim = rLim, S.FN = S.FN, Clim = Consumed,
                              algorithm = algorithm, controls = controls, relax = relax, browse = browse)
    }, error = function(e) {
      browser()
      kumaraswamy$optim.Kumar(corr.mtx[indx.mtx, indx.mtx],
                              P.in = P[, P.indx], W.in = W, R.in = R, Z.in = Z, 
                              Wlim = lend.lim, Rlim = rLim, S.FN = S.FN, Clim = Consumed,
                              algorithm = algorithm, controls = controls, relax = relax, browse = TRUE)
      browser()
    })
    if(sum(is.na(W))==1 & sum(is.na(R))==0 & sum(is.na(Z))==0){
      return(soln)
    }
    soln$R <- r-soln$R
    soln$Z <- z-soln$Z
    return(soln)
  }
  
  #for debugging purposes
  rslt <- optim.C(Rate, Collateral)
  
  #Divy results by lend.DF and store
  # cnt <- 1
  # rslt <- list()
  # for(i in c(1:n.brw)) {
  #   C.ZR <- V[, c(seq(i,(length(Zc)-1)*n.brw, n.brw),(length(Zc)-1)*n.brw+i)]
  #   rownames(C.ZR) <- Rc
  #   colnames(C.ZR) <- Zc
  #   i.brw <- which(lend.df$Brw == v.brw[i])
  #   for(j in i.brw) {
  #     C.ZR.i <- C.ZR*lend.df$lend.pct[j]
  #     rslt[[cnt]] <- C.ZR.i
  #     names(rslt)[cnt] <- paste0(v, '_', lend.df$Orgn[j], '.', lend.df$Brw[j])
  #     cnt <- cnt+1
  #   }
  # }
  return (rslt)
  
}

loan.frwdProp <-  function(ntwk,root,S.list,Amt,Rate,Collateral,
                           algorithm ="NLOPT_GN_ISRES",zLim = 0.99, rLim = 2.5,
                           controls = list(maxeval=1000,
                                           xtol_rel=0.1,xtol_abs=c(0.5,0.01,0.01,0.001,0.001), span = 0.5),
                           browse = FALSE){
  
  #Convert Matrix to loess smoothin function
  smooth.loess <- function(C.ZR) {
    C.ZR <- round(C.ZR,2)
    df <-  reshape2::melt(add_rownames(as.data.frame(C.ZR, row.names = rownames(C.ZR)))) %>% 
      mutate(R = as.numeric(rowname), Z = as.numeric(as.character(variable)))
    
    #add border cases
    df <- rbind(df, c(0,0,0), c(1,1, max(df$value)))
    loess.fit <-loess(value~R+Z, df, control = loess.control(surface = 'direct'), span = 0.5, degree = 2) #tune
    return (loess.fit)
  }
  
  #figure out order of node traversal
  subntwk.EL <- as.numeric(do.call(rbind, strsplit(sapply(strsplit(names(S.list), "\\."), "[", 1),'_')),drop=FALSE)
  subntwk.EL <- as.data.frame(matrix(subntwk.EL,ncol=2))[,c(2,1)]
  colnames(subntwk.EL) <- c('from','to')
  
  frwd.order <- unique(rev(as.numeric(sapply(strsplit( names(S.list), "_"), "[", 1)))) #setdiff(rev(utils$postorder.DFS(subntwk.EL, root)),root)
  
  #If back.order is NULL then return
  if(length(frwd.order)==0){
    return(NULL)
  }
  
  #Create dataframe with frwd.order and associated amounts lent to v
  subnet.DF <- data.frame(subntwk.EL)
  subnet.DF$Amt.C  <- NA
  subnet.DF$Rate.C <- NA
  subnet.DF$Scrt.C <- NA
  subnet.DF$Amt.S  <- NA 
  subnet.DF$Rate.S <- NA 
  subnet.DF$Scrt.S <- NA 
  subnet.DF$brw <- root
  
  #first vertices to propagate to
  v.in <- paste0(subntwk.EL[subntwk.EL[, 'from']== root, 'to'], '_', root,'.',root)
  v.in.indx <- match(sapply(strsplit(v.in, "\\."), "[", 1),paste0(subnet.DF[,2],'_',subnet.DF[,1]))
  
  #Extract amounts for v.in
  amt.in <- c()
  for(i in c(1:length(v.in))){
    amt.in[i] <-  predict(smooth.loess(S.list[[v.in[i]]]), as.data.frame(list(R=Rate,Z=Collateral)))
  }
  #Normalize by amount borrowed
  subnet.DF[v.in.indx,3] <- Amt*amt.in/sum(amt.in)
  subnet.DF[v.in.indx,4] <- Rate
  subnet.DF[v.in.indx,5] <- Collateral
  
  #Iterate over frwd.order
  for(v in frwd.order){
    
    #create propogation mtx
    v.indx <- which(subnet.DF$to==v)
    prop.mtx <- as.matrix(subnet.DF[v.indx,c('brw','from','to','Amt.C','Rate.C','Scrt.C')]) #loan assets coming in
    
    if(any(is.na(prop.mtx))){
      #prune edges
      subnet.DF <- subnet.DF[-match(rownames(prop.mtx)[is.na(prop.mtx[,'Amt.C'])],rownames(subnet.DF)),]
      v.indx <- which(subnet.DF$to==v)
      prop.mtx <- as.matrix(subnet.DF[v.indx,c('brw','from','to','Amt.C','Rate.C','Scrt.C')]) #loan assets coming in
    }
    
    print(paste0('Frwd-Prop from {',paste(prop.mtx[,'from'],collapse=',') ,'} to ',v))
    
    #vertices to propagate to
    if(length(subntwk.EL[subntwk.EL[, 'from']== v, 'to'])>0){
      v.in <- paste0(subntwk.EL[subntwk.EL[, 'from']== v, 'to'], '_', v,'.',root)
      v.in.indx <- match(sapply(strsplit(v.in, "\\."), "[", 1),paste0(subnet.DF[,2],'_',subnet.DF[,1]))
    } else {
      v.in <- integer(0)
      v.in.indx <-  integer(0)
    }
    if(any(is.na(v.in.indx))){ #append missing row
      newrow <- as.numeric(do.call(rbind, strsplit(sapply(strsplit(v.in[is.na(v.in.indx)], "\\."), "[", 1),'_')),drop=FALSE)
      newrow <- as.data.frame(matrix(newrow,ncol=2))
      colnames(newrow) <- c('to','from')
      newrow$brw <- root
      subnet.DF <- plyr::rbind.fill(subnet.DF,newrow[,c('from','to','brw')])
      v.in.indx <- match(sapply(strsplit(v.in, "\\."), "[", 1),paste0(subnet.DF[,2],'_',subnet.DF[,1]))
    }
    
    #Optimize to determine Amount Sold, Sell Rate, and Sell Collateral
    rslt <- tryCatch({cnsm.ZR.frwdsolve(ntwk,v, prop.mtx, S.out = S.list[v.in],
                                        zLim = zLim, rLim = prop.mtx[,'Rate.C'],
                                        algorithm=algorithm, controls=controls, browse=browse)
    }, error = function(e) {
      browser()
      cnsm.ZR.frwdsolve(ntwk,v, prop.mtx, S.out = S.list[v.in],
                        zLim = zLim, rLim = prop.mtx[,'Rate.C'],
                        algorithm=algorithm, controls=controls, browse=TRUE)
      browser()
    })
    
    #save updated portfolio distribution
    ntwk[['val']][[v]]$Ptfl.kumPDF.b <- rslt$K[1]
    ntwk[['val']][[v]]$Ptfl.kumPDF.a <- rslt$K[2]
    
    if(length(v.in)>0){
      #Scale rslt$S
      scale.fctr <- sum(prop.mtx[,'Amt.C'])/(rslt$S+rslt$W)
      rslt$S <- rslt$S*scale.fctr
      rslt$W <- rslt$W*scale.fctr
      
      subnet.DF[v.indx,'Amt.S'] <- subnet.DF[v.indx,'Amt.C']/sum(subnet.DF[v.indx,'Amt.C'])*rslt$S #distribute
      subnet.DF[v.indx,'Rate.S'] <- rslt$R
      subnet.DF[v.indx,'Scrt.S'] <- rslt$Z
      
      #Extract consumptions amounts for v.in -> scale amount sold
      amt.in <- c()
      for(i in c(1:length(v.in))){
        amt.in[i] <-  predict(smooth.loess(S.list[[v.in[i]]]), as.data.frame(list(R=rslt$R,Z=rslt$Z)))
      }
      
      #Normalize by amount borrowed
      subnet.DF[v.in.indx,'Amt.C'] <- rslt$S*amt.in/sum(amt.in)
      subnet.DF[v.in.indx,'Rate.C'] <- rslt$R
      subnet.DF[v.in.indx,'Scrt.C'] <- rslt$Z
      
      #if((rslt$S+rslt$W)>sum(prop.mtx[,'Amt.C'])){browser()}
    } else {
      subnet.DF[v.indx,'Amt.S'] <- 0
    }
    
    #create portfolio if not there
    if(!is.data.frame(ntwk[['val']][[v]]$Portfolio)){
      ntwk[['val']][[v]]$Portfolio <- data.frame(to=numeric(),Risk.coef=numeric(),tot.trust=numeric(),
                                                 lent=numeric(),rate=numeric(),security=numeric())
    }
    if(!is.data.frame(ntwk[['val']][[v]]$Assets)){
      ntwk[['val']][[v]]$Assets <- data.frame(borrower=numeric(),via=numeric(),via.trust=numeric(),risk.coef=numeric(),
                                              amount=numeric(),rate=numeric(),security=numeric(),type=character())
    }
    if(!is.data.frame(ntwk[['val']][[v]]$Liabilities)){
      ntwk[['val']][[v]]$Liabilities <- data.frame(borrower=numeric(),via=numeric(),lender=numeric(),
                                                   amount=numeric(),rate=numeric(),security=numeric(),type=character())
    }
    
    #write results
    for(i in c(1:length(v.indx))){
      v.from <- subnet.DF[v.indx[i],'from']
      v.to <- subnet.DF[v.indx[i],'to']
      
      #Update Liabilities - liability from loan propagated downstream
      newrow <- list('borrower'=root,
                     'via'=v.from,
                     'lender'=v.to,
                     'amount'=subnet.DF[v.indx[i],'Amt.C'],
                     'rate'=subnet.DF[v.indx[i],'Rate.C'],
                     'security'=subnet.DF[v.indx[i],'Scrt.C'],
                     'type'=ifelse(v.from==root,'loan-origin','loan-prop'))
      newrow <- lapply(newrow,function(x) ifelse(length(x)==0,NA,x))
      ntwk[['val']][[v.from]]$Liabilities <- tryCatch({rbind(ntwk[['val']][[v.from]]$Liabilities, newrow)}, 
                                                      error = function(e) {browser()})
      
      #Update Assets - bond asset received from borrower
      newrow <- list('borrower'=root,
                     'via'=v.from,
                     'via.trust'=ntwk$mel[[unlist(network::get.dyads.eids(ntwk,v.from,v.to))]]$atl$Trust,
                     'risk.coef'=ntwk[['val']][[v.to]]$Subj.risk[ntwk[['val']][[
                       v.to]]$Subj.risk$to==v.from,'Risk.coef'],
                     'amount'=subnet.DF[v.indx[i],'Amt.C']-subnet.DF[v.indx[i],'Amt.S'],
                     'rate'=subnet.DF[v.indx[i],'Rate.C'],
                     'security'=subnet.DF[v.indx[i],'Scrt.C'],
                     'type'='Bond')
      newrow <- lapply(newrow,function(x) ifelse(length(x)==0,NA,x))
      ntwk[['val']][[v.to]]$Assets <-  tryCatch({rbind(ntwk[['val']][[v.to]]$Assets,newrow)}, 
                                                error = function(e) {browser()})
      
      #Update Assets - hedge asset sold downstream
      newrow <- list('borrower'=root,
                     'via'=v.from,
                     'via.trust'=ntwk$mel[[unlist(network::get.dyads.eids(ntwk,v.from,v.to))]]$atl$Trust,
                     'risk.coef'=ntwk[['val']][[v.to]]$Subj.risk[ntwk[['val']][[
                       v.to]]$Subj.risk$to==v.from,'Risk.coef'],
                     'amount'=subnet.DF[v.indx[i],'Amt.S'],
                     'rate'= with(subnet.DF[v.indx[i],],Rate.C-Rate.S),
                     'security'=with(subnet.DF[v.indx[i],],Scrt.C-Scrt.S),
                     'type'='contra-hedge')
      newrow <- lapply(newrow,function(x) ifelse(length(x)==0,NA,x))
      ntwk[['val']][[v.to]]$Assets <- tryCatch({rbind(ntwk[['val']][[v.to]]$Assets,newrow)}, 
                                               error = function(e) {browser()})
      
      #Update Liabilities - negative liability from pass-through of loan propagated downstream
      newrow <- list('borrower'=root,
                     'via'=v.from,
                     'lender'=v.to,
                     'amount'=-subnet.DF[v.indx[i],'Amt.S'],
                     'rate'= subnet.DF[v.indx[i],'Rate.C'],
                     'security'= subnet.DF[v.indx[i],'Scrt.C'],
                     'type'='pass-thru')
      newrow <- lapply(newrow,function(x) ifelse(length(x)==0,NA,x))
      ntwk[['val']][[v.to]]$Liabilities <- tryCatch({rbind(ntwk[['val']][[v.to]]$Liabilities,newrow)}, 
                                                    error = function(e) {browser()})
    }
    
    #Summarize and add to Portfolio
    sumDF.bond <-  subnet.DF[v.indx,] %>% 
      summarise(lent=sum(Amt.C-Amt.S),
                rate=weighted.mean(Rate.C,Amt.C),
                security=weighted.mean(Scrt.C,Amt.C)) %>% 
      mutate(tot.trust=NA,
             Risk.coef=ntwk[['val']][[v]]$Subj.risk[ntwk[['val']][[v]]$Subj.risk$to==root,'Risk.coef'],
             to=root) %>% select(to,Risk.coef,tot.trust,lent,rate,security) %>%
      filter(!is.nan(rate))
    
    sumDF.hdge <-  subnet.DF[v.indx,] %>% 
      summarise(lent=sum(Amt.S),
                rate=weighted.mean(Rate.C,Amt.C)-weighted.mean(Rate.S,Amt.S),
                security=weighted.mean(Scrt.C,Amt.C)-weighted.mean(Scrt.S,Amt.S))  %>% 
      mutate(tot.trust=NA,
             Risk.coef=ntwk[['val']][[v]]$Subj.risk[ntwk[['val']][[v]]$Subj.risk$to==root,'Risk.coef'],
             to=root) %>% select(to,Risk.coef,tot.trust,lent,rate,security) %>%
      filter(!is.nan(rate))
    
    #append
    ntwk[['val']][[v]]$Portfolio <- rbind(ntwk[['val']][[v]]$Portfolio,sumDF.bond)
    ntwk[['val']][[v]]$Portfolio <- rbind(ntwk[['val']][[v]]$Portfolio,sumDF.hdge)
    
    #Update correlation matrix
    corr.mtx <- tryCatch({correlation$correlationUpdate(ntwk, v, direction = 'in')},error=function(e){browser()})
    sort.indx <- sort(ntwk[['val']][[v]]$Portfolio$to,index.return=TRUE)$ix
    sort.indx <- sort(sort.indx,index.return=TRUE)$ix
    corr.mtx <- corr.mtx[sort.indx,sort.indx]
    
    #Check which rows are "contra-hedges' (z<0)
    indx.hedge <- which(ntwk[['val']][[v]]$Portfolio$security<0)
    corr.mtx[indx.hedge,] <-  corr.mtx[indx.hedge,]*-1
    corr.mtx[,indx.hedge] <-  corr.mtx[,indx.hedge]*-1
    diag(corr.mtx) <- 1
    
    #Update
    ntwk[['val']][[v]]$Portfolio.corr <- corr.mtx
    ntwk$val[[v]]$Assets <- ntwk$val[[v]]$Assets %>% filter(amount!=0)
    ntwk$val[[v]]$Liabilities <- ntwk$val[[v]]$Liabilities %>% filter(amount!=0)
  }
  
  #Update edges
  edgeIDs <- unlist(network::get.dyads.eids(ntwk,v.from,v.to))
  for(E in c(1:length(edgeIDs))){
    ntwk$mel[[edgeIDs[E]]]$atl$Utilized <- ifelse(is.na(ntwk$mel[[edgeIDs[E]]]$atl$Utilized),0,
                                                  ntwk$mel[[edgeIDs[E]]]$atl$Utilized)+subnet.DF$Amt.C[E]
  }
  
  return(list('transactions'=subnet.DF,'ntwk'=ntwk))
}

# for(v in frwd.order){
#   
#   #Remove all NA's and 0s
#   #ntwk[['val']][[v]]$Liabilities <- ntwk[['val']][[v]]$Liabilities %>% filter(amount>0)
#   #ntwk[['val']][[v]]$Assets <- ntwk[['val']][[v]]$Assets %>% filter(amount>0)
#   
#   #go thru and aggregate assets and out in portfolio
#   #v.from <- which(subnet.DF$from==v)
#   #  v.to <- which(subnet.DF$to==v)
#     
#   #aggregate if needed
#   sumDF <- ntwk[['val']][[v]]$Assets %>% filter(!(borrower %in% ntwk[['val']][[v]]$Portfolio$to)) %>%
#           group_by(borrower,via,via.trust,risk.coef,type) %>% 
#           summarise(amount=sum(amount),
#                     rate=weighted.mean(rate,amount),
#                     security=weighted.mean(security,amount)) %>% 
#           ungroup() %>% select(borrower,risk.coef,via.trust,amount,rate,security)
#   
#   if(!is.data.frame(ntwk[['val']][[v]]$Portfolio)){
#     ntwk[['val']][[v]]$Portfolio <- data.frame(to=numeric(),Risk.coef=numeric(),tot.trust=numeric(),
#                                              lent=numeric(),rate=numeric(),security=numeric())
#   }
#   colnames(sumDF) <- colnames(ntwk[['val']][[v]]$Portfolio)
#   #Update Portfolio - Add bond and contra-hedge
#   #indx.assets <- c((nrow(ntwk[['val']][[v.to]]$Assets)-1):(nrow(ntwk[['val']][[v.to]]$Assets)))
#   #indx <- ifelse(is.data.frame(ntwk[['val']][[v]]$Portfolio),nrow(ntwk[['val']][[v]]$Portfolio),0)+1
#   #indx.ptfl <- c(indx:(indx-1+nrow(sumDF)))
#   tryCatch({ntwk[['val']][[v]]$Portfolio <- rbind(ntwk[['val']][[v]]$Portfolio,sumDF)},error=function(e){browser()})
#   
#   #ntwk[['val']][[v.to]]$Portfolio[indx.ptfl,'tot.trust'] <- NA
#   
#   #Update correlation matrix
#   corr.mtx <- tryCatch({correlation$correlationUpdate(ntwk, v, direction = 'in')},error=function(e){browser()})
#   sort.indx <- sort(ntwk[['val']][[v]]$Assets$borrower,index.return=TRUE)$ix
#   sort.indx <- sort(sort.indx,index.return=TRUE)$ix
#   corr.mtx <- corr.mtx[sort.indx,sort.indx]
#   
#   #Check which rows are "contra-hedges' (z<0)
#   indx.hedge <- which(ntwk[['val']][[v]]$Assets$type=='contra-hedge')
#   corr.mtx[indx.hedge,] <-  corr.mtx[indx.hedge,]*-1
#   corr.mtx[,indx.hedge] <-  corr.mtx[,indx.hedge]*-1
#   diag(corr.mtx) <- 1
#   
#   #Update
#   ntwk[['val']][[v]]$Portfolio.corr <- corr.mtx
# }