import(dplyr)
import(predictionet)
import(MCMCpack)
import(network)
import(stats)

correlation <- modules::use(here::here("ShinyApps/Arboreum/app/src/PortfolioCorrelation.R"))
trustRisk <- modules::use(here::here("ShinyApps/Arboreum/app/src/TrustToRisk.R"))
traverse <- modules::use(here::here("ShinyApps/Arboreum/app/src/Traverse.R"))
utils    <- modules::use(here::here("ShinyApps/Arboreum/app/src/Utils.R"))

#Add node
addNode2Ntwk <- function(ntwk,risk.array,assets,out.DF,in.DF=NULL,direction='in'){
  
  #number of vertices
  n.vrt <- ntwk %n% "n"
  
  #if in.DF is null generate incoming nodes & trust
  if(is.null(in.DF)){
    
    #distribute trust according to total assets
    assets.v <- abs(get.vertex.attribute(ntwk,'Equity')+get.vertex.attribute(ntwk,'PtflAtRisk'))
    #loop through and calculate probability of attachment and amount "entrusted"
    amt.trust <- c()
    pr.attach <- c()
    for(v in c(1:n.vrt)){
      if(!is.na(ntwk[['val']][[v]]$Portfolio)){
        a <- (ntwk[['val']][[v]]$Portfolio %>% 
                dplyr::filter(to %in% get.neighborhood(ntwk,v,direction)) %>% 
                select(tot.trust) %>% unlist())
        b <- assets.v[get.neighborhood(ntwk,v,direction)]
        mdl <- lm(log(a)~log(b),data=data.frame(list(a,b)))
        amt.trust[v] <- exp(predict(mdl,data.frame(list(b=assets))))
        pr.attach[v] <- gtools::inv.logit(amt.trust[v])-0.5
      }
    }
    
    #choose average for pr.attach is NA
    amt.trust[is.na(amt.trust)] <- runif(sum(is.na(amt.trust)),max=assets.v[is.na(amt.trust)])
    pr.attach[is.na(pr.attach)] <- mean(pr.attach,na.rm = TRUE)/10
    
    #randomly generate edges
    eta <- MCMCpack::rdirichlet(1,pr.attach)
    nodes.in <- which(rbinom(length(eta),size=1,prob=eta/max(eta))==1)
    trust.in <- amt.trust[nodes.in]
    
    #dataframe
    in.DF <- data.frame(list('nodes'=nodes.in,
                             'trust'=trust.in))
    rm(eta,nodes.in,trust.in,a,b,amt.trust,pr.attach,mdl)
  }
  
  #add new vertex
  add.vertices(ntwk,1,vattr=list(c(list('Equity'=assets*0.05,
                                        'vertex.names'=paste0('X',n.vrt+1),
                                        'deg'=nrow(in.DF)+nrow(out.DF)),
                                   setNames(rep(list(NA),length(names(ntwk$val[[n.vrt]]))-4),
                                            setdiff(names(ntwk$val[[n.vrt]]),
                                                    c('Equity','vertex.names','na','deg')))
  )))
  
  #add edges from incoming trust nodes (presented as outgoing bonds)
  network::add.edges(ntwk, head=as.list(in.DF$nodes), tail=as.list(rep(n.vrt+1,nrow(in.DF))),
                     names.eval=rep(list(setdiff(names(ntwk$mel[[1]]$atl),'na')),nrow(in.DF)),
                     vals.eval=lapply(as.list(in.DF$trust),function(x) c(x,rep(list(NA),4))))
  
  #add edges to outgoing trust nodes (presented as incoming bonds)
  add.edges(ntwk, tail=out.DF$nodes, head=rep(n.vrt+1,nrow(out.DF)),
            names.eval=rep(list(setdiff(names(ntwk$mel[[1]]$atl),'na')),nrow(out.DF)),
            vals.eval=lapply(as.list(out.DF$trust),function(x) c(x,rep(list(NA),4))))
  
  #check if directed acyclic graph
  x.dag <- predictionet::adj.remove.cycles(network::as.matrix.network(ntwk,matrix.type='adjacency'),
                                           maxlength = 10)
  x.dag <- which(x.dag$adjmat.removed, arr.ind = TRUE)
  colnames(x.dag) <- c('from', 'to')
  x.dag <- x.dag[x.dag[,'from']==(n.vrt+1),,drop=FALSE] #change to "to" if direction is out
  if(nrow(x.dag)>0){
    in.DF <- in.DF[!(in.DF$nodes %in% x.dag[,'to']),]
    edges2delete <- unlist(get.dyads.eids(ntwk,x.dag[,'from'],x.dag[,'to']))
    delete.edges(ntwk,edges2delete)
  }
  
  #calculate correlation matrix
  corr.mtx <- correlation$correlationUpdate(ntwk, n.vrt+1, direction = direction)
  ntwk[['val']][[n.vrt+1]]$Portfolio.corr <- corr.mtx
  for(v in in.DF$nodes){
    corr.mtx <- correlation$correlationUpdate(ntwk, v, v.new=n.vrt+1, direction=direction)
    ntwk[['val']][[v]]$Portfolio.corr <- corr.mtx
  }
  
  #convert trust to risk (binomial probability)
  assets.v[n.vrt+1] <- assets
  equity.v <- get.vertex.attribute(ntwk,'Equity')
  ptflAtRisk.v <- get.vertex.attribute(ntwk,'PtflAtRisk')
  equity.v[n.vrt+1] <- assets*0.05 #hardcoded in for now
  ptflAtRisk.v[n.vrt+1] <- assets*0.95
  for(v in c(n.vrt+1,in.DF$nodes)){
    
    #corresponding edge IDs
    #edges.out <- sapply(network::get.neighborhood(ntwk, v, direction),
    #                    function (x)  network::get.dyads.eids(ntwk, v, x, neighborhood = direction)[[1]])
    #corresponding network attribute
    #trust.out <- network::get.edge.attribute(ntwk,'Trust')[edges.out]
    #if(any(is.na(trust.out))){browser()}
    
    rslt <- trustRisk$trust2Risk.solve(ntwk, v, assets.v[v],equity.v[v], direction = direction)
    df <- rslt[[1]]
    L  <- rslt[[2]] #risk aversion coefficient
    
    #set risk attributes
    if(is.na(L) ) {
      browser()
    }
    
    #set risk attributes
    set.vertex.attribute(ntwk, 'Risk.aversion', L[1,1], v = v)
    if(!any(is.na(df)) & nrow(df)>= 1) {
      set.edge.attribute(ntwk, 'Risk.coef', df$Risk.coef, df$Edge.To)
      set.edge.attribute(ntwk, 'Risk',   ptflAtRisk.v[n.vrt+1]*df$Risk.coef, df$Edge.To)
      
    }
  }
  
  #retrieve nodes to update
  nodes2update <- sort(unique(c(utils$rtrv.lcl.Edgelist(ntwk,n.vrt+1))))  
  
  #Update risk array
  rslt <- tryCatch({traverse$calcRiskArray(ntwk,nodes=nodes2update,runParallel=TRUE)}, 
                   error = function(e) {
                     browser()
                     traverse$calcRiskArray(ntwk,nodes=nodes2update,runParallel=FALSE)
                   })
  
  #update risk.mtx
  risk.mtx <- risk.array[,,1]
  risk.mtx <- rbind(cbind(risk.mtx,rep(NA,nrow(risk.mtx))),rep(NA,ncol(risk.mtx)+1)) #append column and row
  indx <- which(!is.na(rslt$risk.array[,,1]),arr.ind=TRUE)
  risk.mtx.new <- rslt$risk.array[,,1]
  risk.mtx[indx] <- risk.mtx.new[indx]
  rm(risk.mtx.new)
  
  #calculate beta distribution representation of risk
  dist.mtx <- igraph::distances(utils$ntwk2igraph.cvrt(ntwk), mode = direction)
  risk.array <- array(dim = c(nrow(risk.mtx), ncol(risk.mtx),4))
  risk.array[,,1] <- risk.mtx
  n.vrt <- n.vrt+1
  for(v in which(!is.na(risk.mtx))) { #iterates over all non-NA cells
    i <- v%%n.vrt #row of cell (node doing assessing)
    i <- ifelse(i==0,n.vrt,i) #if modulus is 0 then last row
    j <- ceiling(v/n.vrt) #col of cell (node being assessed)
    x <- risk.mtx[!is.na(risk.mtx[, j]), j] #select column representing judgements of risk by other nodes
    wgt.j <- tryCatch({dist.mtx[i,!is.na(risk.mtx[, j])]+1}, error = function(e) {browser()})
    wgt.j[!is.finite(wgt.j)] <- max(dist.mtx[is.finite(dist.mtx)])+2
    muHat <- weighted.mean(x,1/wgt.j)
    varHat <- Hmisc::wtd.var(x,1/wgt.j)
    
    risk.array[i, j,2] <- muHat^2*((1-muHat)/varHat-1/muHat)
    risk.array[i, j,3] <- risk.array[i, j,2]*(1/muHat-1)
    risk.array[i, j,4] <- risk.mtx[v]-muHat
  }
  
  #append to network
  set.vertex.attribute(ntwk, 'Subj.risk', NA)
  for(v in c(1:n.vrt)) {
    mtx <- risk.array[v,,]
    indx <- which(!is.na(mtx[,1]))
    mtx <- mtx[indx,, drop = FALSE]
    mtx <- cbind(indx, mtx)
    colnames(mtx) <- c('to', 'Risk.coef', 'alpha.Beta', 'beta.Beta', 'offest.Beta')
    ntwk[['val']][[v]]$Subj.risk <- as.data.frame(mtx)
  }
  
  #Add assets, liabilities, portfolio dataframe if not present
  if(!is.data.frame(ntwk[['val']][[n.vrt]]$Portfolio)){
    ntwk[['val']][[n.vrt]]$Portfolio <- data.frame(to=numeric(), tot.trust=numeric(), lent=numeric(), rate=numeric(),security=numeric())
  }
  if(!is.data.frame(ntwk[['val']][[n.vrt]]$Assets)){
    ntwk[['val']][[n.vrt]]$Assets <- data.frame(borrower = numeric(), via = numeric(), via.trust = numeric(),
                                            risk.coef=numeric(), amount = numeric(), rate = numeric(),
                                            security = numeric(), type=character())}
  
  if(!is.data.frame(ntwk[['val']][[n.vrt]]$Liabilities)){
    ntwk[['val']][[n.vrt]]$Liabilities <- data.frame(borrower = numeric(), via = numeric(), lender = numeric(),
                                                 amount = numeric(), rate = numeric(),security = numeric(),
                                                 type=character())
  }
  if(is.na(ntwk$val[[n.vrt]]$PtflAtRisk)){ntwk$val[[n.vrt]]$PtflAtRisk <- 4/5 *assets}
  
  return (list('risk.array'=risk.array, 'ntwk'=ntwk))
}