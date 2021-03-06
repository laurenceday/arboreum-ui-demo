import(network)
import(dplyr)
import(stats)
import(Hmisc)
import(doParallel)
import(bigmemory)
import(foreach)
import(predictionet)

utils    <- modules::use(here::here("src/Utils.R"))

#' Traverse network and calculate how trust cascades
#' see Sun, Zhu, Han (2006) Information Theoretic Framework for Trust Modeling for equations to update trust
#'
#' @param ntwk : network object of class 'network'
#' @param ntwk.attr : which network attribute to cascade
#' @param direction : trust cascades to outgoing vertices or incoming vertices?
#' @param max.depth : maximum number of arcs before stopping depth-first search
#'
#' @return adjacency matrix with i, j pairs indicating value cascade from i to j
#' @export
#'
#' @examples
calcRiskArray <- function(ntwk, nodes=c(), ntwk.attr = 'Risk', direction = 'in', max.depth = 10, converge = FALSE,runParallel=TRUE) {
  #' Recursively traverse network and calculate how trust cascades indirectly from origin to other nodes
  #' see Sun, Zhu, YHn (2006) Information Theoretic Framework for Trust Modeling for equations to update trust
  #'
  #' @param ntwk : network object of class 'network'
  #' @param v.orgn : vertex to cascade from
  #' @param edgeMtx : edge relationships matrix with following columns |from|to|Trust|Trust.coef|
  #' @param direction : trust cascades to outgoing vertices or incoming vertices?
  #' @param max.depth : maximum number of arcs before stopping depth-first search
  #' @param crnt.depth : number of arcs currently traversed (do not alter)
  #' @param v.visited : number of vertices currently visited (do not alter)
  #' @param converge : in the advent that trust vertex extends to another vertex and retrieved
  #' value of said vertex neighbours is different, should they be converged or excpetion made for original (logical)
  #'
  #' @return matrix with columns |Dest|Trust.dest|
  #' @export
  #'
  #' @examples
  traverse.CalcTrust <- function(ntwk, v.orgn, edgeMtx, direction = 'in', max.depth = 10, crnt.depth = 0, v.visited = c(), converge = FALSE) {
    
    #choose which direction to propogate trust
    if(direction == 'out') {
      orgn.col <- 'from'
      dest.col <- 'to'
    } else {
      orgn.col <- 'to'
      dest.col <- 'from'
    }
    
    #find all nieghboring vertices in propogation direction
    indx.trusted <- which(edgeMtx[, orgn.col]== v.orgn)
    v.trusted <- edgeMtx[indx.trusted, dest.col]
    crnt.depth <- crnt.depth+1
    
    #remove indices of all vertices previously visited
    i.novisit <- match(setdiff(v.trusted, v.visited), v.trusted)
    indx.trusted <- indx.trusted[i.novisit]
    v.visited <- c(v.visited, v.orgn)
    
    #if no more vertices then return
    if(length(indx.trusted) == 0 | crnt.depth>max.depth) {
      prpg.trust.Mtx.v <- matrix(c(0,1), nrow = 1, ncol = 2)
      colnames(prpg.trust.Mtx.v) <- c('Dest', 'Trust.dest')
      return (as.data.frame(prpg.trust.Mtx.v))
    } else {
      
      #retrieve vertices and trust amounts
      vrtx.trusted  <- edgeMtx[indx.trusted, dest.col]
      amt.trust.Mtx <- edgeMtx[indx.trusted, c(dest.col, 'Trust.coef'), drop = FALSE]
      amt.trust.Mtx <- as.data.frame(amt.trust.Mtx)
      colnames(amt.trust.Mtx) <- c('Dest', 'Trust.dest')
      amt.trust.Mtx$Orgn <- v.orgn
      amt.trust.Mtx$Trust.orgn <-  0.99999
      
      #recurse - do same for all children nodes
      prpg.trust.Mtx.List <- list()
      for(i in c(1:length(indx.trusted))) {
        prpg.trust.Mtx.v <- traverse.CalcTrust(ntwk, vrtx.trusted[i], edgeMtx,
                                               direction = direction, max.depth = max.depth, crnt.depth = crnt.depth,
                                               v.visited = v.visited, converge = converge)
        #append to list if dest!= 0 (dead end)
        if(prpg.trust.Mtx.v[1]>0) {
          prpg.trust.Mtx.v$Orgn <- vrtx.trusted[i]
          prpg.trust.Mtx.v$Trust.orgn <- amt.trust.Mtx[i, 'Trust.dest']
          
          #store in list
          prpg.trust.Mtx.List[[length(prpg.trust.Mtx.List)+1]] <- prpg.trust.Mtx.v
        }
      }
      
      #memory efficient rbindlist
      if(length(prpg.trust.Mtx.List)>0) {
        prpg.trust.Mtx.List[[length(prpg.trust.Mtx.List)+1]] <- amt.trust.Mtx
        prpg.trust.Mtx <- data.table::rbindlist(prpg.trust.Mtx.List)
      } else { #return retrieved vertices and trust amounts
        return (amt.trust.Mtx[, c('Dest', 'Trust.dest')])
      }
      rm(prpg.trust.Mtx.List, prpg.trust.Mtx.v)
      
      #KEY ASSUMPTION: trust is propogated as bernoulli probabilities rather than its entropy representation
      #if node appears more than once in prpg.trust.Mtx->must fuse data
      if(length(unique(prpg.trust.Mtx[[1]]))<nrow(prpg.trust.Mtx)) {
        prpg.trust.Mtx <- prpg.trust.Mtx %>%
          mutate(Trust.prop = Trust.dest*Trust.orgn+(1-Trust.dest)*(1-Trust.orgn)) %>%
          group_by(Dest) %>%
          summarise(Trust.prod = prod(Trust.prop/(1-Trust.prop)),
                    Trust.dest = Trust.prod/(1+Trust.prod))
      } else {
        
        #directly calculate propogated trust (multiply both probability vectors together)
        prpg.trust.Mtx$Trust.dest <- with(prpg.trust.Mtx, Trust.dest*Trust.orgn+(1-Trust.dest)*(1-Trust.orgn))
      }
      
      #rbind original vertices
      if(converge) { #return converged values
        return (prpg.trust.Mtx[, c('Dest', 'Trust.dest')])
      } else { #make exception for original trust values
        prpg.trust.Mtx <-prpg.trust.Mtx[prpg.trust.Mtx$Dest %in% setdiff(prpg.trust.Mtx$Dest, amt.trust.Mtx$Dest),
                                        c('Dest', 'Trust.dest')]
        amt.trust.Mtx[c((nrow(amt.trust.Mtx)+1):(nrow(amt.trust.Mtx)+nrow(prpg.trust.Mtx))),] <- prpg.trust.Mtx[, c(1,2)]
        if(length(unique(amt.trust.Mtx[[1]]))<nrow(amt.trust.Mtx)) {browser()}
        return (amt.trust.Mtx[, c('Dest', 'Trust.dest')])
      }
    }
  }
  
  
  #vertices
  n.vrt <- ntwk %n% "n"
  ntwk.i <- utils$ntwk2igraph.cvrt(ntwk)
  print('Here')
  
  #Indirect Trust between all members
  edges.Mtx <- stats::setNames(as.data.frame(network::as.edgelist(ntwk, attrname = c(ntwk.attr), as.sna.edgelist = TRUE)),
                               c('from', 'to', ntwk.attr)) %>%
    dplyr::left_join(stats::setNames(as.data.frame(network::as.edgelist(ntwk, attrname = c(paste0(ntwk.attr, '.coef')), as.sna.edgelist = TRUE)),
                                     c('from', 'to', paste0(ntwk.attr, '.coef')))) %>%
    dplyr::left_join(igraph::as_long_data_frame(ntwk.i) %>% select(from, to, from_deg, to_deg)) %>%
    arrange(from, to)
  edges.Mtx <- base::as.matrix(edges.Mtx[complete.cases(edges.Mtx),])
  colnames(edges.Mtx)[colnames(edges.Mtx) == ntwk.attr] <- 'Trust'
  colnames(edges.Mtx)[colnames(edges.Mtx) == paste0(ntwk.attr, '.coef')] <- 'Trust.coef'
  edges.Mtx[edges.Mtx[, 'Trust.coef']>0.995 , 'Trust.coef'] <- 0.995
  edges.Mtx[edges.Mtx[, 'Trust.coef']<0.505 , 'Trust.coef'] <- 0.505
  
  #generate nodes if not passed in as arg
  if(length(nodes)==0){nodes <- c(1:n.vrt)}
  
  #Remove any cycles just in case
  edges2delete <- mapply(function(x) { ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),
                                              setdiff(ntwk$val[[x]]$Portfolio$to,get.neighborhood(ntwk,x,'in')),
                                              NA)},x=c(1:n.vrt))
  
  #check if directed acyclic graph and remove corresponding edges
  x.dag <- predictionet::adj.remove.cycles(network::as.matrix.network(ntwk,matrix.type='adjacency'),
                                           maxlength = max.depth)
  x.dag <- which(x.dag$adjmat.removed, arr.ind = TRUE)
  colnames(x.dag) <- c('from', 'to')
  x.dag <- x.dag[x.dag[,'to'] %in% setdiff(x.dag[,'to'],nodes),] #direction='in'
  if(nrow(x.dag)>0 & length(nodes)<n.vrt){
    indx <- match(paste(x.dag[,'from'],x.dag[,'to'],sep='.'),
                  paste(edges.Mtx[,'from'],edges.Mtx[,'to'],sep='.')) #for direction='in'
    edges.Mtx <- edges.Mtx[-indx[!is.na(indx)],]
  }
  
  #build container
  risk.mtx  <- matrix(nrow = n.vrt, ncol = n.vrt)
  
  #calculate how trust cascades indirectly
  if(runParallel){
    print('Parallel pass')
    #parallelize
    no_cores <- parallel::detectCores() - 2
    registerDoParallel(cores = no_cores)
    ostype <- if (Sys.info()[['sysname']] == 'Windows') {"PSOCK"} else {"FORK"}
    clst <- parallel::makeCluster(no_cores, type = ostype)
    on.exit(parallel::stopCluster(clst))
    print('Ding')
    #data storage object
    V <- big.matrix(n.vrt,n.vrt)
    desc <- describe(V)
    #parallel for loop
    print('Ding2')
    result = foreach(i = nodes, .packages="dplyr")%dopar%
      {
        print(paste(i))
        V <- bigmemory::attach.big.matrix(desc)
        z <- traverse.CalcTrust(ntwk, i, edges.Mtx, direction = direction, converge = converge)
        z <- z[complete.cases(z),]
        if(all(z[,1]>0)) {
          V[i, z[[1]]] <- z[[2]] #store row-wise
        }
        print(paste('v', i))
      }
    risk.mtx <- as.matrix(V)
  } else {
    print('Non-parallel pass')
    for(i in nodes) {
      z <- traverse.CalcTrust(ntwk, i, edges.Mtx, direction = direction, converge = converge)
      z <- z[complete.cases(z),]
      if(all(z[,1]>0)) {
        tryCatch({
          risk.mtx[i, z[[1]]] <- z[[2]] #store row-wise
        },error=function(e){browser()})
      }
      print(paste('v', i))
    }
  }
  
  #calculate beta distribution representation of risk
  dist.mtx <- igraph::distances(ntwk.i, mode = direction)
  risk.array <- array(dim = c(nrow(risk.mtx), ncol(risk.mtx),4))
  risk.array[,,1] <- risk.mtx
  for(v in which(!is.na(risk.mtx))) { #iterates over all non-NA cells
    i <- v%%n.vrt #row of cell (node doing assessing)
    i <- ifelse(i==0,n.vrt,i) #if modulus is 0 then last row
    j <- ceiling(v/n.vrt) #col of cell (node being assessed)
    x <- risk.mtx[!is.na(risk.mtx[, j]), j] #select column representing judgements of risk by other nodes
    wgt.j <- dist.mtx[i,!is.na(risk.mtx[, j])]+1
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
  return (list('risk.array'=risk.array, 'ntwk'=ntwk))
}