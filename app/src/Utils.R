import(intergraph)
import(dplyr)

#convert network class graph to igraph class graph
ntwk2igraph.cvrt <- function(ntwk) {
  for(a in names(which(sapply(ntwk[["val"]][[1]], function (x)  {length (x) >1})))) {
    network::set.vertex.attribute(ntwk, a, NA)
  }
  
  #add missing properties if differences exist
  vrt.prop <- sapply(ntwk$val,function(x) names(x))
  edg.prop <- sapply(ntwk$mel,function(x) names(x$atl))
  v.Props <- unique(c(vrt.prop))
  e.Props <- unique(c(edg.prop))
  vrt.prop.diff <- lapply(vrt.prop, function(x) setdiff(vrt.prop,v.Props))
  edg.prop.diff <- lapply(vrt.prop, function(x) setdiff(vrt.prop,v.Props))
  rm(vrt.prop,edg.prop,v.Props,e.Props)
  
  for(v in which(sapply(vrt.prop.diff,length)>0)){
    for(prop in vrt.prop.diff[[v]]){
      ntwk$val[[v]][[prop]] <- NA
    }
  }
  for(i in which(sapply(edg.prop.diff,length)>0)){
    for(prop in edg.prop.diff[[i]]){
      ntwk$val[[e]]$atl[[prop]] <- NA
    }
  }
  
  ntwk.i <- intergraph::asIgraph(ntwk, amap = rbind(rbind(intergraph::attrmap(),
                                                          data.frame(type ="vertex",
                                                                     fromcls ="network",
                                                                     fromattr ="Portfolio",
                                                                     tocls ="igraph",
                                                                     toattr = NA,
                                                                     stringsAsFactors = FALSE)),
                                                    data.frame(type ="vertex",
                                                               fromcls ="network",
                                                               fromattr ="Portfolio.corr",
                                                               tocls ="igraph",
                                                               toattr = NA,
                                                               stringsAsFactors = FALSE)))
  return (ntwk.i)
}

#fetch subgraph of v which is its largest connected component 
rtrv.lcl.Edgelist <- function(ntwk, v, max.dist = 6, remove.cycles.len=0) {
  
  #fetch edgelist
  edges.Mtx <- network::as.edgelist(ntwk, as.sna.edgelist = TRUE)[, c(1,2)]
  colnames(edges.Mtx) <- c('from', 'to')
  
  #subgraph of v
  ntwk.i <- ntwk2igraph.cvrt(ntwk)
  ntwk.i <- igraph::delete_edges(ntwk.i, #delete incoming edges to v
                                 igraph::get.edge.ids(ntwk.i,c(t(cbind(network::get.neighborhood(ntwk,v,'in'),v)))))
  igraph::V(ntwk.i)$name <- (c(1:length(ntwk$val)))
  igraph::V(ntwk.i)$label <- (c(1:length(ntwk$val)))
  v.bfs <- igraph::bfs(ntwk.i,v, neimode = 'out', dist = TRUE, order = TRUE, father = TRUE, rank = TRUE, pred = TRUE,unreachable = FALSE)
  subg.v <- igraph::induced_subgraph(ntwk.i,
                                     vids=which(!is.nan(v.bfs$dist) & v.bfs$dist<max.dist))
  subg.v <- igraph::as_edgelist(subg.v)
  subg.v <- cbind(as.numeric(subg.v[,1]),as.numeric(subg.v[,2]))
  x <- subg.v
  colnames(x) <- c('from','to')
  #x <- edges.Mtx[edges.Mtx[, 'to'] %in% which(!is.nan(v.bfs$dist) & v.bfs$dist<max.dist),]
  #x <- x[x[, 'to']!= v,]
  #x <- x[x[, 'from'] %in% c(v, which(!is.nan(v.bfs$dist) & v.bfs$dist<max.dist)),]
  
  #Remove cycles
  if(remove.cycles.len>0){
    y <- unique(c(x[, c(1,2)]))
    x[,1] <- match(x[,1], y)
    x[,2] <- match(x[,2], y)
    x.adj <- matrix(0, length(unique(c(x[, c(1,2)]))), length(unique(c(x[, c(1,2)]))))
    x.adj[x[, c(1,2)]] <- 1
    x.dag <- predictionet::adj.remove.cycles(x.adj, maxlength = remove.cycles.len)
    x.dag <- which(x.dag$adjmat.acyclic>0, arr.ind = TRUE)
    x.dag[,1] <- y[x.dag[,1]]
    x.dag[,2] <- y[x.dag[,2]]
    colnames(x.dag) <- c('from', 'to')
    x <- x.dag
  }
  return (x)
}

#post-order depth-first-search for backprop order of above subgraph
postorder.DFS <- function(edgeList, root, max.depth = 10) {
  v.visit <- c()
  v.order <- c()
  traverse.DFS <- function(v, cur.depth) {
    v.visit <<- unique(c(v.visit, v))
    cur.depth <- cur.depth+1
    if(cur.depth<max.depth) {
      for(v.new in edgeList[edgeList[, 'from']== v, 'to']) {
        if(!(v.new %in% v.visit)) {
          traverse.DFS(v.new, cur.depth = cur.depth)
        }
      }
    }
    v.order <<- c(v.order, v)
  }
  traverse.DFS(root, cur.depth = 0)
  return (v.order)
}