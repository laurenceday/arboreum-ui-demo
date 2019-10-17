library(intergraph)

#convert network class graph to igraph class graph
ntwk2igraph.cvrt <- function(ntwk) {
  for(a in names(which(sapply(ntwk[["val"]][[1]], function (x)  {length (x) >1})))) {
    network::set.vertex.attribute(ntwk, a, NA)
  }
  
  ntwk.i <- intergraph::asIgraph(ntwk, amap = rbind(rbind(intergraph::attrmap(), data.frame(type ="vertex", fromcls ="network", fromattr ="Portfolio", tocls ="igraph", toattr = NA, stringsAsFactors = FALSE)), data.frame(type ="vertex", fromcls ="network", fromattr ="Portfolio.corr", tocls ="igraph", toattr = NA, stringsAsFactors = FALSE)))
  return (ntwk.i)
}