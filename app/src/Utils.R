import(intergraph)

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
  #find which vertices/edges have missing properties, loop through and append NA
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
  
  ntwk.i <- intergraph::asIgraph(ntwk, amap = rbind(rbind(intergraph::attrmap(), data.frame(type ="vertex", fromcls ="network", fromattr ="Portfolio", tocls ="igraph", toattr = NA, stringsAsFactors = FALSE)), data.frame(type ="vertex", fromcls ="network", fromattr ="Portfolio.corr", tocls ="igraph", toattr = NA, stringsAsFactors = FALSE)))
  return (ntwk.i)
}
