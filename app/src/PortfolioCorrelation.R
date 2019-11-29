import(Matrix)

utils     <- modules::use(here::here("ShinyApps/Arboreum/app/src/Utils.R"))

#' Computes/Updates correlation matrix of the portfolio of assets (loans made to others) for vertex in network
#'
#' @param ntwk : network object of class 'network' 
#' @param v    : vertex ID
#' @param v.new : vertex ID of new borrower to be considered (leave blank to compute afresh)
#' @param p.bwidth : bandwidth parameter to compute RBF kernel matrix for vertex correlation
#' @param p.scale  : scale parameter to compute RBF kernel matrix for vertex correlation
#' @param direction : direction of edge representing extension of trust
#'
#' @return correlation matrix
#' @export
#'
#' @examples
correlationUpdate <- function(ntwk, v, v.new = c(), p.bwidth = NULL, p.scale = 1, direction = 'out') {
  
  if(is.data.frame(ntwk[['val']][[v]]$Assets)) {
    df <- ntwk[['val']][[v]]$Assets
    v.ptfl <- sort(c(df$borrower, v.new))
  } else {
    #get attributes from network make dataframe and attach outgoing neighbor vertices
    v.out <- network::get.neighborhood(ntwk, v, direction)
    v.ptfl <- sort(unique(c(v.out, v.new)))
  }
  
  #stop if no neighbors
  if(length(v.ptfl) == 0) {
    return (NA)
  }
  if(length(v.ptfl) == 1) {
    return (matrix(1,1,1))
  }
  
  ntwk.i <- utils$ntwk2igraph.cvrt(ntwk)
  
  if(is.null(p.bwidth)) {
    p.bwidth = 1/igraph::mean_distance(ntwk.i)
  }
  if(direction == 'out') {
    edges2delete <-  igraph::get.edge.ids(ntwk.i, c(t(cbind(v, v.ptfl))))
  } else {
    edges2delete <-  igraph::get.edge.ids(ntwk.i, c(t(cbind(v.ptfl, v))))
  }
  krn.mtx <- igraph::similarity(igraph::delete_edges(ntwk.i, edges2delete[edges2delete>0]), mode = 'all', method = 'invlogweighted')
  
  #Select only vertices in neighborhood, ensures positive def -> compute correlation matrix
  krn.mtx <- p.scale^2*exp(krn.mtx[v.ptfl, v.ptfl]/(-2*p.bwidth^2)) 
  krn.mtx[krn.mtx == 1] <- 1-p.bwidth^2
  diag(krn.mtx) <- 1
  corr.mtx <- tryCatch({
    as.matrix(Matrix::nearPD(krn.mtx, corr = TRUE, ensureSymmetry = TRUE, keepDiag = TRUE, maxit = 10000)[[1]])}
    , error = function(e) {browser()})
  rownames(corr.mtx) <- colnames(corr.mtx) <- v.ptfl
  rm(krn.mtx)
  return (corr.mtx)
}