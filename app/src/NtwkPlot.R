import(dplyr)
import(stats)
import(network)
import(ndtv)
import(networkDynamic)
import(networkD3)
import(grDevices)
import(RColorBrewer)

utils <- modules::use(here::here("ShinyApps/Arboreum/app/src/Utils.R"))

#' Title
#' 
#' @param ntwk : network object
#' @param trgt : node that borrows
#' @param transactions : transactions dataframe from frwd-prop
#' @param var.sz.vrt : variable for size of vertices ('LendAmt','Risk-Adj-Rtrn','Intr Rate','Security','Liabilities')
#' @param var.clr.vrt : variable for color of vertices('LendAmt','Risk-Adj-Rtrn','Intr Rate','Security')
#' @param var.sz.edg : variable for width of edges ('LendAmt','Risk-Adj-Rtrn', 'Intr Rate','Security')
#' @param var.clr.edg : variable for color of edges ('LendAmt','Risk-Adj-Rtrn', 'Intr Rate','Security')
#' @param vrt.clr.palette : RcolorBrewer palette for defining colors on vertices
#' @param edg.clr.palette : RcolorBrewer palette for defining colors on vertices
#' @param launchBrowser : see render.d3movie (edit for shiny output)
#' @param output.mode : see render.d3movie (edit for shiny output)
#' @param file_name : see render.d3movie (edit for shiny output)
#' @param script.type : see render.d3movie (edit for shiny output)
#'
#' @return d3movie object
#' @export
#'
#' @examples
movie.D3 <- function(ntwk,trgt,transactions,file_name,
                     var.sz.vrt='LendAmt',var.clr.vrt='Intr Rate',var.sz.edg='LendAmt',var.clr.edg='Intr Rate',
                     vrt.clr.palette='YlGnBu',edg.clr.palette='YlGnBu',
                     launchBrowser=TRUE, output.mode='htmlWidget',script.type='embedded'){

  #parse inputs
  var.sz.vrt <- switch(var.sz.vrt,'LendAmt'='TotAsst','Risk-Adj-Rtrn'='LendRAR','Intr Rate'='LendRate','Security'='LendScrt','Liabilities'='TotLiab')
  var.clr.vrt <- switch(var.clr.vrt,'LendAmt'='TotAsst','Risk-Adj-Rtrn'='LendRAR','Intr Rate'='LendRate','Security'='LendScrt','Liabilities'='TotLiab')
  var.sz.edg <- switch(var.sz.edg,'LendAmt'='Amt.C','Risk-Adj-Rtrn'='RAR.C','Intr Rate'='Rate.C','Security'='Scrt.C')
  var.clr.edg <- switch(var.clr.edg,'LendAmt'='Amt.C','Risk-Adj-Rtrn'='RAR.C','Intr Rate'='Rate.C','Security'='Scrt.C')
  
  #function to assign color to edges and vertices
  assign.color <- function(var.clr,clr.start='goldenrod',clr.end='limegreen',clr.palette=NULL){
    if(!is.null(clr.palette)){
      F2 <- colorRampPalette(RColorBrewer::brewer.pal(9,clr.palette))
    } else {
      F2 <- colorRampPalette(c(clr.start,clr.end), bias = length(unique(var.clr)), space = "rgb", interpolate = "linear")
    }
    colCodes <- F2(length(unique(var.clr)))
    rslt <- sapply(var.clr, function(x) colCodes[which(sort(unique(var.clr)) == x)])
    return(rslt)
  }
  
  #create initial dataframes
  g <- networkD3::igraph_to_networkD3(utils$ntwk2igraph.cvrt(ntwk))
  g$links <- data.frame(network::as.edgelist(ntwk))
  colnames(g$links) <- c('source','target')
  g$nodes$name <- as.numeric(as.character(g$nodes$name))
  nodes.trns <- sort(unique(c(transactions$from,transactions$to)))  

  #Merge General Attributes to Edges
  g$links$Trust <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                          function(x) ntwk$mel[[x]]$atl$Trust)
  g$links$Utilized <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                             function(x) ntwk$mel[[x]]$atl$Utilized)
  g$links$Risk <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                         function(x) ntwk$mel[[x]]$atl$Risk)
  g$links$Risk.Coef <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                              function(x) ntwk$mel[[x]]$atl$Risk.coef)
  
  #Merge General Attributes to nodes
  g$nodes$Equity <- sapply(g$nodes$name,function(x) ntwk$val[[x]]$Equity)
  g$nodes$PtflAtRisk <- sapply(g$nodes$name,function(x) ntwk$val[[x]]$PtflAtRisk)
  #g$nodes$TotLiab  <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount)),NA))
  g$nodes$LoanLiab <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount[type=='loan-origin'])),NA))
  g$nodes$HdgeLiab <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount[type=='loan-prop'])),NA))
  g$nodes$PassLiab <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount[type=='pass-thru'])),NA))
  
  #g$nodes$TotAsst  <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Assets),with(ntwk$val[[x]]$Assets,sum(amount)),NA))
  g$nodes$BondAsst <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Assets),with(ntwk$val[[x]]$Assets,sum(amount[type=='Bond'])),NA))
  g$nodes$HdgeAsst <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Assets),with(ntwk$val[[x]]$Assets,sum(amount[type=='contra-hedge'])),NA))
  
  #Merge Target Specific Attributes to nodes
  g$nodes$BondAmtTrgt <- sapply(g$nodes$name,function(x)  ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, lent[to==trgt & security>0 & is.na(tot.trust)]),NA))
  g$nodes$HdgeAmtTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, lent[to==trgt & security<0 & is.na(tot.trust)]),NA))
  g$nodes$BondRateTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, rate[to==trgt & security>0 & is.na(tot.trust)]),NA))
  g$nodes$HdgeRateTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, rate[to==trgt & security<0 & is.na(tot.trust)]),NA))
  g$nodes$BondScrttTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, security[to==trgt & security>0 & is.na(tot.trust)]),NA))
  g$nodes$HdgeScrtTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, security[to==trgt & security<0 & is.na(tot.trust)]),NA))
  
  #compute before and after assets and liabilities
  g$nodes$TotAsst.prv <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio), with(ntwk$val[[x]]$Portfolio, sum(lent[!is.na(tot.trust) & to!=trgt],na.rm=TRUE)),0))
  g$nodes$TotAsst.cur <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio), with(ntwk$val[[x]]$Portfolio, sum(lent[security>0],na.rm=TRUE)+sum(lent[security<0]*rate[security<0],na.rm=TRUE)),g$nodes$TotAsst.prv[g$nodes$name==x]))
  g$nodes$TotLiab.prv <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities), with(ntwk$val[[x]]$Liabilities, sum(amount[borrower!=trgt & type=='loan-origin']),na.rm=TRUE),0))
  g$nodes$TotLiab.cur <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities), with(ntwk$val[[x]]$Liabilities, sum(amount[type=='loan-origin'],na.rm=TRUE)+sum(amount[type!='loan-origin']*security[type!='loan-origin']),na.rm=TRUE),NA))
  
  ##BFS to see how loan propogates
  v.bfs <- igraph::bfs(igraph::graph_from_edgelist(as.matrix(transactions[,c(1,2)]), directed = TRUE),trgt, 
                       neimode = 'out', dist = TRUE, 
                       order = TRUE, father = TRUE, rank = TRUE, pred = TRUE,unreachable = FALSE)
  v.dfs <- igraph::dfs(igraph::graph_from_edgelist(as.matrix(transactions[,c(1,2)]), directed = TRUE),trgt, 
                       neimode = 'out', dist = TRUE, 
                       order = TRUE, father = TRUE, order.out = TRUE,unreachable = FALSE)
  
  #Add temporal structure
  ##post-order depth-first-search for backprop order of above subgraph
  frwd.order <- rev(utils$postorder.DFS(utils$rtrv.lcl.Edgelist(ntwk,trgt,remove.cycles.len = 0), trgt))
  frwd.order2 <- rev(utils$postorder.DFS(stats::setNames(transactions[c(1,2)],c('from','to')), trgt))
  frwd.order3 <- v.bfs$order[!is.na(v.bfs$order)]
  frwd.order4 <- rev(v.dfs$order.out[!is.na(v.dfs$order.out)])
  
  #onset.ordr <- cbind('name'=nodes.trns,'onset'=sapply(nodes.trns,function(x) v.bfs$dist[x]))
  
  #if frwd.ordr!=nodes.trns
  if(length(setdiff(frwd.order,nodes.trns))>1){browser()} #node mismatch
  
  #Assign Onset
  transactions$onset <- NA
  for(i in c(1:length(frwd.order))){
    transactions$onset[transactions$from==frwd.order[i]] <- i
  }
  transactions$onset <- transactions$onset
  transactions$terminus <- length(frwd.order)
  transactions$duration <- transactions$terminus-transactions$onset
  
  #Assign color
  transactions$RAR.C <- with(transactions,Rate.C/(1-Scrt.C))
  transactions$clr.edg <-  assign.color(log(transactions[[var.clr.edg]]),clr.palette=edg.clr.palette)
  #transactions$clr.rate.edg <- assign.color(log(transactions$Rate.C),clr.palette=edg.clr.palette)
  #transactions$clr.scrt.edg <- assign.color(log(transactions$Scrt.C),clr.palette=edg.clr.palette)
  #transactions$clr.RAR.edg <- assign.color(log(transactions$RAR.C),clr.palette=edg.clr.palette)
  
  g$nodes$onset <- sapply(g$nodes$name,function(x) min(with(transactions,onset[to==x])))-1
  g$nodes$onset[g$nodes$onset<0] <- 0
  g$nodes$onset[g$nodes$onset>length(frwd.order)] <- length(frwd.order)
  g$nodes$onset[g$nodes$name==trgt] <- -Inf
  g$nodes$terminus <- Inf
  
  #Merge Target Specific Attributes to edges
  if(is.data.frame(transactions)){
    colnames(transactions)[c(1,2)] <- colnames(g$links)[c(1,2)]
    transactions$brw <- NULL
    g$links <- merge(g$links,transactions,all.x = TRUE)
  } else {
    #create some recursive way to do it
  }
  
  #Set group membership variable
  g$nodes$LoanGroup <- 'out'
  g$links$LoanGroup <- 'out'
  g$nodes$LoanGroup[g$nodes$name %in% nodes.trns] <- 'in'
  g$links$LoanGroup[!is.na(g$links$Amt.C)] <- 'in'
  g$nodes$LoanGroup[g$nodes$name==trgt] <- 'trgt'
  g$links$LoanGroup[g$links$source==trgt] <- 'trgt'
  
  ##create dynamic network
  g.orig <- g
  #remove unused nodes, reset names
  g$links <- g$links[g$links$LoanGroup!='out',]
  g$nodes <- g$nodes[g$nodes$LoanGroup!='out',]
  if(length(setdiff(nodes.trns,nodes.trns))>1){browser()} #node mismatch
  
  g$links$source <- match(g$links$source,nodes.trns)
  g$links$target <- match(g$links$target,nodes.trns)
  
  g$nodes$label <- g$nodes$name
  g$nodes$name  <-match(g$nodes$name,nodes.trns)
  g$nodes$TotAsst.cur[g$nodes$name==trgt] <- 0
  #create network
  ntwk.dyn <- networkDynamic(edge.spells=g$links[g$links$LoanGroup!='out',
                                                 c('onset','terminus','source','target',
                                                   setdiff(colnames(g$links),c('onset','terminus','source','target','duration')))], 
                             vertex.spells=g$nodes[g$nodes$LoanGroup!='out',
                                                   c('onset','terminus','name',
                                                     setdiff(colnames(g$nodes),c('onset','terminus','name','duration')))],
                             create.TEAs=TRUE,
                             edge.TEA.names = setdiff(colnames(g$links),c('onset','terminus','source','target','duration')),
                             vertex.TEA.names = setdiff(colnames(g$nodes),c('onset','terminus','name','duration')))

  #calculate effective rates post-propagation
  calc.eff.rates <- function(node){
    ptfl <- ntwk$val[[node]]$Portfolio
    if(is.data.frame(ptfl)){
      indx.bond <- which(with(ptfl,is.na(tot.trust) & to==trgt & security>0))
      indx.hdge <- which(with(ptfl,is.na(tot.trust) & to==trgt & security<0))
      if (length(indx.bond)>0){
        tot.risk <- sum(with(ptfl,c(lent[indx.bond],lent[indx.hdge]*abs(security[indx.hdge]))),na.rm = TRUE)
        tot.prft <- sum(with(ptfl,c(lent[indx.bond]*abs(rate[indx.bond]),lent[indx.hdge]*abs(rate[indx.hdge]))),na.rm = TRUE)
        tot.scrt <- sum(with(ptfl,c(lent[indx.bond]*abs(security[indx.bond]))),na.rm = TRUE)
        
        eff.rate <- tot.prft/tot.risk#ptfl$lent[indx.bond]
        eff.scrt <- tot.scrt/tot.risk
        eff.rar <- tot.prft/(tot.risk-tot.scrt)
        return(c(eff.rate,eff.scrt,eff.rar))
      }
    }
    return(c(NA,NA,NA))
  }
  
  #empty dataframe to store effective rate to which color will be assigned to later
  df.clr <- data.frame(node=numeric(),amt=numeric(),rate=numeric(),scrt=numeric(),RAR=numeric(),onset=numeric(),terminus=numeric())
  
  #Add Time Dependent attributes
  for(v in g$nodes$name[g$nodes$LoanGroup!='out']){
    
    #add starting liabilities
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'TotLiab',v=v,value=g$nodes$TotLiab.prv[g$nodes$name==v], 
                                          onset=0,terminus=which(frwd.order==nodes.trns[v])-0)
    
    #update assets as edges come in
    df <- transactions %>% dplyr::filter(target==nodes.trns[v]) %>% dplyr::arrange(onset) 
    df$onset <- df$onset+1
    onsets <- sort(unique(df$onset))
    if(ifelse(nrow(df)>0,onsets[length(onsets)]>which(frwd.order==nodes.trns[v]),FALSE)){
      if(nrow(df)>1){ 
        df <- df[c(1:(nrow(df)-1)),]
      } else {
        df <- df[integer(0),]
      }
    } else {
      onsets <- c(onsets,which(frwd.order==nodes.trns[v]))
    }#issue with ordering
    
    #add starting assets
    ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'TotAsst',v=v,value=0,#g$nodes$TotAsst.prv[g$nodes$name==v], 
                                                    onset=0,terminus=min(onsets))},
                         error=function(e){browser()})
    #add starting interest rate
    ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'LendRate',v=v,value=1,onset=0,terminus=min(onsets))},
                         error=function(e){browser()})
    
    #add starting securitization
    ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'LendScrt',v=v,value=1,onset=0,terminus=min(onsets))},
                         error=function(e){browser()})
    
    #add starting risk adjusted return
    ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'LendRAR',v=v,value=0,onset=0,terminus=min(onsets))},
                         error=function(e){browser()})
    
    df.clr <- rbind(df.clr,list(node=v,amt=0,rate=1,scrt=1,RAR=0,onset=0,terminus=min(onsets)))
    
    #iterate over transactions
    for(i in if(length(onsets)>1){seq(1,length(onsets)-1)}else{integer(0)}){
      
      #update assets
      amt <- sum(df$Amt.C[df$onset<=onsets[i]])#+g$nodes$TotAsst.prv[g$nodes$name==v]
      ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'TotAsst',v=v,value=amt,
                                                      onset=onsets[i],terminus=onsets[i+1])},
                           error=function(e){browser()})
      #update interest rate
      rate <- stats::weighted.mean(df$Rate.C[df$onset<=onsets[i]],df$Amt.C[df$onset<=onsets[i]])
      ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'LendRate',v=v,value=rate,
                                                      onset=onsets[i],terminus=onsets[i+1])},
                           error=function(e){browser()})
      
      #update securitization
      scrt <- stats::weighted.mean(df$Scrt.C[df$onset<=onsets[i]],df$Amt.C[df$onset<=onsets[i]])
      ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'LendScrt',v=v,value=scrt,
                                                      onset=onsets[i],terminus=onsets[i+1])},
                           error=function(e){browser()})
      
      #update risk Adjusted return
      RAR <- rate/(1-scrt)
      ntwk.dyn <- tryCatch({activate.vertex.attribute(ntwk.dyn,'LendScrt',v=v,value=RAR,
                                                      onset=onsets[i],terminus=onsets[i+1])},
                           error=function(e){browser()})
      
      if(any(is.na(c(amt,rate,scrt,RAR)))){browser()}
      #update dataframe
      df.clr <- rbind(df.clr,list(node=v,amt=amt,rate=rate,scrt=scrt,RAR=RAR,onset=onsets[i],terminus=onsets[i+1]))
    }
    
    #update liabilities when node finishes propogating
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'TotLiab',v=v,value=g$nodes$TotLiab.cur[g$nodes$name==v], 
                                          onset=which(frwd.order==nodes.trns[v])-0,terminus=length(frwd.order)-0)
    
    #final assets when node finishes propogating and sells off assets
    amt <- g$nodes$TotAsst.cur[g$nodes$name==v]-g$nodes$TotAsst.prv[g$nodes$name==v]
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'TotAsst',v=v,value=amt, 
                                          onset=which(frwd.order==nodes.trns[v]),terminus=Inf)
    
    if(v!=which(nodes.trns==trgt)){
      rates <-  tryCatch({calc.eff.rates(nodes.trns[v])},
                         error=function(e){browser()})
    } else {
      rates <- c(-with(transactions[transactions$source==nodes.trns[v],],stats::weighted.mean(Rate.C,Amt.C)),
                 -with(transactions[transactions$source==nodes.trns[v],],stats::weighted.mean(Scrt.C,Amt.C))
      )
      rates <- c(rates,rates[1]/(1-rates[2]))
    }
    #final interest rate when node finishes propogating and sells off assets
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'LendRate',v=v,value=rates[1], 
                                          onset=onsets[length(onsets)],terminus=Inf)
    #final securitization when node finishes propogating and sells off assets
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'LendScrt',v=v,value=rates[2], 
                                          onset=onsets[length(onsets)],terminus=Inf)
    #final Risk Adjusted Return
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'LendScrt',v=v,value=rates[3], 
                                          onset=onsets[length(onsets)],terminus=Inf)
    
    df.clr <- rbind(df.clr,list(node=v,amt=amt,rate=rates[1],scrt=rates[2],RAR=rates[3],onset=which(frwd.order==nodes.trns[v]),terminus=Inf))
  }
  rm(rates,rate,scrt,RAR)
  
  #assign colors to node
  var.clr.vrt <- switch(var.clr.vrt,'TotAsst'='Amt','LendRAR'='RAR','LendRate'='rate','LendScrt'='scrt')#,'TotLiab')
  df.clr$clr.vrt <- assign.color(abs(df.clr[[var.clr.vrt]]),clr.palette=vrt.clr.palette)
  df.clr$clr.vrt[df.clr$node==which(nodes.trns==trgt)] <- '#EF3B2C'
  
  #create new node attribute for color
  ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'clr.vrt','#E0E0E0', onset=0,terminus=length(frwd.order))
  for(i in c(1:nrow(df.clr))){
    ntwk.dyn <- activate.vertex.attribute(ntwk.dyn,'clr.vrt', v=df.clr$node[i],
                                          value=df.clr$clr.vrt[i], 
                                          onset=df.clr$onset[i],terminus=df.clr$terminus[i]) 
  }
  
  #reconcile edge and vertex ativity
  reconcile.vertex.activity(ntwk.dyn,"expand.to.edges")
  #reconcile.edge.activity(ntwk.dyn,'match.to.vertices')
  
  ##Create Animation
  compute.animation(ntwk.dyn, animation.mode = "Graphviz",
                    slice.par=list(start=0, end=length(frwd.order), interval=1, 
                                   aggregate.dur=1, rule='any'),
                   # default.dist=20,
                    layout.par = list(gv.engine='neato',mode='ipsep',overlap='ipsep',sep='+100,100',weight=-10,splines=TRUE),
                    #layout.par=list(gv.engine='sfdp',K=1,repulsiveforce=100,overlap=FALSE),
                    weight.attr = NULL,#'Amt.C.active', #modify, to set property (NULL)
                    chain.direction = 'forward')
  
  render.d3movie(ntwk.dyn, usearrows = TRUE, 
                 displaylabels = FALSE, 
                 #label=net3 %v% "media", #vertex label name
                 bg="#FFFFFF", #background 
                 vertex.border="#333333",
                 vertex.cex = function(slice){replace(log(slice %v% var.sz.vrt+1),log(slice %v% var.sz.vrt+1)==0,1)}, #vertex size
                 vertex.col = function(slice){slice %v% 'clr.vrt'}, #vertex color
                 edge.lwd = function(slice){replace(log(slice %e% var.sz.edg+1),log(slice %e% var.sz.edg+1)==0,1)}, #edge width
                 edge.col = function(slice){slice %e% 'clr.edg'}, #edge color
                 vertex.tooltip = function(slice){tryCatch({paste("<b>Assets:</b>", round(slice %v% "TotAsst",2), "<br>",
                                                                  "<b>Liabilities:</b>", round(slice %v% "TotLiab",2)," <br>",
                                                                  "<b>Eff. Rate:</b>", round(slice %v% "LendRate",2), "<br>",
                                                                  "<b>Eff. Collat:</b>", round(slice %v% "LendScrt",2))},
                                                           error=function(e){browser()})},
                 edge.tooltip = function(slice){tryCatch({paste("<b>Amt Xfer:</b>", round(ifelse(length(slice$mel)>0,slice %e% "Amt.C",0),2), "<br>", 
                                                                "<b>Int Rate:</b>", round(ifelse(length(slice$mel)>0,slice %e% "Rate.C",1),2)-1, "<br>",
                                                                "<b>Collateral:</b>", round(ifelse(length(slice$mel)>0,slice %e% "Scrt.C",1),2))},
                                                         error=function(e){browser()})},
                 launchBrowser=launchBrowser, filename=file_name, output.mode=output.mode, script.type = script.type,
                 render.par=list(tween.frames = 15, show.time = FALSE),
                 plot.par=list(mar=c(0,0,0,0)))
            
}

#' Plot network
#'
#' @param ntwk : network object
#' @param trgt : node that borrows
#' @param transactions : transactions dataframe from frwd-prop
#' @param var.sz.vrt : variable for size of vertices ('Tot Amt Brw','Tot Amt Prop','Tot Bond Assets','Tot Hedge Assets','Tot Assets pre-loan','Tot Assets post-loan','Tot Liabilities pre-loan','Tot Liabilities post-loan','Delta Assets post-loan','Delta Liabilities post-loan','Trgt Amt Lent','Trgt Amt Prop','Trgt Rate Lent','Trgt Rate Prop','Trgt Scrt Lent','Trgt Scrt Prop','Trgt Eff Rate','Trgt Eff Scrt','Trgt Eff Risk-adj-Return')
#' @param var.sz.edg : variable for width of edges ('Tot Amt Trusted','Tot Amt Trust Used','Subj Risk','Amt Debt X-fer','Rate Debt X-fer','Scrt Debt X-fer')
#' @param var.clr.edg : variable for color of edges ('Tot Amt Trusted','Tot Amt Trust Used','Subj Risk','Amt Debt X-fer','Rate Debt X-fer','Scrt Debt X-fer')
#' @param vrt.clr.palette : RcolorBrewer palette for defining colors on vertices
#' @param edg.clr.palette : RcolorBrewer palette for defining colors on vertices
#' @param charge : repulsion/attraction of vertices (for plotting)
#' @param linkDistance : distance of links (for plotting)
#' @param height : height of display area (for plotting)
#' @param width : width of display are (for plotting)
#'
#' @return d3movie object
#' @export
#'
#' @examples
plot.D3 <- function(ntwk,trgt,transactions,
                    var.sz.vrt='Delta Assets post-loan',var.sz.edg='Amt Debt X-fer',var.clr.edg='Rate Debt X-fer',
                    vrt.clr.palette='YlGnBu',edg.clr.palette='YlGnBu',
                    charge=0,linkDistance=250,height=700,width=1000){
  
  #parse inputs
  var.sz.vrt <- switch(var.sz.vrt,'Tot Amt Brw'='LoanLiab',
                       'Tot Amt Prop'='LoanLiab',
                       'Tot Bond Assets'='BondAsst',
                       'Tot Hedge Assets'='HdgeAsst',
                       'Tot Assets pre-loan'='TotAsst.prv',
                       'Tot Assets post-loan'='TotAsst.cur',
                       'Tot Liabilities pre-loan'='TotLiab.cur',
                       'Tot Liabilities post-loan'='TotLiab.prv',
                       'Delta Assets post-loan'='DiffAsst',
                       'Delta Liabilities post-loan'='DiffLiab',
                       'Trgt Amt Lent'='BondAmtTrgt',
                       'Trgt Amt Prop'='HdgeAmtTrgt',
                       'Trgt Rate Lent'='BondRateTrgt',
                       'Trgt Rate Prop'='HdgeRateTrgt',
                       'Trgt Scrt Lent'='BondScrtTrgt',
                       'Trgt Scrt Prop'='HdgeScrtTrgt',
                       'Trgt Eff Rate'='Eff.Rate',
                       'Trgt Eff Scrt'='Eff.Scrt',
                       'Trgt Eff Risk-adj-Return'='Eff.RAR')
  #var.clr.vrt <- switch(var.clr.vrt,vrt.switch)
  var.sz.edg <- switch(var.sz.edg,'Tot Amt Trusted'='Trust',
                       'Tot Amt Trust Used'='Utilized',
                       'Subj Risk'='Risk.Coef',
                       'Amt Debt X-fer'='Amt.C',
                       'Rate Debt X-fer'='Rate.C',
                       'Scrt Debt X-fer'='Scrt.C')
  var.clr.edg <- switch(var.clr.edg,'Tot Amt Trusted'='Trust',
                        'Tot Amt Trust Used'='Utilized',
                        'Subj Risk'='Risk.Coef',
                        'Amt Debt X-fer'='Amt.C',
                        'Rate Debt X-fer'='Rate.C',
                        'Scrt Debt X-fer'='Scrt.C')
  
  #function to assign color to edges and vertices
  assign.color <- function(var.clr,clr.start='goldenrod',clr.end='limegreen',clr.palette=NULL){
    if(!is.null(clr.palette)){
      F2 <- colorRampPalette(RColorBrewer::brewer.pal(9,clr.palette))
    } else {
      F2 <- colorRampPalette(c(clr.start,clr.end), bias = length(unique(var.clr)), space = "rgb", interpolate = "linear")
    }
    colCodes <- F2(length(unique(var.clr)))
    rslt <- unlist(sapply(var.clr, function(x) ifelse(is.na(x),'#808080',colCodes[which(sort(unique(var.clr)) == x)])))
    return(rslt)
  }
  
  #create initial dataframes
  g <- networkD3::igraph_to_networkD3(utils$ntwk2igraph.cvrt(ntwk))
  g$links <- data.frame(network::as.edgelist(ntwk))
  colnames(g$links) <- c('source','target')
  g$nodes$name <- as.numeric(as.character(g$nodes$name))
  
  #Merge General Attributes to Edges
  g$links$Trust <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                          function(x) ntwk$mel[[x]]$atl$Trust)
  g$links$Utilized <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                             function(x) ntwk$mel[[x]]$atl$Utilized)
  g$links$Risk <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                         function(x) ntwk$mel[[x]]$atl$Risk)
  g$links$Risk.Coef <- sapply(unlist(network::get.dyads.eids(ntwk,g$links$source,g$links$target)),
                              function(x) ntwk$mel[[x]]$atl$Risk.coef)
  
  #Merge General Attributes to nodes
  g$nodes$Equity <- sapply(g$nodes$name,function(x) ntwk$val[[x]]$Equity)
  g$nodes$PtflAtRisk <- sapply(g$nodes$name,function(x) ntwk$val[[x]]$PtflAtRisk)
  #g$nodes$TotLiab  <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount)),NA))
  g$nodes$LoanLiab <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount[type=='loan-origin'])),NA))
  g$nodes$HdgeLiab <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount[type=='loan-prop'])),NA))
  g$nodes$PassLiab <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities),with(ntwk$val[[x]]$Liabilities,sum(amount[type=='pass-thru'])),NA))
  
  #g$nodes$TotAsst  <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Assets),with(ntwk$val[[x]]$Assets,sum(amount)),NA))
  g$nodes$BondAsst <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Assets),with(ntwk$val[[x]]$Assets,sum(amount[type=='Bond'])),NA))
  g$nodes$HdgeAsst <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Assets),with(ntwk$val[[x]]$Assets,sum(amount[type=='contra-hedge'])),NA))

  #Merge Target Specific Attributes to nodes
  g$nodes$BondAmtTrgt <- sapply(g$nodes$name,function(x)  ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, lent[to==trgt & security>0 & is.na(tot.trust)]),NA))
  g$nodes$HdgeAmtTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, lent[to==trgt & security<0 & is.na(tot.trust)]),NA))
  g$nodes$BondRateTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, rate[to==trgt & security>0 & is.na(tot.trust)]),NA))
  g$nodes$HdgeRateTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, rate[to==trgt & security<0 & is.na(tot.trust)]),NA))
  g$nodes$BondScrttTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, security[to==trgt & security>0 & is.na(tot.trust)]),NA))
  g$nodes$HdgeScrtTrgt <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio),with(ntwk$val[[x]]$Portfolio, security[to==trgt & security<0 & is.na(tot.trust)]),NA))
  
  #compute before and after assets and liabilities
  g$nodes$TotAsst.prv <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio), with(ntwk$val[[x]]$Portfolio, sum(lent[!is.na(tot.trust) & to!=trgt],na.rm=TRUE)),0))
  g$nodes$TotAsst.cur <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Portfolio), with(ntwk$val[[x]]$Portfolio, sum(lent[security>0],na.rm=TRUE)+sum(lent[security<0]*rate[security<0],na.rm=TRUE)),g$nodes$TotAsst.prv[g$nodes$name==x]))
  g$nodes$TotLiab.prv <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities), with(ntwk$val[[x]]$Liabilities, sum(amount[borrower!=trgt & type=='loan-origin']),na.rm=TRUE),0))
  g$nodes$TotLiab.cur <- sapply(g$nodes$name,function(x) ifelse(is.data.frame(ntwk$val[[x]]$Liabilities), with(ntwk$val[[x]]$Liabilities, sum(amount[type=='loan-origin'],na.rm=TRUE)+sum(amount[type!='loan-origin']*security[type!='loan-origin']),na.rm=TRUE),NA))
  
  #calculate effective rates post-propagation
  calc.eff.rates <- function(node){
    ptfl <- ntwk$val[[node]]$Portfolio
    if(is.data.frame(ptfl)){
      indx.bond <- which(with(ptfl,is.na(tot.trust) & to==trgt & security>0))
      indx.hdge <- which(with(ptfl,is.na(tot.trust) & to==trgt & security<0))
      if (length(indx.bond)>0){
        tot.risk <- sum(with(ptfl,c(lent[indx.bond],lent[indx.hdge]*abs(security[indx.hdge]))),na.rm = TRUE)
        tot.prft <- sum(with(ptfl,c(lent[indx.bond]*abs(rate[indx.bond]),lent[indx.hdge]*abs(rate[indx.hdge]))),na.rm = TRUE)
        tot.scrt <- sum(with(ptfl,c(lent[indx.bond]*abs(security[indx.bond]))),na.rm = TRUE)
        
        eff.rate <- tot.prft/tot.risk#ptfl$lent[indx.bond]
        eff.scrt <- tot.scrt/tot.risk
        eff.rar <- tot.prft/(tot.risk-tot.scrt)
        return(c(eff.rate,eff.scrt,eff.rar))
      }
    }
    return(c(NA,NA,NA))
  }
  
  g$nodes$DiffAsst <- with(g$nodes,TotAsst.cur-TotAsst.prv)
  g$nodes$DiffLiab <- with(g$nodes,TotLiab.cur-TotLiab.prv) 
  g$nodes <- cbind(g$nodes,
                   t(data.frame(sapply(g$nodes$name,calc.eff.rates),row.names=c('Eff.Rate','Eff.Scrt','Eff.RAR'))))
  
  #Merge Target Specific Attributes to edges
  if(is.data.frame(transactions)){
    colnames(transactions)[c(1,2)] <- colnames(g$links)[c(1,2)]
    transactions$brw <- NULL
    g$links <- merge(g$links,transactions,all.x = TRUE)
  } else {
    #create some recursive way to do it
  }
  
  #Set group membership variable
  g$nodes$LoanGroup <- 'out'
  g$links$LoanGroup <- 'out'
  
  g$nodes$LoanGroup[!is.na(g$nodes$BondAmtTrgt)] <- 'in'
  g$links$LoanGroup[!is.na(g$links$Amt.C)] <- 'in'
  
  g$nodes$LoanGroup[g$nodes$name==trgt] <- 'trgt'
  g$links$LoanGroup[g$links$source==trgt] <- 'trgt'
  
  #color edge
  #print(g$links[[var.clr.edg]])
  g$links$clr.edg <-  assign.color(log(g$links[[var.clr.edg]]), clr.palette=edg.clr.palette)
  
  #NA edge width
  g$links[[var.sz.edg]][is.na(g$links[[var.sz.edg]])] <- round(min(g$links[[var.sz.edg]],na.rm=TRUE)/10,2)
  
  #0 node size
  g$nodes[[var.sz.vrt]][g$nodes[[var.sz.vrt]]==0 | is.na(g$nodes[[var.sz.vrt]])] <- round(min( g$nodes[[var.sz.vrt]][g$nodes[[var.sz.vrt]]>0],na.rm=TRUE)/10,2)
  
  #color nodes
  #g$nodes$clr.vrt <-  assign.color(log(g$nodes[[var.clr.vrt]]),clr.palette=vrt.clr.palette)
  
  #color group
  #colCodes <- c("#A0A0A0", RColorBrewer::brewer.pal(length(unique(g$nodes$LoanGroup))-1,'Set2'))[c(1:length(unique(g$nodes$LoanGroup)))]
  #nodeColor.JS <- paste0("d3.scale.ordinal().domain(['out','",
  #                       paste0(setdiff(unique(g$nodes$LoanGroup),"out"),collapse="','"),"']).range('",
  #                       paste0(colCodes,collapse="','"),"');")
  
  #reindex staring at 0
  g$nodes$name <- as.numeric(as.character(g$nodes$name))
  g$links$source <- as.numeric(as.character(g$links$source))-1
  g$links$target <- as.numeric(as.character(g$links$target))-1

  #Plot
  plt <- networkD3::forceNetwork(Links = g$links, Nodes = g$nodes, Source = "source",Target = "target", NodeID = "name",
                                 opacity = 0.85, zoom = TRUE, opacityNoHover = 0.1, arrows = TRUE, bounded=TRUE,
                                 height=height,width=width,
                                 charge=charge,
                                 Group = "LoanGroup", 
                                 linkColour = g$links$clr.edg,
                                 Value = var.sz.edg, 
                                 Nodesize = var.sz.vrt, 
                                 colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory10);"), #vertex color
                                 linkDistance = linkDistance,#networkD3::JS("function(d) { return 10*d.value; }"), 
                                 linkWidth = networkD3::JS("function(d) { return Math.sqrt(d.value)+1; }"))
  
  return(plt)
}
