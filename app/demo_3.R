import(dplyr)
import(network)
import(modules)
import(plotly)
import(here)
import(ndtv)

ntwkPlot            <- modules::use(here::here("src/NtwkPlot.R"))

postBrwTransactions <- readRDS(here::here("pregenerated/LoanTransactions.rds"))
postBrwNetwork      <- readRDS(here::here("pregenerated/LoanNetwork.rds"))

networkMovie        <- readRDS(here::here("pregenerated/NetworkMovie.rds"))
 
output$networkMovie <- ndtv::renderNdtvAnimationWidget(networkMovie)

##Calculate Asset Changes
f <- function(df){
  tot.risk <- with(df,sum(Amt.C-Amt.S+ifelse(is.na(Scrt.S),0,(Scrt.S-Scrt.C)*Amt.S)))
  tot.prft <- with(df,sum((Amt.C-Amt.S)*(Rate.C-1)+ifelse(is.na(Rate.S),0,Amt.S*(Rate.C-Rate.S))))
  tot.scrt <- with(df,sum((Amt.C-Amt.S)*Scrt.C-ifelse(is.na(Scrt.S),0,Amt.S*(Scrt.S-Scrt.C))))            
  
  AmtLent = with(df,round(sum(Amt.C-Amt.S),2))
  LendRate = with(df,paste0(round(100*(stats::weighted.mean(Rate.C,Amt.C)-1),2),'%'))
  ScrtRate = with(df,paste0(round(100*stats::weighted.mean(Scrt.C,Amt.C),2),'%'))
  AmtEncumb = with(df,round(sum(ifelse(is.na(Rate.S),0,Amt.S*(Rate.C-Rate.S))),2))+AmtLent
  
  eff.rate <- tot.prft/tot.risk
  eff.scrt <- tot.scrt/tot.risk
  eff.rar  <- tot.prft/(tot.risk-tot.scrt)
  #if(eff.rate<0 | eff.rar <0 | eff.scrt <0 ){browser()}
  #eff.rate <- paste0(eff.rate,'%')
  #eff.scrt <- paste0(eff.scrt,'%')
  #eff.rar  <- paste0(eff.rar,'%')
  
  brw.conf <- round(postBrwNetwork$val[[unique(df$to)]]$Subj.risk %>% filter(to==unique(df$brw)) %>% select(Risk.coef),4)
  
  return(as.data.frame(list('brw.conf'=brw.conf[[1]],'AmtLent'=AmtLent,'LendRate'=LendRate,'ScrtRate'=ScrtRate,
                            'AmtEncumb'=AmtEncumb,'eff.rate'=eff.rate,'eff.scrt'=eff.scrt,'eff.rar'=eff.rar)))
}

postBrwAssets <- postBrwTransactions %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>% 
                 do(f(.)) %>% ungroup() %>% replace(.,is.na(.),0) %>%
                 mutate(to=paste0('Node ',to)) %>% select(to,brw.conf,AmtLent,eff.rate,eff.scrt,eff.rar)

names(postBrwAssets)[1] <- "Node"
names(postBrwAssets)[2] <- "Loan Confidence"
names(postBrwAssets)[3] <- "Amount Loaned"
#names(postBrwAssets)[4] <- "Amount Encumbered"
names(postBrwAssets)[4] <- "Eff. Interest Rate"
names(postBrwAssets)[5] <- "Eff. Collateral Rate"
names(postBrwAssets)[6] <- "Eff. Risk-adj-Return"

assetMoneyFlow      <- sum(postBrwAssets[[3]])

##Calculate Liability Changes
postBrwLiabilities  <- postBrwTransactions %>% filter(from==111) %>% group_by(from) %>% 
                       summarise(AmtEncumb=sum(Amt.C),Rate=mean(Rate.C), Scrt=mean(Scrt.C),Type='Loan Origin') %>%
                       mutate(to='You') %>% select(to,AmtEncumb,Rate,Scrt,Type) %>%
                       bind_rows(.,postBrwTransactions %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>% 
                                   do(f(.)) %>% ungroup() %>% replace(.,is.na(.),0) %>%
                                   mutate(to=paste0('Node ',to),Rate=0,Scrt=0,Type='Upon Default') %>% 
                                   select(to,AmtEncumb,Rate,Scrt,Type))
  
names(postBrwLiabilities)[1] <- "Node"
names(postBrwLiabilities)[2] <- "Amount"
names(postBrwLiabilities)[3] <- "Interest Rate"
names(postBrwLiabilities)[4] <- "Collateral Rate"
names(postBrwLiabilities)[5] <- "Liability Type"

liabilityMoneyFlow  <- sum(postBrwLiabilities[[2]])

##Calculate Transactions
txMoneyFlow         <- sum(postBrwTransactions$Amt.C)
txMedianValue       <- median(postBrwTransactions$Amt.C)
txMeanValue         <- mean(postBrwTransactions$Amt.C)
postBrwTransactions[is.na(postBrwTransactions)] <- 0
postBrwTransactions$Amount.Retained <- with(postBrwTransactions, Amt.C-Amt.S+Amt.S*(ifelse(is.na(Scrt.S),0,Scrt.S-Scrt.C))) #calculates amount encumbered (ToDo: Ensure right formula)
txEncumberedValue   <- sum(postBrwTransactions$Amount.Retained, na.rm = TRUE)
postBrwTransactions <- subset(postBrwTransactions, select = -c(6, 7, 8, 9))

postBrwTransactions[[1]] <- paste0('Node ',postBrwTransactions[[1]])
postBrwTransactions[[2]] <- paste0('Node ',postBrwTransactions[[2]])
postBrwTransactions[[1]][postBrwTransactions[[1]]=='Node 111'] <- 'You'

names(postBrwTransactions)[1] <- "Seller"
names(postBrwTransactions)[2] <- "Buyer"
names(postBrwTransactions)[3] <- "Amount"
names(postBrwTransactions)[4] <- "Interest Rate"
names(postBrwTransactions)[5] <- "Collateral Rate"
names(postBrwTransactions)[6] <- "Amount Encumbered"

## render tables
output$yourAssets <- DT::renderDataTable({
  as.data.frame(postBrwAssets)
},
options = list(
  rowCallback = JS(
    "function(row, data) {",
    "var num3  = '$' + data[3].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var sec4 = Math.round(data[4] * 100).toString() + '%';",
    "var sec5 = Math.round(data[5] * 100).toString() + '%';",
    "var sec6 = Math.round(data[6] * 100).toString() + '%';",
    "$('td:eq(3)', row).html(num3);", 
    "$('td:eq(4)', row).html(sec4);",
    "$('td:eq(5)', row).html(sec5);",
    "$('td:eq(6)', row).html(sec6);",
    "}")
), callback = JS("table.order([3, 'desc']).draw();")
)

output$yourLiabilities <- DT::renderDataTable({
  as.data.frame(postBrwLiabilities)
},
options = list(
  rowCallback = JS(
    "function(row, data) {",
    "var num2  = '$' + data[2].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var rate3 = (data[3] * 100).toFixed(3).toString() + '%';",
    "var sec4 = (Math.round(data[4] * 100).toFixed(2)).toString() + '%';",
    "$('td:eq(2)', row).html(num2);", 
    "$('td:eq(3)', row).html(rate3);",
    "$('td:eq(4)', row).html(sec4);",
    "}")
), callback = JS("table.order([2, 'desc']).draw();")
)

output$precookedTransactions <- DT::renderDataTable({
  as.data.frame(postBrwTransactions)
},
options = list(
  rowCallback = JS(
    "function(row, data) {",
    "var num3  = '$' + data[3].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var rate4 = (data[4] * 100 - 100).toFixed(3).toString() + '%';",
    "var sec5 = (Math.round(data[5] * 100)).toString() + '%';",
    "var num6  = '$' + data[6].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "$('td:eq(3)', row).html(num3);", #changes here
    "$('td:eq(4)', row).html(rate4);",
    "$('td:eq(5)', row).html(sec5);",
    "$('td:eq(6)', row).html(num6);",
    "}")
), callback = JS("table.order([3, 'desc']).draw();")
)

output$pageStub <- renderUI({rv$limn;
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("<h2>Scenario #1: You Take Out A Loan</h2>"),
               HTML("Let's have a look at a breakdown of how Arboreum has propagated your loan across the network.<br><br>
                     Remember, the loan was for USD1020, at 8% interest, with a 22% collateral bond.<br><br>
                     Your loan has - in effect - been turned into a bond, and traded across the Arboreum network.<br><br>
                     People who have extended credit to you have taken on the highest risk, but stand to make the highest return.<br><br>
                     Rather holding the entirety of your loan, they may add their own layer of collateral and sell parts of the debt to people that extended credit to <i>them</i>.<br><br>
                     Anyone who has purchased debt is free to do the same, repeated until there are no candidates to sell debt to.<br>"),
               HTML("<h3>Loan Transactions</h3>"),
               HTML("<i>Below is a table of every transaction which took place during the distribution of your loan.</i><br><br>
                    In short, your peers contribute an amount of the loan principal that their agent has determined they are comfortable with, and then sold their debt on to their own peers so as to minimise their own exposure.<br><br>
                    A transaction chain following such a path terminates when a participant cannot find a peer of their own willing to participate.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("precookedTransactions")),
               column(12, offset=0, HTML("<br>Total amount of money propagated by transactions for loan: <b>$", round(txMoneyFlow, 2), "</b>")),
               column(12, offset=0, HTML("<br>Total amount of encumbered debt propagated by transactions for loan: <b>$", round(txEncumberedValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Median transaction value propagated for this loan: <b>$", round(txMedianValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Average transaction value propagated for this loan: <b>$", round(txMeanValue, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Assets</h3>"),
               HTML("<i>After the transactions settle, each agent is left with not one but two assets, the first from buying the underlying loan at a given interest and collateral rate, and a second from selling the loan at another combination of rates. <br>
                        We collapse these two assets and show the effective interest, collateral, and risk-adjusted return the agent now enjoys on the underyling loan.</i><br><br>
                        It is here that the magic of financial markets to spread risk becomes apparent; the effective risk-adjusted interest rate for all lenders increases as the loan, and thus the risk, propagates over the network.<br><br>
                     NOTE: In this particular instance, you have only just joined the network and requested a loan for yourself, you have played no part in any loan propagation as a neighbour or third party, hence you have gained no additional assets.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourAssets"))
        ),
        column(12, offset=1,
               HTML("<h3>Liabilities</h3>"),
               HTML("<i>In this table, we see details of liabilities you have accumulated throughout from either (1) requesting a loan for yourself (2) additional collateral pledged when reselling debt to others.</i><br><br>
                    NOTE: the amounts that are owed upon default are encumbered and returned to the agent when the underlying loan has been repaid.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourLiabilities")),
               column(12, offset=0, HTML("<br>Total amount of liabilities outstanding: <b>$", round(liabilityMoneyFlow, 2), "</b><br><br>"))
        ),
        column(12, offset=1,
               HTML("<h3>Your Network Impact</h3>"),
               HTML("<br>Here we visualize how your loan propagates throughout the network.<br><br>
                    Each node in the below is an active participant in the loan propagation, with you being the red node.<br><br>
                    The thickness/radius of an arrow/node indicates amount of debt transferred/retained, while the colour [ranging from blue to green] of the arrow/node represents the risk-adjusted return of the debt transferred/retained.<br><br><br>")
        ),
        column(12, offset=1, ndtv::ndtvAnimationWidgetOutput("networkMovie", width = "800px", height = "400px")),
        HTML("<br><br>"),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage2', "Previous")), column(1, offset=0, actionButton('stage4', label='Proceed'))))
        
    )
    
    )
  
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})


observeEvent(input$backToStage2, {
  js$redirect("?demo_2")
})

observeEvent(input$stage4, {
  js$redirect("?demo_4")
})
