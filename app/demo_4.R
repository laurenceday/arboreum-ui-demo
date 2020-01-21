import(dplyr)
import(network)
import(modules)
import(plotly)

ntwkPlot            <- modules::use(here::here("/src/NtwkPlot.R"))

postBrwNetwork      <- readRDS(here::here("/pregenerated/NeighborLoanNetwork.rds"))
postBrwTransactions <- readRDS(here::here("/pregenerated/NeighborLoanTransactions.rds"))

networkMovie        <- readRDS(here::here("/pregenerated/NeighborNetworkMovie.rds"))

output$networkMovie <- ndtv::renderNdtvAnimationWidget(networkMovie)
##Calculate Asset Changes
f <- function(df){
  tot.risk <- with(df,sum(Amt.C-Amt.S+ifelse(is.na(Scrt.S),0,(Scrt.S-Scrt.C)*Amt.S)))
  tot.prft <- with(df,sum((Amt.C-Amt.S)*(Rate.C-1)+ifelse(is.na(Rate.S),0,Amt.S*(Rate.C-Rate.S))))
  tot.scrt <- with(df,sum((Amt.C-Amt.S)*Scrt.C-ifelse(is.na(Scrt.S),0,Amt.S*(Scrt.S-Scrt.C))))            
  
  AmtLent = with(df,round(sum(Amt.C-Amt.S),2))
  LendRate = with(df,stats::weighted.mean(Rate.C,Amt.C)-1)
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
  mutate(to=paste0('Node ',to)) %>% select(to,brw.conf,AmtLent,LendRate,eff.rate,eff.scrt,eff.rar)

names(postBrwAssets)[1] <- "Node"
names(postBrwAssets)[2] <- "Loan Confidence"
names(postBrwAssets)[3] <- "Amount Loaned"
names(postBrwAssets)[4] <- "Nominal Intr. Rate"
names(postBrwAssets)[5] <- "Eff. Interest Rate"
names(postBrwAssets)[6] <- "Eff. Collateral Rate"
names(postBrwAssets)[7] <- "Eff. Risk-adj-Return"
postBrwAssets[[1]][postBrwAssets[[1]]=='Node 111'] <- 'You'

assetMoneyFlow      <- sum(postBrwAssets[[3]])

##Calculate Liability Changes
postBrwLiabilities  <- postBrwTransactions %>% filter(from==1) %>% group_by(from) %>% 
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
postBrwLiabilities[[1]][postBrwLiabilities[[1]]=='Node 111'] <- 'You'

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
postBrwTransactions[[2]][postBrwTransactions[[2]]=='Node 111'] <- 'You'
postBrwTransactions[[1]][postBrwTransactions[[1]]=='Node 1'] <- 'Mack'
postBrwTransactions[[2]][postBrwTransactions[[2]]=='Node 1'] <- 'Mack'

names(postBrwTransactions)[1] <- "Seller"
names(postBrwTransactions)[2] <- "Buyer"
names(postBrwTransactions)[3] <- "Amount"
names(postBrwTransactions)[4] <- "Interest Rate"
names(postBrwTransactions)[5] <- "Collateral Rate"
names(postBrwTransactions)[6] <- "Amount Encumbered"

##filter for only relevant transactions
postBrwTransactions <- postBrwTransactions %>% filter(Seller=='You' | Buyer=='You')
postBrwAssets <- postBrwAssets %>% filter(Node=='You') %>% select(-1)
postBrwLiabilities <- postBrwLiabilities %>% filter(Node=='You' & `Liability Type`=='Upon Default') %>% select(-1)

## render tables
output$yourAssets <- DT::renderDataTable({
  as.data.frame(postBrwAssets)
},
options = list(
  rowCallback = JS(
    "function(row, data) {",
    "var num2  = '$' + data[2].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var sec3 = Math.round(data[3] * 100).toString() + '%';",
    "var sec4 = Math.round(data[4] * 100).toString() + '%';",
    "var sec5 = Math.round(data[5] * 100).toString() + '%';",
    "var sec6 = Math.round(data[6] * 100).toString() + '%';",
    "$('td:eq(2)', row).html(num2);", 
    "$('td:eq(3)', row).html(sec3);",
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
    "var num1  = '$' + data[1].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var rate2 = (data[2] * 100).toFixed(3).toString() + '%';",
    "var sec3 = (Math.round(data[3] * 100).toFixed(2)).toString() + '%';",
    "$('td:eq(1)', row).html(num1);", 
    "$('td:eq(2)', row).html(rate2);",
    "$('td:eq(3)', row).html(sec3);",
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
    "$('td:eq(3)', row).html(num3);", 
    "$('td:eq(4)', row).html(rate4);",
    "$('td:eq(5)', row).html(sec5);",
    "$('td:eq(6)', row).html(num6);",
    "}")
), callback = JS("table.order([3, 'desc']).draw();")
)

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("<h2>Scenario #2: Someone You Trust Takes Out A Loan</h2>"),
               HTML("Let's now see how you're impacted if Mack - one of your friends who you extended $325 credit to - decides that he wishes to take out a loan of $400, at a 10% interest rate and with 30% collateralisation. <br><br>
                     This time we will only show entries where you are personally affected"),
               HTML("<h3>Loan Transactions</h3><br>"),
               column(6, offset=0, DT::dataTableOutput("precookedTransactions")),
               column(12, offset=0, HTML("<br>Total amount of money propagated by transactions for loan: <b>$", round(txMoneyFlow, 2), "</b>")),
               column(12, offset=0, HTML("<br>Total amount of encumbered debt propagated by transactions for loan: <b>$", round(txEncumberedValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Median transaction value propagated for this loan: <b>$", round(txMedianValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Average transaction value propagated for this loan: <b>$", round(txMeanValue, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Assets</h3>"),
               HTML("In this scenario, you are the holder of some of Mack's debt, which count as assets as they represent money owed to you with an associated interest rate.<br><br>
                    In the table below you can now clearly see the difference between the effective and nominal rates of interest you earn given your privileged position of being an immediate neighbor to Mack, who represents a low-risk borrower. <br><br>
                    Each individual loan entry is associated with a 'confidence' prediction - namely, the center of tendency of a distribution estimating the probability that the loan will be faithfully repaid.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourAssets")),
               column(12, offset=0, HTML("<br>Total amount of assets outstanding for everyone: <b>$", round(assetMoneyFlow, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Liabilities</h3>"),
               HTML("As before, your liabilities for this loan are shown below.<br><br>Note that in this case, the asset types are no longer 'Loan Origin', as you are not the <i>initial</i> originator of the debt.<br>
                     These entries indicate liabilities you have incurred by adding an additional layer of collateral as you propagated Mack's loan to reduce your own expose. Fortunately, they will only materialize if Mack defaults.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourLiabilities")),
               column(12, offset=0, HTML("<br>Total amount of liabilities outstanding for everyone: <b>$", round(liabilityMoneyFlow, 2), "</b><br><br>"))
        ),
        column(12, offset=1,
                HTML("<h3>Mack's Network Impact</h3>"),
                HTML("<br>Here we visualize how Mack's loan (Mack is depicted by the red dot) propagates throughout the network.<br><br>")
        ),
        column(12, offset=1, ndtv::ndtvAnimationWidgetOutput("networkMovie", width = "800px", height = "400px")),
        HTML("<br><br>"),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage3', "Previous")), column(1, offset=1, actionButton('stage5', label='Proceed')))
        )
      ))
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})


observeEvent(input$backToStage3, {
  js$redirect("?demo_3")
})

observeEvent(input$stage5, {
  js$redirect("?demo_5")
})

