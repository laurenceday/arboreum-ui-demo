import(dplyr)
import(network)
import(modules)
import(plotly)

ntwkPlot            <- modules::use(here::here("src/NtwkPlot.R"))

postBrwTransactions <- readRDS(here::here("pregenerated/ThirdPartyLoanTransactions.rds"))
postBrwNetwork      <- readRDS(here::here("pregenerated/ThirdPartyLoanNetwork.rds"))

postBrwNetwork_Mack      <- readRDS(here::here("/pregenerated/NeighborLoanNetwork.rds"))
postBrwTransactions_Mack <- readRDS(here::here("/pregenerated/NeighborLoanTransactions.rds"))

postBrwTransactions_Self <- readRDS(here::here("pregenerated/LoanTransactions.rds"))
postBrwNetwork_Self      <- readRDS(here::here("pregenerated/LoanNetwork.rds"))

networkMovie        <- readRDS(here::here("pregenerated/ThirdPartyNetworkMovie.rds"))

output$networkMovie <- ndtv::renderNdtvAnimationWidget(networkMovie)
##Calculate Asset Changes
f <- function(df,ntwk){
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
  
  brw.conf <- round(ntwk$val[[unique(df$to)]]$Subj.risk %>% filter(to==unique(df$brw)) %>% dplyr::select(Risk.coef),4)
  
  return(as.data.frame(list('brw.conf'=brw.conf[[1]],'AmtLent'=AmtLent,'LendRate'=LendRate,'ScrtRate'=ScrtRate,
                            'AmtEncumb'=AmtEncumb,'eff.rate'=eff.rate,'eff.scrt'=eff.scrt,'eff.rar'=eff.rar)))
}

postBrwAssets <- postBrwTransactions %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>% 
                  do(f(.,postBrwNetwork)) %>% ungroup() %>% replace(.,is.na(.),0) %>%
                  dplyr::select(to,brw.conf,AmtLent,LendRate,eff.rate,eff.scrt,eff.rar)
                  
assetMoneyFlow  <- sum(postBrwAssets[[3]])

postBrwAssets <- bind_rows(postBrwAssets %>% mutate(Borrower='Farmer'),
                           postBrwTransactions_Mack %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>% 
                           do(f(.,postBrwNetwork_Mack)) %>% ungroup() %>% replace(.,is.na(.),0) %>% mutate(Borrower='Mack') %>%
                           dplyr::select(to,brw.conf,AmtLent,LendRate,eff.rate,eff.scrt,eff.rar,Borrower)) %>%
                 mutate(to=paste0('Node ',to)) %>% filter(to=='Node 111') %>% dplyr::select(-1) %>%
                 dplyr::select(Borrower,brw.conf,AmtLent,LendRate,eff.rate,eff.scrt,eff.rar)

names(postBrwAssets)[2] <- "Loan Confidence"
names(postBrwAssets)[3] <- "Amount Loaned"
names(postBrwAssets)[4] <- "Nominal Intr. Rate"
names(postBrwAssets)[5] <- "Eff. Interest Rate"
names(postBrwAssets)[6] <- "Eff. Collateral Rate"
names(postBrwAssets)[7] <- "Eff. Risk-adj-Return"


##Calculate Liability Changes
postBrwLiabilities  <- postBrwTransactions %>% filter(from==69) %>% group_by(from) %>% 
                        summarise(AmtEncumb=sum(Amt.C),Rate=mean(Rate.C), Scrt=mean(Scrt.C),Type='Loan Origin') %>%
                        mutate(to='You') %>% dplyr::select(to,AmtEncumb,Rate,Scrt,Type) %>%
                        bind_rows(.,postBrwTransactions %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>% 
                                    do(f(.,postBrwNetwork)) %>% ungroup() %>% replace(.,is.na(.),0) %>%
                                    mutate(to=paste0('Node ',to),Rate=0,Scrt=0,Type='Upon Default') %>% 
                                    dplyr::select(to,AmtEncumb,Rate,Scrt,Type))

liabilityMoneyFlow  <- sum(postBrwLiabilities[[2]])

postBrwLiabilities  <- postBrwTransactions_Self %>% filter(from==111) %>% group_by(from) %>% 
                       summarise(AmtEncumb=sum(Amt.C),Rate=mean(Rate.C), Scrt=mean(Scrt.C),Type='Loan Origin') %>%
                       mutate(to='Node 111',Borrower='Self') %>% dplyr::select(to,Borrower,AmtEncumb,Rate,Scrt,Type) %>%
                        bind_rows(.,postBrwTransactions %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>%
                                    do(f(.,postBrwNetwork)) %>% ungroup() %>% replace(.,is.na(.),0) %>%
                                    mutate(to=paste0('Node ',to),Borrower='Farmer',Rate=0,Scrt=0,Type='Upon Default') %>%
                                    dplyr::select(to,Borrower,AmtEncumb,Rate,Scrt,Type)) %>%
                       bind_rows(.,postBrwTransactions_Mack %>% replace(.,is.na(.),0) %>% group_by(brw,to) %>%
                                   do(f(.,postBrwNetwork_Mack)) %>% ungroup() %>% replace(.,is.na(.),0) %>%
                                   mutate(to=paste0('Node ',to),Borrower='Mack',Rate=0,Scrt=0,Type='Upon Default') %>%
                                   dplyr::select(to,Borrower,AmtEncumb,Rate,Scrt,Type)) %>%
                      filter(to=='Node 111') %>%
                       dplyr::select(-1)

names(postBrwLiabilities)[2] <- "Amount"
names(postBrwLiabilities)[3] <- "Interest Rate"
names(postBrwLiabilities)[4] <- "Collateral Rate"
names(postBrwLiabilities)[5] <- "Liability Type"

rm(postBrwTransactions_Mack,postBrwNetwork_Mack,postBrwNetwork_Self,postBrwTransactions_Self,postBrwNetwork)


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
postBrwTransactions[[1]][postBrwTransactions[[1]]=='Node 69'] <- 'Farmer'
postBrwTransactions[[2]][postBrwTransactions[[2]]=='Node 69'] <- 'Farmer'

names(postBrwTransactions)[1] <- "Seller"
names(postBrwTransactions)[2] <- "Buyer"
names(postBrwTransactions)[3] <- "Amount"
names(postBrwTransactions)[4] <- "Interest Rate"
names(postBrwTransactions)[5] <- "Collateral Rate"
names(postBrwTransactions)[6] <- "Amount Encumbered"

##filter for only relevant transactions
postBrwTransactions <- postBrwTransactions %>% filter(Seller=='You' | Buyer=='You')

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
    "var sec7 = Math.round(data[7] * 100).toString() + '%';",
    "$('td:eq(3)', row).html(num3);", 
    "$('td:eq(4)', row).html(sec4);",
    "$('td:eq(5)', row).html(sec5);",
    "$('td:eq(6)', row).html(sec6);",
    "$('td:eq(7)', row).html(sec7);",
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
               HTML("<h2>Scenario #3: Someone Indirectly Connected To You Takes Out A Loan</h2>"),
               HTML("Next, let's see how you're impacted if a network participant who you do not directly trust, but you are nonetheless connected to via someone who you DO trust, requests a loan.<br>
                     In the below, a random participant (Farmer) has requested a loan of $150, at an interest rate of 10% and a 20% securitisation rate.<br><br>
                     In the below assets and liabilities tables we depict the final state of the network after incurring all three scenarios."),
               HTML("<h3>Loan Transactions</h3><br>"),
               HTML("<b>Note: table currently not fully reflective - debugging underway</b><br>"),               
               column(6, offset=0, DT::dataTableOutput("precookedTransactions")),
               column(12, offset=0, HTML("<br>Total amount of money propagated by transactions for loan: <b>$", round(txMoneyFlow, 2), "</b>")),
               column(12, offset=0, HTML("<br>Total amount of encumbered debt propagated by transactions for loan: <b>$", round(txEncumberedValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Median transaction value propagated for this loan: <b>$", round(txMedianValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Average transaction value propagated for this loan: <b>$", round(txMeanValue, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Assets</h3>"),
               HTML("As before, the table below details loans which you have yourself purchased from neighbours.<br><br>
                    Observe that even though the loan is for the same amount as that asked by Mack, you are only taking on debt from intermediate network participants, and hence your overall exposure to this loan is much lower.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourAssets")),
               column(12, offset=0, HTML("<br>Total network amount of assets outstanding for this loan: <b>$", round(assetMoneyFlow, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Liabilities</h3>"),
               HTML("As before, the table below details loans which you have yourself purchased from neighbours.<br><br>
                     For the same reason as given for the assets above, your total liability for this loan is much lower than it was for when Mack requested the loan, as you don't directly trust the borrower even though your <i>Loan Confidence</i> is higher in this case.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourLiabilities")),
               column(12, offset=0, HTML("<br>Total network amount of liabilities outstanding for this loan: <b>$", round(liabilityMoneyFlow, 2), "</b><br><br>"))
        ),
        column(12, offset=1,
               HTML("<h3>Network Impact</h3>"),
               HTML("<br>As before, below we see the propagation of the loan through the network.<br><br>")
        ),
        column(12, offset=1, ndtv::ndtvAnimationWidgetOutput("networkMovie", width = "800px", height = "400px")),
        HTML("<br><br>"),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage4', "Previous")), column(1, offset=1, actionButton('stage6', label='Proceed')))
        )
      ))
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})


observeEvent(input$backToStage4, {
  js$redirect("?demo_4")
})

observeEvent(input$stage6, {
  js$redirect("?demo_6")
})
