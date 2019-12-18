import(dplyr)
import(network)
import(modules)
import(plotly)

ntwkPlot            <- modules::use(here::here("ShinyApps/Arboreum/app/src/NtwkPlot.R"))

postBrwTransactions <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyLoanTransactions.rds"))
postBrwNetwork      <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyLoanNetwork.rds"))

networkConstruction <- ntwkPlot$plot.D3(postBrwNetwork, 69, postBrwTransactions)
saveRDS(networkConstruction, here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyNetworkInteractive.rds"), version = 2)
networkMovie        <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyNetworkMovie.rds"))

output$networkMovie <- ndtv::renderNdtvAnimationWidget(networkMovie)

txMoneyFlow         <- sum(postBrwTransactions$Amt.C)
txMedianValue       <- median(postBrwTransactions$Amt.C)
txMeanValue         <- mean(postBrwTransactions$Amt.C)
postBrwTransactions[is.na(postBrwTransactions)] <- 0
postBrwTransactions$Amount.Retained <- postBrwTransactions$Amt.C - postBrwTransactions$Amt.S
txEncumberedValue   <- sum(postBrwTransactions$Amt.C) - sum(postBrwTransactions$Amt.S, na.rm = TRUE)
postBrwTransactions <- subset(postBrwTransactions, select = -c(6, 7, 8, 9))
names(postBrwTransactions)[1] <- "Borrower"
names(postBrwTransactions)[2] <- "Lender"
names(postBrwTransactions)[3] <- "Amount Lent"
names(postBrwTransactions)[4] <- "Lending Rate"
names(postBrwTransactions)[5] <- "Loan Securitisation"

postBrwAssets       <- postBrwNetwork$val[[111]]$Assets
postBrwAssets       <- subset(postBrwAssets, select = -c(2, 3))
postBrwAssets       <- postBrwAssets %>% filter(rate > 0 & security > 0)
assetMoneyFlow      <- sum(postBrwAssets$amount)
names(postBrwAssets)[1] <- "Borrower"
names(postBrwAssets)[2] <- "Loan Confidence"
names(postBrwAssets)[3] <- "Amount Loaned"
names(postBrwAssets)[4] <- "Lending Rate"
names(postBrwAssets)[5] <- "Loan Securitisation"
names(postBrwAssets)[6] <- "Asset Type"

postBrwLiabilities  <- postBrwNetwork$val[[111]]$Liabilities
postBrwLiabilities  <- subset(postBrwLiabilities, select = -c(1, 2))
postBrwLiabilities  <- postBrwLiabilities %>% filter(amount > 0)
liabilityMoneyFlow  <- sum(postBrwLiabilities$amount)
names(postBrwLiabilities)[1] <- "Lender"
names(postBrwLiabilities)[2] <- "Amount Borrowed"
names(postBrwLiabilities)[3] <- "Borrowing Rate"
names(postBrwLiabilities)[4] <- "Loan Securitisation"
names(postBrwLiabilities)[5] <- "Asset Type"

output$yourAssets <- DT::renderDataTable({
  as.data.frame(postBrwAssets)
},
options = list(
  rowCallback = JS(
    "function(row, data) {",
    "var conf2 = Math.round(data[2] * 100).toString() + '%';",
    "var num3  = '$' + data[3].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var rate4 = ((data[4] * 100 - 100).toFixed(3)).toString() + '%';",
    "var sec5 = Math.round(data[5] * 100).toString() + '%';",
    "$('td:eq(2)', row).html(conf2);",
    "$('td:eq(3)', row).html(num3);",
    "$('td:eq(4)', row).html(rate4);",
    "$('td:eq(5)', row).html(sec5);",
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
    "var rate3 = (data[3] * 100 - 100).toFixed(3).toString() + '%';",
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
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("<h2>Scenario #3: Someone Indirectly Connected To You Takes Out A Loan</h2>"),
               HTML("Finally, let's see how you're impacted if a network participant who you do not directly trust, but you are nonetheless connected to via someone who<br>you DO trust, requests a loan. In the below, a random participant has requested a loan of USD150, at an interest rate of 10% and a 20% securitisation rate."),
               HTML("<h3>Loan Transactions</h3><br>"),
               column(6, offset=0, DT::dataTableOutput("precookedTransactions")),
               column(12, offset=0, HTML("<br>Total amount of money propagated by transactions for loan: <b>$", round(txMoneyFlow, 2), "</b>")),
               column(12, offset=0, HTML("<br>Total amount of encumbered debt propagated by transactions for loan: <b>$", round(txEncumberedValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Median transaction value propagated for this loan: <b>$", round(txMedianValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Average transaction value propagated for this loan: <b>$", round(txMeanValue, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Assets</h3>"),
               HTML("As before, the table below details loans which you have yourself purchased from neighbours.<br><br>Observe that even though the loan is for the same amount as that asked by Mack, you are only taking on<br>debt from intermediate network participants, and hence your overall exposure to this loan is much lower.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourAssets")),
               column(12, offset=0, HTML("<br>Total amount of assets outstanding: <b>$", round(assetMoneyFlow, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Liabilities</h3>"),
               HTML("As before, the table below details loans which you have yourself purchased from neighbours.<br><br>For the same reason as given for the assets above, your total liability for this loan is much lower than it was for when<br>Mack requested the loan, as you don't directly trust the borrower.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourLiabilities")),
               column(12, offset=0, HTML("<br>Total amount of liabilities outstanding: <b>$", round(liabilityMoneyFlow, 2), "</b><br><br>"))
        ),
        column(12, offset=1,
               HTML("<h3>Network Impact</h3>"),
               HTML("<br>As before, below we see the propagation of the loan through the network.<br><br>")
        ),
        column(12, offset=1, ndtv::ndtvAnimationWidgetOutput("networkMovie", width = "800px", height = "400px")),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage4', "Previous")), column(1, offset=1, actionButton('stage6', label='Proceed')))
        )
      ))
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})


observeEvent(input$backToStage4, {
  js$redirect("?demo_4")
})

observeEvent(input$stage6, {
  js$redirect("?demo_6")
})
