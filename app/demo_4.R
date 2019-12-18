import(dplyr)
import(network)
import(modules)
import(plotly)

ntwkPlot            <- modules::use(here::here("ShinyApps/Arboreum/app/src/NtwkPlot.R"))

postBrwNetwork      <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/NeighborLoanNetwork.rds"))
postBrwTransactions <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/NeighborLoanTransactions.rds"))

networkConstruction <- ntwkPlot$plot.D3(postBrwNetwork, 1, postBrwTransactions)
saveRDS(networkConstruction, here::here("ShinyApps/Arboreum/app/pregenerated/NeighborNetworkInteractive.rds"), version = 2)
networkMovie        <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/NeighborNetworkMovie.rds"))

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
names(postBrwTransactions)[6] <- "Amount Encumbered"

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
postBrwLiabilities  <- postBrwLiabilities %>% filter(amount > 0)
liabilityMoneyFlow  <- sum(postBrwLiabilities$amount)
names(postBrwLiabilities)[1] <- "Borrower"
names(postBrwLiabilities)[2] <- "Intermediary"
names(postBrwLiabilities)[3] <- "Lender"
names(postBrwLiabilities)[4] <- "Amount Borrowed"
names(postBrwLiabilities)[5] <- "Borrowing Rate"
names(postBrwLiabilities)[6] <- "Loan Securitisation"
names(postBrwLiabilities)[7] <- "Asset Type"

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
    "var num4  = '$' + data[4].toFixed(2).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');",
    "var rate5 = (data[5] * 100 - 100).toFixed(3).toString() + '%';",
    "var sec6 = (Math.round(data[6] * 100).toFixed(2)).toString() + '%';",
    "$('td:eq(4)', row).html(num4);",
    "$('td:eq(5)', row).html(rate5);",
    "$('td:eq(6)', row).html(sec6);",
    "}")
), callback = JS("table.order([4, 'desc']).draw();")
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
               HTML("<h2>Scenario #2: Someone You Trust Takes Out A Loan</h2>"),
               HTML("Let's now see how you're impacted if Mack - one of your friends who you extended USD325 credit to - decides that he wishes to take out a loan of USD400, at a 10% interest rate and with 30% collateralisation."),
               HTML("<h3>Loan Transactions</h3><br>"),
               column(6, offset=0, DT::dataTableOutput("precookedTransactions")),
               column(12, offset=0, HTML("<br>Total amount of money propagated by transactions for loan: <b>$", round(txMoneyFlow, 2), "</b>")),
               column(12, offset=0, HTML("<br>Total amount of encumbered debt propagated by transactions for loan: <b>$", round(txEncumberedValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Median transaction value propagated for this loan: <b>$", round(txMedianValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Average transaction value propagated for this loan: <b>$", round(txMeanValue, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Assets</h3>"),
               HTML("In this scenario, you are the holder of some of Mack's debt, which count as assets as they represent money owed to you with an associated interest rate.<br><br>In the table below you can now see the amounts owed to you. Note that even though all borrowers are Mack, there are multiple entries - this is because<br>you are also the holder of debt propagated by other members of the network which you agree to take on.<br><br>Each individual loan entry is associated with a 'confidence' level - namely, an estimated probability of that loan entry being faithfully redeemed.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourAssets")),
               column(12, offset=0, HTML("<br>Total amount of assets outstanding: <b>$", round(assetMoneyFlow, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Liabilities</h3>"),
               HTML("As before, your liabilities for this loan are shown below.<br><br>Note that in this case, the asset types are no longer 'loan-origin', as you are not the <i>initial</i> originator of the debt.<br><br>These entries indicate debt which you have propagated through the network to disperse your own exposure.<br>The disparity between your assets and liabilities in this case is a function of your <i>trust</i> in Mack.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourLiabilities")),
               column(12, offset=0, HTML("<br>Total amount of liabilities outstanding: <b>$", round(liabilityMoneyFlow, 2), "</b><br><br>"))
        ),
        column(12, offset=1,
                HTML("<h3>Network Impact</h3>"),
                HTML("<br>Below is a visualisation of the impact of Mack's loan being propagated through the network.<br><br>")
        ),
        column(12, offset=1, ndtv::ndtvAnimationWidgetOutput("networkMovie", width = "800px", height = "400px")),
        HTML("<br><br>"),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage3', "Previous")), column(1, offset=1, actionButton('stage5', label='Proceed')))
        )
      ))
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})


observeEvent(input$backToStage3, {
  js$redirect("?demo_3")
})

observeEvent(input$stage5, {
  js$redirect("?demo_5")
})

