import(dplyr)
import(network)
import(modules)
import(plotly)
import(here)
import(ndtv)

ntwkPlot            <- modules::use(here::here("ShinyApps/Arboreum/app/src/NtwkPlot.R"))

postBrwTransactions <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/LoanTransactions.rds"))
postBrwNetwork      <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/LoanNetwork.rds"))

networkConstruction <- ntwkPlot$plot.D3(postBrwNetwork,  111, postBrwTransactions)
saveRDS(networkConstruction, here::here("ShinyApps/Arboreum/app/pregenerated/NetworkInteractive.rds"), version = 2)
networkMovie        <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/NetworkMovie.rds"))

output$networkMovie <- ndtv::renderNdtvAnimationWidget(networkMovie)

txMoneyFlow         <- sum(postBrwTransactions$Amt.C)
txMedianValue       <- median(postBrwTransactions$Amt.C)
txMeanValue         <- mean(postBrwTransactions$Amt.C)
postBrwTransactions[is.na(postBrwTransactions)] <- 0
postBrwTransactions$Amount.Retained <- postBrwTransactions$Amt.C - postBrwTransactions$Amt.S #calculates amount encumbered (ToDo: Ensure right formula)
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

output$pageStub <- renderUI({rv$limn;
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("<h2>Scenario #1: You Take Out A Loan</h2>"),
               HTML("<h4><b>Assume you requested a loan for USD1020, at 8% interest and 22% securitisation.</b></h4>"),
               HTML("Let's further assume that everything was approved, and your chosen loan amount has been granted on the terms that you've selected.<br>Let's have a look at a breakdown of how Arboreum has propagated your loan across the network.<br><br>As you can see, your immediate peers (those who have directly trusted you with credit) have taken on the highest risk, but<br> by virtue of the fact that they are doing so, they also stand to make the highest return."),
               HTML("<h3>Loan Transactions</h3>"),
               HTML("<i>Below is a table of every transaction which took place during the distribution of your loan.</i><br><br>In short, your peers contribute an amount of the loan principal that their agent has determined they are<br>comfortable with, and then sold their debt on to their own peers so as to minimise their own exposure.<br><br>A transaction chain following such a path terminates when a participant cannot find a peer of their own willing to participate.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("precookedTransactions")),
               column(12, offset=0, HTML("<br>Total amount of money propagated by transactions for loan: <b>$", round(txMoneyFlow, 2), "</b>")),
               column(12, offset=0, HTML("<br>Total amount of encumbered debt propagated by transactions for loan: <b>$", round(txEncumberedValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Median transaction value propagated for this loan: <b>$", round(txMedianValue, 2), "</b>")),
               column(12, offset=0, HTML("<br>Average transaction value propagated for this loan: <b>$", round(txMeanValue, 2), "</b>"))
        ),
        column(12, offset=1,
               HTML("<h3>Assets</h3>"),
               HTML("<i>This table would typically contain the details of debt which you have yourself purchased from peers.</i><br><br>However, in this particular instance, you have only just joined the network and requested a loan for yourself, and as such<br>have played no part in any loan propagation as a neighbour or third party, so this table is empty.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourAssets"))
        ),
        column(12, offset=1,
               HTML("<h3>Liabilities</h3>"),
               HTML("<i>In this table, we see details of debt you have accumulated throughout the process of the propagation of this loan.</i><br><br>It's particularly important to note that you may not necessarily be currently encumbered by these liabilities [as you may have sold debt to your neighbours], but<br>instead represent monies due under the assumption that all loans are faithfully repaid.<br><br>"),
               column(6, offset=0, DT::dataTableOutput("yourLiabilities")),
               column(12, offset=0, HTML("<br>Total amount of liabilities outstanding: <b>$", round(liabilityMoneyFlow, 2), "</b><br><br>"))
        ),
        column(12, offset=1,
               HTML("<h3>Your Network Impact</h3>"),
               HTML("<br>Below is a visualisation of the impact your loan made on the network.<br><br>
                    Each node in the below is an active participant in the loan propagation. The thickness of an arrow indicates amount of debt transferred, the<br>size of a node represents their assets, and the colour [ranging from red through blue to green] represent the overall interest rate of their liabilities.<br><br><br>")
        ),
        column(12, offset=1, ndtv::ndtvAnimationWidgetOutput("networkMovie", width = "800px", height = "400px")),
        HTML("<br><br>"),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage2', "Previous")), column(1, offset=0, actionButton('stage4', label='Proceed'))))
        
    )
    
    )
  }
  
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})


observeEvent(input$backToStage2, {
  js$redirect("?demo_2")
})

observeEvent(input$stage4, {
  js$redirect("?demo_4")
})
