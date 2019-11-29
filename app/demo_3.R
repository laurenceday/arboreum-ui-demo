library(dplyr)
library(networkD3)
library(network)

postBrwTransactions <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/PostLoanTransactions.rds"))
postBrwNetwork      <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/PostLoanNetwork.rds"))

postBrwAssets <- postBrwNetwork$val[[111]]$Assets
postBrwLiabilities <- postBrwNetwork$val[[111]]$Liabilities

output$yourAssets <- DT::renderDataTable({
  as.data.frame(postBrwAssets)
})

output$yourLiabilities <- DT::renderDataTable({
  as.data.frame(postBrwLiabilities)
})

output$precookedTransactions <- DT::renderDataTable({
  as.data.frame(postBrwTransactions)
})

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(6, offset=1,
               HTML("<h3>What happened to your loan?</h3>"),
               HTML("Let's see how Arboreum has distributed your loan.<br>As you can see, your immediate peers have taken on the highest risk, but also make the highest return."),
               HTML("<h4>Your Loan Terms</h4>"),
               textOutput("chosenLoan"),
               HTML("<h3>Loan Transactions</h3>"),
               DT::dataTableOutput("precookedTransactions"),
               HTML("<h4>Your Balance Sheet</h4>"),
               HTML("<h3>Assets</h3>"),
               DT::dataTableOutput("yourAssets"),
               HTML("<h3>Liabilities</h3>"),
               DT::dataTableOutput("yourLiabilities")),
        column(4, offset=0,
               HTML("<h3>Your Network Impact</h3>")
        )
      )
    )
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})

