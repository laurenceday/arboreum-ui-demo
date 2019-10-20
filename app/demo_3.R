library(dplyr)
library(networkD3)
library(network)

### om_skeleton profile.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

source(here::here("app/src/Generate.R"))
source(here::here("app/src/Propagate.R"))
source(here::here("app/src/Traverse.R"))

postBrw.ntwk <- readRDS(here::here("app/tmp/precookedFwdProp.rds"))
precookedFwdPropTxes <- postBrw.ntwk$transactions
precookedFwdPropNtwk <- postBrw.ntwk$ntwk

yourPortfolio <- precookedFwdProp$val[[1]]$Portfolio

output$yourAssets <- DT::renderDataTable({
  as.data.frame(yourPortfolio)
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
               HTML("<h4>Your Balance Sheet</h4>"),
               fluidRow(column(3, offset=0, renderTable(yourAssets)), column(3, offset=0, renderTable(yourLiabilities))),
               fluidRow(column(3, offset=0, actionButton('backToStage2', "Previous")), column(3, offset=0, actionButton('stage4', label='Proceed')))
        ),
        column(4, offset=0,
               HTML("<h3>Your Network Impact</h3>")
        )
      )
    )
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})

