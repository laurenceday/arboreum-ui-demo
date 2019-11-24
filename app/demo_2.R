library(dplyr)
library(networkD3)
library(network)

### om_skeleton profile.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

source(here::here("app/src/Generate.R"))
source(here::here("app/src/Propagate.R"))
source(here::here("app/src/Traverse.R"))

session$userData$usingPrecooked <- FALSE

# userGeneratedNetwork <- readRDS(here::here("app/tmp/userGeneratedNetwork.rds"))
precookedNetwork <- readRDS(here::here("app/tmp/precookedNetwork.rds"))
precookedSList   <- readRDS(here::here("app/tmp/precookedSList.rds"))
load(here::here("app/src/DemoTable.rda")) # The pre-cooked back-propagated network, loads in z.loess

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("Now, let us assume that you would like to take out a loan for some purpose. Let's follow how this process works."),
               HTML("<h3>Step 1: Select Your Parameters</h3>"),
               numericInput('minLoanAmount', label="Minimum Loan Amount", value = 0, max = 10000),
               numericInput('maxInterestRate', label="Maximum Interest Rate", value = 0, max = 25),
               numericInput('maxCollateralRate', label="Maximum Collateral Percentage", value = 0, max = 8000),
               fluidRow(column(6, offset=1, actionButton('computeBackprop', label="Calculate Available Loans")),column(6, offset=1, "NOTE: This will take some time.")),
               fluidRow(column(6, offset=1, actionButton('usePrecooked', label="Choose From A Demo Table")),column(6, offset=1, "This is a pre-cooked version!"))
        ),
        column(6, offset=1,
               HTML("<h3>Step 2: Choose Your Loan</h3>"),
               DT::dataTableOutput("loanGrid"),
               textOutput("loanDetails"),
               fluidRow(column(3, offset=0, actionButton('backToStage1', "Previous")), column(3, offset=0, actionButton('stage3', label='Proceed')))
        )
      )
    )
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})

observeEvent(input$computeBackprop, {
  showNotification("Calculating... this may take some time - go get a coffee.", type="warning", duration=30)
  session$userData$minLoanAmount      <- input$minLoanAmount
  session$userData$maxInterestRate    <- input$maxInterestRate
  session$userData$maxCollateralRate  <- input$maxCollateralRate
  
  session$userData$propNetwork        <- session$userData$riskArray[[2]]
  
  session$userData$loanTable          <- loan.backProp(session$userData$propNetwork, 1, algorithm ="NLOPT_GN_ISRES", browse = FALSE,
                                                       controls = list(controls = list(xtol_rel = 0.1,
                                                                                       xtol_abs = c(rep(0.1,2),0.01,0.01),
                                                                                       relax = FALSE, maxeval = 1000,
                                                                                       risk.coef = 'Bernoulli',
                                                                                       span = 0.5)))
  saveRDS(session$userData$loanTable, here::here("app/tmp/myLoanTable.rds"))
  session$userData$Slist              <- session$userData$loanTable$S.list

  # Ideally we want to have, instead of a data frame, a selection of the lowest rate/securitisation combinations per
  #   integer around the amount that you want to borrow: perform a floor on each element, then a map getIndex, then 
  #   hoist out the 'minimum' of each set. Boom, you have a list of options rather than a table.
  output$loanGrid <-  DT::renderDataTable(
    as.data.frame(round(matrix(predict(session$userData$loanTable$root.S, expand.grid(R = seq(1.01, 2.5,0.01), Z = seq(0.01, 0.99,0.01))),
                             length(seq(1.01, 2.5,0.01)), length(seq(0.01, 0.99,0.01))), 0), colnames = seq(1, 99,0.01), selection=list(target='cell'))
  )
  session$userData$computedLoan <- TRUE
  
})

observeEvent(input$usePrecooked, {
  session$userData$loanTable <- round(z.loess, 0)
  output$loanGrid <- DT::renderDataTable(as.data.frame(session$userData$loanTable), colnames = seq(1, 99, 1), selection=list(target='cell'))
  session$userData$computedLoan <- TRUE
  session$userData$usingPrecooked <- TRUE
})

observeEvent(input$loanGrid_cells_selected, {
    if (length(input$loanGrid_cells_selected) == 0) {
      session$userData$selectedLoan <- FALSE
    } else {
      if (length(input$loanGrid_cells_selected == 1)) {
          session$userData$interestRate    <- input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), 1]
          session$userData$collateralRate  <- input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), ncol(input$loanGrid_cells_selected)]
          session$userData$loanAmount      <- session$userData$loanTable[input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), 1], input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), ncol(input$loanGrid_cells_selected)]]
          write(input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), 1], here::here("app/tmp/interestRate.rds"))
          write(input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), ncol(input$loanGrid_cells_selected)], here::here("app/tmp/securitisationRate.rds"))
          write(session$userData$loanTable[input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), 1], input$loanGrid_cells_selected[nrow(input$loanGrid_cells_selected), ncol(input$loanGrid_cells_selected)]], here::here("app/tmp/loanAmount.rds"))
          session$userData$selectedLoan <- TRUE
          output$loanDetails <- renderText({paste0("You wish to borrow $", round(session$userData$loanAmount, 2), " at an interest rate of ", session$userData$interestRate, "% and securitisation ratio of ", session$userData$collateralRate, "%. If you agree, click Proceed.")})
      } else {
        session$userData$selectedLoan <- FALSE
        showNotification("You need to choose a single loan option.", type="error")
      }
    }
})

observeEvent(input$backToStage1, {
  js$redirect("?demo_1")
})

observeEvent(input$stage3, {
  if (session$userData$computedLoan && session$userData$selectedLoan) {
      networkToPropagate <- if (session$userData$usingPrecooked) { precookedNetwork } else { session$userData$riskArray[[2]] }
      networkSList <- if (session$userData$usingPrecooked) { precookedSList } else { session$userData$Slist }
      showNotification("Propagating your loan through the network, please wait...", type="warning")
      if (!session$userData$usingPrecooked) {
          forwardNetwork <- suppressWarnings(loan.frwdProp(networkToPropagate, 1, networkSList, session$userData$loanAmount, 1 + (session$userData$interestRate/100), session$userData$collateralRate/100))
          saveRDS(forwardNetwork, here::here("app/tmp/naturalForwardProp.rds"))
      }
      js$redirect("?demo_3")
  } else { 
      showNotification("You have either not yet computed a loan, or not selected your desired offered loan.", type="error")
    }
})


