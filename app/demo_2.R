library(dplyr)
library(networkD3)
library(network)

z.loess <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/LoanFunctionMatrix.rds"))

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
               numericInput('minLoanAmount', label="Minimum Loan Amount", value = 865, max = 10000),
               numericInput('maxInterestRate', label="Maximum Interest Rate", value = 1.1, max = 25),
               numericInput('maxCollateralRate', label="Maximum Collateral Percentage", value = 0.42, max = 8000),
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
    js$redirect("?demo_3")
})


