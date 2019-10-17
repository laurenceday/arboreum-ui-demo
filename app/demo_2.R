library(dplyr)
library(networkD3)
library(network)

### om_skeleton profile.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

output$loanGrid      <- DT::renderDataTable(as.data.frame(matrix(0, ncol = 10, nrow = 10)), selection=list(target='cell'))

source(here::here("app/src/Generate.R"), local=TRUE)
source(here::here("app/src/Traverse.R"), local=TRUE)
 
initialisedNetwork <- buildCorePeri(N = 50, K = 5)
initialisedSheets  <- suppressWarnings(initializeSheets(initialisedNetwork, K = 5, A0 = 10000))

baseNetwork        <- initialisedSheets[[2]]
rsltCalc <- (calcRiskArray(baseNetwork))

session$userData$computedLoan      <- FALSE
session$userData$selectedLoan      <- FALSE
session$userData$minLoanAmount     <- 0
session$userData$maxInterestRate   <- 0
session$userData$maxCollateralRate <- 0

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
               fluidRow(column(6, offset=1, actionButton('computeBackprop', label="Calculate Available Loans")),column(6, offset=1, "NOTE: This will take some time."))
        ),
        column(6, offset=1,
               HTML("<h3>Step 2: Choose Your Loan</h3>"),
               DT::dataTableOutput("loanGrid"),
               actionButton('stage3', label='Proceed')
        )
      )
    )
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})

observeEvent(input$computeBackprop, {
  showNotification("Calculating... this may take some time - go get a coffee.", type="warning", duration=30)
  session$userData$minLoanAmount     <- input$minLoanAmount
  session$userData$maxInterestRate   <- input$maxInterestRate
  session$userData$maxCollateralRate <- input$maxCollateralRate
  
  session$userData$initialisedNetwork <- buildCorePeri(N = 100, K = 40)
  session$userData$initialisedSheets  <- suppressWarnings(initializeSheets(session$userData$initialisedNetwork, K = 40, A0 = 10000))
  
  ntwkData   <- session$userData$initialisedNetwork[["mel"]]
  source     <- sapply(ntwkData, function(x) x[["inl"]])
  target     <- sapply(ntwkData, function(x) x[["outl"]])
  zeroValues <- replicate(length(source), 0)
  ntwkData   <- data.frame(source, target, zeroValues)
  
  session$userData$simpleNetwork <- renderSimpleNetwork({
    suppressWarnings(simpleNetwork(ntwkData, height = 1600, width = 1600, zoom = TRUE, charge = -300))
  })
  
  baseNetwork        <- session$userData$initialisedSheets[[2]]
  session$userData$rsltCalc <- suppressWarnings(calcRiskArray(baseNetwork))
  output$loanGrid <- DT::renderDataTable(
    (session$userData$rsltCalc[[1]])[1:140, 1:140, 1]
    )
})

observeEvent(input$stage3, {
  if (session$userData$computedLoan && session$userData$selectedLoan) {js$redirect("?demo_3")} else { showNotification("You have either not yet computed a loan, or not selected your desired offered loan.", type="error") }
})


