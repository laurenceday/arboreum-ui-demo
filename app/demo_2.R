library(dplyr)
library(networkD3)
library(network)

z.loess <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/LoanFunctionMatrix.rds"))
rotate  <- function(x) t(apply(x, 2, rev))
output$loanGrid <- DT::renderDataTable(subset(as.data.frame(round(rotate(z.loess), 0)), 
                                       select=c(seq(10, 150, 10)),
                                       colnames = seq(10, 150, 10),
                                       selection=list(target='cell')))

output$proceedWarning <- renderText("")

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("Now, let us assume that you would like to take out a loan for some purpose. Let's follow how this process works.<br><br>In practice, you would provide a justification for this loan which your immediate neighbours (those who trust YOU) can choose to opt in or out of. However, in this demo, everything goes.<br>
                    For this demo, we have assumed that you deposited USD750, and extended credit to your four friends according to your view of their risk profile.<br>We have also forced some random network participants to extend trust to YOU - you're now a middle party within Arboreum.<br><br>
                    There are three parameters you need to provide to the network agent - these are:<br> (1) the <i>minimum amount</i> you want to borrow, (2) the <i>maximum interest rate</i> which you are willing to pay the loan back at, and (3) the <i>degree of collateral</i> you wish to hedge against the loan.<br><br>
                    These parameters all affect each other - higher securitisation implies lower interest, lower interest is associated with lower loan amounts and so on.<br><br>
                    In this instance, we have said we wish to borrow USD1020 at a maximum interest rate of 8%, securitised by a 22% collateralisation ratio.<br>
                    We've prepopulated the table below with the resulting agent function - smoothed into a matrix. You can now select your preferred option and proceed to see the effects on the network."),
               HTML("<h3>Step 1: Select Your Parameters</h3>"),
               numericInput('minLoanAmount', label="Minimum Loan Amount", value = 865),
               numericInput('maxInterestRate', label="Maximum Interest Rate", value = 1.1),
               numericInput('maxCollateralRate', label="Maximum Collateral Percentage", value = 0.42)
               ),
        column(12, offset=1,
               HTML("We've removed the data points for each and every interest rate percentage in the table below for brevity.<br><br>As you can see, a willingness to pay a higher interest rate allows for more funds to be borrowed.<br>Further, a higher securitisation rate lowers the amount of capital 'at risk' from the perspective of lenders further away from you in the network: your immediate neighbours carry most of the risk.<br><br><b>This is a sample of what options you might have available to you. We've selected some parameters for you for the next page.</b>")),
        column(6, offset=1,
               HTML("<h3>Step 2: Choose Your Loan (X-Axis: Interest Rate %, Y-Axis: Securitisation)</h3>"),
               DT::dataTableOutput("loanGrid"),
               HTML("<br><br>"),
               textOutput("proceedWarning"),
               HTML("<br><br>"),
               fluidRow(column(1, offset=0, actionButton('backToStage1', "Previous")), column(1, offset=0, actionButton('stage3', label='Proceed')))
        )
      )
    )
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})

# TODO: for some reason, can't select a single cell [rather, a row - used to have this working?].
# Also, need to save column/row values and cell value as arguments to pass on to the forward prop.

# observeEvent(input$loanGrid_cells_selected, {
#   if (length(input$loanGrid_cells_selected) == 0) {
#     session$userData$selectedLoan <- FALSE
#   } else {
#     if (length(input$loanGrid_cells_selected == 1)) {
#       session$userData$selectedLoan <- TRUE
#     } else {
#       session$userData$selectedLoan <- FALSE
#       showNotification("You need to choose a single loan option.", type="error")
#     }
#   }
#   }
# )

observeEvent(input$backToStage1, {
  js$redirect("?demo_1")
})

observeEvent(input$stage3, {
 # if(session$userData$selectedLoan) {
    js$redirect("?demo_3")
 # } else { output$proceedWarning <- renderText("You need to select a single value!")  }
})


