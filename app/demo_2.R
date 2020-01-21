library(dplyr)
library(networkD3)
library(network)

z.loess <- readRDS(here::here("pregenerated/LoanFunctionMatrix.rds"))
rotate  <- function(x) t(apply(x, 2, rev))
output$loanGrid <- DT::renderDataTable(subset(stats::setNames(
                                                    data.frame(round(t(z.loess)[c(1,seq(5,95,5),99),seq(5,75,5)],0),
                                                               row.names = paste0('c=',c(1,seq(05, 95, 5),99),'%')),
                                                               paste0('r=',seq(05, 75, 5),'%')), 
                                              col.names = paste0('r=',seq(05, 75, 5),'%'),
                                              selection=list(target='cell')))

output$proceedWarning <- renderText("")

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  pageText <- tagList(
    fluidRow(
      column(12, offset=1,
             HTML("<h3>Using Arboreum: Requesting A Loan<br><br></h3>"),
             HTML("Let us assume that you would like to take out a loan. Maybe you're short on milk, or want to plant trees. Who knows?<br><br>
                  In practice, you would explain what your loan is for, and people who have extended credit to you could opt in or out.<br><br>
                  We have assumed that you deposited $750, and extended credit to your four friends per the previous screen.<br><br>We have also forced some random network participants to extend some credit lines to <i>you</i>.<br><br>
                  When asking for a loan, there are three required parameters:<br> (1) the <i>minimum amount</i> you want to borrow<br>(2) the <i>maximum interest rate</i> which you are willing to pay the loan back at<br>(3) the <i>percentage of collateral</i> you wish to pledge against the loan.<br><br>
                  The above all impact each other - higher securitisation implies lower interest, lower interest implies lower loan amounts, etc.<br><br>
                  The FAQ (reachable from the top right menu) goes into a <b>lot</b> more detail: read it if you have the time!<br><br>
                  Now, we ask the network if we can take out a loan with the parameters you've selected.<br>Arboreum crawls the network and coalesces <i>everyone else's views</i> of your risk to create your resulting loan function.<br><br>We've prepopulated the table below with the resulting loan function - smoothed into a matrix. <br><br>
                  In this instance, we have said we wish to borrow $1020 at a maximum interest rate of 8%, securitised by a 22% collateralisation ratio.<br>
                  "), #
             HTML("<h3>Step 1: Select Parameters [Demo V0.1: Fixed Values]</h3>"),
             numericInput('minLoanAmount', label="Minimum Loan Amount (USD)", value = 1020),
             numericInput('maxInterestRate', label="Maximum Interest Rate", value = 1.08),
             numericInput('maxCollateralRate', label="Maximum Securitisation", value = 0.22)
             ),
      column(12, offset=1, 
             HTML("<h3>Step 2: Choose Your Loan Amount [Demo V0.1: Illustration Only]</h3>"),
             HTML("<br>We've only included a few of the resulting `offers' in the table below, but it's enough to illustrate!<br><br>If you're willing to pay more interest, you can borrow more funds.<br><br>Moreover, higher securitisation lowers the amount of capital 'at risk' from the perspective of lenders further away from you in the network.<br><br><b><p style='color:blue'>This is a sample of what options you might have available to you.<br>In this version of the demo [V0.1], selecting a value here has no impact.<br><br>We've precomputed a loan of USD1020 on the next page.</p></b><br><br>")),
      column(6, offset=1,
             DT::dataTableOutput("loanGrid"),
             HTML("<br><br>"),
             textOutput("proceedWarning"),
             HTML("<br><br>"),
             fluidRow(column(1, offset=0, actionButton('backToStage1', "Previous")), column(1, offset=0, actionButton('stage3', label='Proceed')))
      )
    )
  )
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


