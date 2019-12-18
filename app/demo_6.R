import(dplyr)
import(network)
import(modules)
import(plotly)

loan111 <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/NetworkInteractive.rds"))
loan1   <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/NeighborNetworkInteractive.rds"))
loan69  <- readRDS(here::here("ShinyApps/Arboreum/app/pregenerated/ThirdPartyNetworkInteractive.rds"))

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(12, offset=1,
               HTML("<h2>Loan Networks</h2>"),
               HTML("Having seen the animations demonstrating propagation, here are some more interactive force networks<br>demonstrating how the loans were propagated.")
        ),
        column(7, offset=1,
               HTML("<h3>First Loan: You Borrowed $1040</h3>"),
               renderForceNetwork(loan111)
        ),
        column(7, offset=1,
               HTML("<h3>Second Loan: Mack Borrowed $400</h3>"),
               renderForceNetwork(loan1)
        ),
        column(7, offset=1,
               HTML("<h3>Third Loan: Someone Borrowed $120</h3>"),
               renderForceNetwork(loan69)
        ),
        column(12, offset=1, fluidRow(column(1, offset=0, actionButton('backToStage5', "Previous")))
      )))
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})



observeEvent(input$backToStage5, {
  js$redirect("?demo_5")
})