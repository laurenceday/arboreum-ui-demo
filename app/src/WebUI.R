
library(shiny)
library(DT)
library(data.table)
library(dplyr)
library(networkD3)

source(here::here("app/src/Generate.R"), local=TRUE)
source(here::here("app/src/Traverse.R"), local=TRUE)

ui <- fluidPage(
  sliderInput(inputId = "coreNodes", label = "Choose # Core Nodes", value = 10, min = 10, max = 40),
  sliderInput(inputId = "periNodes", label = "Choose # Peripheral Nodes", value = 50, min = 40, max = 200),
  actionButton(inputId = "refreshTable", label = "Update Balance Sheet"), actionButton(inputId = "refreshRiskArray", label = "Update Risk Array"),
  hr(),
  mainPanel(
    tabsetPanel(
      id = 'sampleData',
      tabPanel("Balances", tableOutput("balanceSheet")),
      tabPanel("Simple Network", simpleNetworkOutput("simpleNetwork")),
      tabPanel("Subjective Trust", tableOutput("riskArray1"))
    )
  )
)

server <- function (input, output, session) {

  observeEvent(input$refreshTable, {
    core <- input$coreNodes
    peri <- input$periNodes
    print(paste0("Calculating table with ", peri, " peripheral nodes and ", core, " core nodes.."))
    initialisedNetwork <- buildCorePeri(N = peri, K = core)
    initialisedSheets  <- suppressWarnings(initializeSheets(initialisedNetwork, K = core, A0 = 10000))
    
    output$balanceSheet <- renderTable({
      baseDataframe      <- initialisedSheets[[1]]
      baseDataframe      <- baseDataframe[,c(1, 2, 3, 6, 7, 8, 9)]
      names(baseDataframe)[1] <- "Assets"
      names(baseDataframe)[2] <- "At_Risk"
      names(baseDataframe)[3] <- "Equity"
      names(baseDataframe)[4] <- "Trust_Given"
      names(baseDataframe)[5] <- "Trust_Received"
      names(baseDataframe)[6] <- "Trust_Borrowed"
      names(baseDataframe)[7] <- "Trust_Lent"
      return (baseDataframe)
    }, striped = TRUE, bordered = TRUE, rownames = TRUE)
    
    ntwkData <- initialisedNetwork[["mel"]]
    source <- sapply(ntwkData, function(x) x[["inl"]])
    target <- sapply(ntwkData, function(x) x[["outl"]])
    zeroValues <- replicate(length(source), 0)
    ntwkData <- data.frame(source, target, zeroValues)
    
    output$simpleNetwork <- renderSimpleNetwork({
      simpleNetworkData <- ntwkData
      suppressWarnings(simpleNetwork(simpleNetworkData, height = 1600, width = 1600, zoom = TRUE, charge = -300))
    })
    
    numberNodes        <- core + peri
    baseNetwork        <- initialisedSheets[[2]]
    print("Calculating risk array")
    rsltCalc           <- suppressWarnings(calcRiskArray(baseNetwork))
    
    output$riskArray1 <- renderTable({
      riskList           <- (rsltCalc[[1]])[1:numberNodes, 1:numberNodes, 1]
      return(riskList)
    }, striped = TRUE, bordered = TRUE, rownames = TRUE)
    
  })
  
  
}

shinyApp(ui = ui, server = server)