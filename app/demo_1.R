
require(MASS)
require(igraph)
require(network)
require(sna)

require(modules)
require(here)

session$userData$fixingemail <- FALSE     # flag to separate fixing email from new regisitation

output$mackImage <- renderImage({ return(list(
  src = "images/MR.jpg",
  width = 100,
  height = 100,
  contentType = "image/jpg",
  alt = "Mack"
))}, deleteFile = FALSE)

output$gauravImage <- renderImage({ return(list(
  src = "images/GS.jpg",
  width = 100,
  height = 100,
  contentType = "image/jpg",
  alt = "Gaurav"
))}, deleteFile = FALSE)

output$laurenceImage <- renderImage({ return(list(
  src = "images/LD.jpg",
  width = 100,
  height = 100,
  contentType = "image/jpg",
  alt = "Laurence"
))}, deleteFile = FALSE)

output$pranavImage <- renderImage({ return(list(
  src = "images/PR.jpg",
  width = 100,
  height = 100,
  contentType = "image/jpg",
  alt = "Pranav"
))}, deleteFile = FALSE)

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  pageText <- tagList(
    fluidRow(
      column(8, offset=1,
             HTML("<h3 style='margin-bottom: 1em;'>Committing Funds</h4>"),
             HTML("To start, we decide how much money you want to commit to the network.<br><br>Think of these as funds you wish to deposit into a Fixed Deposit account at your local bank.<br><br>
                        Next, we decide the maximum limit of unsecured credit you are willing to loan to four friends.<br><br> In future iterations, you will also set your risk tolerance via a small questionnaire.<br><br>
                        NOTE: you can extend the entirety of your funds to <i>each</i> of the people below, should you choose.<br><br>Your funds are only utilised if you participate in funding a loan which falls within your risk parameters.<br><br>")
      )
    ),
    fluidRow( column(8, offset=1, numericInput("amountDeposit", "USD Amount To Deposit: [Demo V0.1: Fixed Values]", value=750, width= "30%") ) ),
    
    column(4, offset = 1, wellPanel(fluidRow(column(1, offset=0, imageOutput("mackImage",     height=120)),
                                             column(4, offset=2,
                                                    HTML("<h3>Mack</h3>"),
                                                    numericInput("mackTrust", "Credit To Mack", value = 325, width = "100%"))))),
    column(4, offset = 0, wellPanel(fluidRow(column(1, offset=0, imageOutput("gauravImage",   height=120)),
                                             column(4, offset=2,
                                                    HTML("<h3>Gaurav</h3>"),
                                                    numericInput("gauravTrust", "Credit To Gaurav", value = 225, width = "100%"))))),
    column(4, offset = 1, wellPanel(fluidRow(column(1, offset=0, imageOutput("laurenceImage", height=120)),
                                             column(4, offset=2,
                                                    HTML("<h3>Laurence</h3>"),
                                                    numericInput("laurenceTrust", "Credit To Laurence", value = 175, width = "100%"))))),
    column(4, offset = 0, wellPanel(fluidRow(column(1, offset=0, imageOutput("pranavImage",   height=120)),
                                             column(4, offset=2,
                                                    HTML("<h3>Pranav</h3>"),
                                                    numericInput("pranavTrust", "Credit To Pranav", value = 275, width = "100%"))))),
    fluidRow(column(6, offset = 1, actionButton("stage2", label="Proceed")))
  )
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})                    #    Shiny expects renderUI to return some text, which may have embedded
#    HTML. Although Shiny examples rarely use it, if you include an
#    explicit return, your code looks more R-like and it helps to keep
#    straight what part of your renderUI is actual code and what part
#    is building and returning the HTML. Nested tagLists() are ok.
#    Other pages here embed the returns in the code rather than using a
#    variable that is returned at the end of the code. Either way is ok.

observeEvent(input$stage2, {
  js$redirect("?demo_2")
})

