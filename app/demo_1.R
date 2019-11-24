
require(MASS)
require(igraph)
require(network)
require(sna)

require(modules)
require(here)

generate <- modules::use(here::here("app/src/Generate.R"))

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

output$depositInLimits <- renderImage({ return(list(
  src = if ((session$userData$mackTrust + session$userData$gauravTrust + session$userData$laurenceTrust + session$userData$pranavTrust) <= session$userData$amountDeposit) {"images/tick.JPG"} else {"images/cross.JPG"},
  width = 50,
  height = 50,
  contentType = "image/jpg",
  alt = "Are Initial Parameters Acceptable?"
))}, deleteFile = FALSE)

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  if(session$userData$user$sp==0) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(4, offset=4,
               ttextInput("username", "User Name:", value="", style="width: 100%;", autofocus=TRUE),
               passwordInput("password1", "Password:", value=""),
               passwordInput("password2", "Repeat Password:", value=""),
               ttextInput("email", "Email address (for account verification):", value="", style="width: 100%;"),
               actionButton("register_btn", "Register", class="btn-primary btn-sm",
                            style="display: block; margin-top: 2em; margin-left: auto; margin-right: auto;")
        )
      )
    )
  } else {                               # logged in but email unverified, send email and return verification inputs
    if(session$userData$user$sp && !session$userData$user$emailverified) {
      session$userData$tempcode <- generate_code()
      if(session$userData$fixingemail) {           # if email is already verified, user is changing email address
        send.email(session$userData$user$username, session$userData$user$email,
                   paste0("Code to verify your ", site_name," account."),
                   paste0("Here's the code you must enter to change your ", site_name," email address: ", session$userData$tempcode))
      } else {                                     #    otherwise user is verifing email address for a new account
        send.email(session$userData$user$username, session$userData$user$email,
                   paste0("Code to verify your new ", site_name," account."),
                   paste0("Here's the code you must enter to complete your ", site_name," registration: ", session$userData$tempcode))
      }
      pageText <- tagList(
        fluidRow(
          column(6, offset=3,
                 HTML(paste0(
                   "<h3>Almost there...</h3><p>We've just sent an email to <b>", session$userData$user$email,
                   "</b> with a 6-digit temporarty PIN. To complete your registration, ",
                   "enter the PIN here and click the OK button.</p>"))
          )
        )
      )
    } else {                            # logged in and email verified; show profile update inputs
      if(session$userData$user$sp) {
        session$userData$fixingemail <- TRUE
        pageText <- tagList(
            fluidRow(
              column(8, offset=1,
                     HTML("<h3 style='margin-bottom: 1em;'>Welcome to Arboreum!</h4>"),
                     HTML("Backstory - Hi there! Welcome to our demo.<p> In this first step you will be joining the Arboreum network.<p>
                          To start, you must decide how much money you would like to put in.<br>
                          Next you will decide what is the maximum amount of an unsecured loan you are willing to give to four hypothetical friends/business colleagues.<br><br>
                          NOTE: you can extend your given trust amount to <i>each</i> of the people below, should you choose: your credit is only utilised if someone explicitly requests it.")
                    )
            ),
            fluidRow( column(4, offset=1, numericInput("amountDeposit", "Amount To Deposit (Between 100-10,000):", value=100, min=100, max=10000, width= "30%") ) ),
            
            column(6, offset = 1, wellPanel(fluidRow(column(1, offset=0, imageOutput("mackImage",     height=120)),
                                        column(5, offset=2,
                                               HTML("<h3>Mack</h3>"),
                                               HTML("PROFILE<p>"),
                                               numericInput("mackTrust", "Credit To Extend To Mack:", value = 0, min = 0, max = 10000, width = "100%"))))),
            column(6, offset = 1, wellPanel(fluidRow(column(1, offset=0, imageOutput("gauravImage",   height=120)),
                                        column(5, offset=2,
                                               HTML("<h3>Gaurav</h3>"),
                                               HTML("PROFILE<p>"),
                                               numericInput("gauravTrust", "Credit To Extend To Gaurav:", value = 0, min = 0, max = 10000, width = "100%"))))),
            column(6, offset = 1, wellPanel(fluidRow(column(1, offset=0, imageOutput("laurenceImage", height=120)),
                                        column(5, offset=2,
                                               HTML("<h3>Laurence</h3>"),
                                               HTML("PROFILE<p>"),
                                               numericInput("laurenceTrust", "Credit To Extend To Laurence:", value = 0, min = 0, max = 10000, width = "100%"))))),
            column(6, offset = 1, wellPanel(fluidRow(column(1, offset=0, imageOutput("pranavImage",   height=120)),
                                        column(5, offset=2,
                                               HTML("<h3>Pranav</h3>"),
                                               HTML("PROFILE<p>"),
                                               numericInput("pranavTrust", "Credit To Extend To Pranav:", value = 0, min = 0, max = 10000, width = "100%"))))),
            fluidRow(column(6, offset = 1, imageOutput("depositInLimits"), actionButton("stage2", label="Proceed"), actionButton("stage2_precooked", label="Use Precalc")))
          )
      }
    }
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})                    #    Shiny expects renderUI to return some text, which may have embedded
#    HTML. Although Shiny examples rarely use it, if you include an
#    explicit return, your code looks more R-like and it helps to keep
#    straight what part of your renderUI is actual code and what part
#    is building and returning the HTML. Nested tagLists() are ok.
#    Other pages here embed the returns in the code rather than using a
#    variable that is returned at the end of the code. Either way is ok.

observeEvent(input$mackTrust, {
  session$userData$mackTrust <- if (is.numeric(input$mackTrust)) {input$mackTrust} else {0}
  session$userData$inLimits <- session$userData$mackTrust <= session$userData$amountDeposit
  if (session$userData$inLimits) {print("Enabling"); shinyjs::enable("stage2")} else {print("Disabled"); shinyjs::disable("stage2")}
  print(paste0(session$userData$inLimits))
  output$depositInLimits <- renderImage({ return(list(
    src = if (session$userData$inLimits) {"images/tick.JPG"} else {"images/cross.JPG"},
    width = 100,
    height = 100,
    contentType = "image/jpg",
    alt = "Are Initial Parameters Acceptable?"
  ))}, deleteFile = FALSE)
})

observeEvent(input$gauravTrust, {
  session$userData$gauravTrust <- if (is.numeric(input$gauravTrust)) {input$gauravTrust} else {0}
  session$userData$inLimits <- session$userData$gauravTrust <= session$userData$amountDeposit
  output$depositInLimits <- renderImage({ return(list(
    src = if (session$userData$inLimits) {"images/tick.JPG"} else {"images/cross.JPG"},
    width = 100,
    height = 100,
    contentType = "image/jpg",
    alt = "Are Initial Parameters Acceptable?"
  ))}, deleteFile = FALSE)
})

observeEvent(input$laurenceTrust, {
  session$userData$laurenceTrust <- if (is.numeric(input$laurenceTrust)) {input$laurenceTrust} else {0}
  session$userData$inLimits <- session$userData$laurenceTrust <= session$userData$amountDeposit
  output$depositInLimits <- renderImage({ return(list(
    src = if (session$userData$inLimits) {"images/tick.JPG"} else {"images/cross.JPG"},
    width = 100,
    height = 100,
    contentType = "image/jpg",
    alt = "Are Initial Parameters Acceptable?"
  ))}, deleteFile = FALSE)
})

observeEvent(input$pranavTrust, {
  session$userData$pranavTrust <- if (is.numeric(input$pranavTrust)) {input$pranavTrust} else {0}
  session$userData$inLimits <- session$userData$pranavTrust <= session$userData$amountDeposit
  output$depositInLimits <- renderImage({ return(list(
    src = if (session$userData$inLimits) {"images/tick.JPG"} else {"images/cross.JPG"},
    width = 100,
    height = 100,
    contentType = "image/jpg",
    alt = "Are Initial Parameters Acceptable?"
  ))}, deleteFile = FALSE)
})


observeEvent(input$amountDeposit, {
  session$userData$amountDeposit <- if (is.numeric(input$amountDeposit)) {input$amountDeposit} else {0}
  session$userData$inLimits <- (session$userData$mackTrust + session$userData$gauravTrust + session$userData$laurenceTrust + session$userData$pranavTrust) <= session$userData$amountDeposit
  output$depositInLimits <- renderImage({ return(list(
    src = if (session$userData$inLimits) {"images/tick.JPG"} else {"images/cross.JPG"},
    width = 100,
    height = 100,
    contentType = "image/jpg",
    alt = "Are Initial Parameters Acceptable?"
  ))}, deleteFile = FALSE)
})

observeEvent(input$stage2, {
  if (session$userData$inLimits)
      {
        initialisedNetwork <- generate$buildCorePeri(N = 100, K = 10)
        initialisedSheets  <- suppressWarnings(generate$initializeSheets(initialisedNetwork, K = 10, A0 = 10000))
        
        baseNetwork        <- initialisedSheets[[2]]
        riskArray          <- suppressWarnings(calcRiskArray(baseNetwork))
        
        risk.array <- riskArray[[1]]
        ntwk       <- riskArray[[2]]
        #### Add Node ####
        rslt <- addNode$addNode2Ntwk(ntwk,risk.array,750,
                                     out.DF=data.frame(list(nodes=c(1,2,3,4), #this dataframe would be input by UI
                                                            names=c('Mack','Gaurav','Laurence','Pranav'),
                                                            trust=c(325,225,175,275))))
        
        saveRDS(rslt, here::here("app/tmp/userGeneratedNetwork.rds"))
        
        js$redirect("?demo_2")
      }
  else { showNotification("You are extending more credit to someone than you have deposited. Please adjust your numbers appropriately.", type="error") }
})

