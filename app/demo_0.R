output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  pageText <- tagList(
    fluidRow(
      column(8, offset=1,
             HTML("<h3 style='margin-bottom: 1em;'>Welcome to Arboreum!</h4>"),
             HTML("Hi there! Welcome to the Arboreum V0.1 demo!<p><p>We're currently working on an improved landing page to explain what we're all about. The demo still works though!")
      )
    ),
    fluidRow(column(6, offset = 1, actionButton("stage1", label="Proceed")))
  )
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})                    #    Shiny expects renderUI to return some text, which may have embedded
#    HTML. Although Shiny examples rarely use it, if you include an
#    explicit return, your code looks more R-like and it helps to keep
#    straight what part of your renderUI is actual code and what part
#    is building and returning the HTML. Nested tagLists() are ok.
#    Other pages here embed the returns in the code rather than using a
#    variable that is returned at the end of the code. Either way is ok.

observeEvent(input$stage1, {
  js$redirect("?demo_1")
})

