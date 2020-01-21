output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  pageText <- tagList(
    fluidRow(
      column(8, offset=1,
             HTML("<h3 style='margin-bottom: 1em;'>Arboreum FAQ</h4>"),
             HTML("Oh lawd still need to fill this out...")
      )
    ),
    fluidRow(column(6, offset = 1, actionButton("home", label="Go To Home")))
  )
  return(pageText)
})})

observeEvent(input$home, {
  js$redirect("?home")
})

