### om_skeleton profile.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

output$pageStub <- renderUI({rv$limn; isolate({
  if(page_debug_on) {
    cat(paste0("Rendering ", webpage$name, " v.", rv$limn, "\n"))
  }
  if(session$userData$user$sp) {      # not logged in; return registration inputs
    pageText <- tagList(
      fluidRow(
        column(6, offset=1,
               HTML("NEXT STAGE")
        )
      )
    )
  }
  return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
})})

#observeEvent(input$stage3, {
#  if (session$userData$inLimits) {js$redirect("demo_3")} else { showNotification("You are extending more credit to someone than you have deposited. Please adjust your numbers appropriately.", type="error") }
#})

