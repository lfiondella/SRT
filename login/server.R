library(shiny)
shinyServer(function(input, output, session) {
  pass <- "password"
  
  output$pwd <- renderText({
    if (pass==input$passwd) #toString()
      paste("Correct. Welcome!")
    else
      paste("Incorrect password. Try Again.")
      #paste("You entered: ",input$passwd)
  })
})