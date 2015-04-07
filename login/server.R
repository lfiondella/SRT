library(shiny)
shinyServer(function(input, output,session) {
  
  output$pwd <- renderText({
    paste0("Your PW is: ",input$passwd)
  })
  
  
})