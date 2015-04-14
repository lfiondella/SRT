library(shiny)
# Miles is currently working on this simple login page
shinyUI(fluidPage(
  
  tabPanel("Login",
           br(),
           tags$form(
             passwordInput("passwd",label = "Enter password"),
             submitButton("send password to shiny")
           ),
           textOutput("pwd")
           )
))