 # Shiny Web App

library(shiny)
library(leaflet)
source("Rating.R")

ui <- fluidPage(
  dateInput(
  inputId = "date", label = "Date of Birth"),
  selectInput(inputId = "destination", label = "Destination", choices = WHO_data$Name),
  numericInput(inputId = "LengthOfStay", label = "Length of stay", value =  10, min = 1, max = 180),
  selectInput(inputId = "Vacinnation", label = "Vaccinated?", choices = c("Yes", "No")),
  selectInput(inputId = "Condition", label = "Which condition(s) do you have?", choices = Consonance$List.of.diseases),
  mainPanel(
    textOutput("summary")
  ))



server <- function(input, output) {
  output$summary <- renderText({ 
    paste("You are going to", Destination, "for", input$LengthOfStay, "days. Are you vaccinated:", input$Vacinnation,". Your pre-existing condition: ", input$Condition)
  })
}
shinyApp(ui = ui, server = server)


