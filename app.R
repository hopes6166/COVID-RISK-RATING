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
  selectInput(inputId = "Condition", label = "Which condition(s) do you have?", choices = Consonance$List.of.diseases))

server <- function(input, output) {

}
shinyApp(ui = ui, server = server)


