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
    verbatimTextOutput("summary"),
    plotOutput("barplot1"),
    plotOutput("barplot2")
  ))



server <- function(input, output) {
  output$summary <- renderText({
    rate <- Rating(input$date, input$destination, input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    paste("STATISTICS" ,
          sprintf("Your Probability of contracting COVID 19: %f",rate$ActualPofCOVID),
          "If having COVID-19: ",
          sprintf("- Expected Length of Hospitalization (in case of COVID): %f", rate$HospitalizationLength),
          sprintf("- Expected Probability of Death: %f", rate$DeathRate),
          sprintf("- Expected COVID 19 Medical Cost: %f", rate$EM),
          sprintf("- Expected COVID 19 PCV Medical Cost payment: %f", rate$EPP),
          sprintf("- Expected COVID 19 PCV Repatriation payment: %f", rate$EPR),
          sprintf("- Expected Pure Premium: %f", rate$PP),
          sep = "\n")
  })
  
  output$barplot1 <- renderPlot({
    x1 <- Rating("01-01-1990", "Global", 30, FALSE, "None of the above")
    x2 <- Rating(input$date, input$destination, input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    x3 <- Rating("01-01-1990", "Viet Nam", 30, FALSE, "None of the above")
    data <- data.frame(value = round(100*c(x1$ActualPofCOVID, x2$ActualPofCOVID, x3$ActualPofCOVID), digits = 2), name = c("Global- 30 years old- 30 days trip", "Your trip", "Vietnam - 30 years old - 30 days trip"))
    barplot(height = data$value, 
            name = data$name, 
            width = 0.05, 
            xlab = "Country",
            ylab = "Probability of Contracting COVID-19 (%)",
            col = "#69b3a2",
            las = 1
            )
  })
  
}
shinyApp(ui = ui, server = server)


