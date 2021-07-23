 # Shiny Web App

library(shiny)
library(leaflet)
source("Rating.R")

ui <- fluidPage(
  titlePanel("COVID 19 Underwriting tool"),
  sidebarLayout(
    sidebarPanel(
  dateInput(
  inputId = "date", label = "Date of Birth"),
  selectInput(inputId = "destination", label = "Destination", choices = WHO_data$Name),
  numericInput(inputId = "LengthOfStay", label = "Length of stay", value =  10, min = 1, max = 180),
  selectInput(inputId = "Vacinnation", label = "Vaccinated?", choices = c("Yes", "No")),
  selectInput(inputId = "Condition", label = "Which condition(s) do you have?", choices = Consonance$List.of.diseases)),
  mainPanel(
    verbatimTextOutput("summary"),
    plotOutput("barplot1"),
    plotOutput("barplot2")
  )))



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
    x11 <- Rating("01-01-1990", "Global", 30, TRUE, "None of the above")
    x12 <- Rating("01-01-1990", input$destination, 30, TRUE, "None of the above")
    x13 <- Rating("01-01-1990", "Viet Nam", 30, TRUE, "None of the above")
    
    x21 <- Rating(input$date, "Global", input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    x22 <- Rating(input$date, input$destination, input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    x23 <- Rating(input$date, "Viet Nam", input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    
    x31 <- Rating("01-01-1990", "Global", 30, FALSE, "None of the above")
    x32 <- Rating("01-01-1990", input$destination, 30, FALSE, "None of the above")
    x33 <- Rating("01-01-1990", "Viet Nam", 30, FALSE, "None of the above")
    
    data <- data.frame(value = round(100*c(x1$ActualPofCOVID, x2$ActualPofCOVID, x3$ActualPofCOVID), digits = 2), 
                       name = c("Global- 30 years old- 30 days trip", "Your trip", "Vietnam - 30 years old - 30 days trip"))
    
    data1 <- data.frame(Global = 100* c(x11$ActualPofCOVID, x21$ActualPofCOVID, x31$ActualPofCOVID),
                        YourTrip = 100 * c(x12$ActualPofCOVID, x22$ActualPofCOVID, x32$ActualPofCOVID),
                        Vietnam = 100 * c(x13$ActualPofCOVID, x23$ActualPofCOVID, x33$ActualPofCOVID))
    data1 <- as.matrix(data1)
    bp <- barplot(data1,
                  main = "Probablity of COVID-19 infection by Area, Age and Vaccination status",
            beside = TRUE,
            width = 0.05, 
            xlab = "Country",
            ylab = "Probability(%)",
            legend = c("Vaccinated 30 year old", "Your case", "Unvaccinated 30 year old"),
            args.legend = list(title = "Adjustment by age and vaccination", x = "topright", cex = .7),
            col = c("lightblue", "lavender", "mistyrose"),
            las = 1
            )
    text(bp, 0, round(data1, 2), cex = 1, pos = 3)
  })
  
}
shinyApp(ui = ui, server = server)


