 # Shiny Web App

library(shiny)
library(leaflet)
source("Rating.R")

ui <- fluidPage(
  titlePanel(title = "Reactive COVID-19 Underwriting Tool"),
  sidebarLayout(
    sidebarPanel(
  dateInput(
  inputId = "date", label = "Date of Birth:"),
  selectInput(inputId = "destination", label = "Destination:", choices = WHO_data$Name),
  numericInput(inputId = "LengthOfStay", label = "Length of stay(days):", value =  10, min = 1, max = 180),
  selectInput(inputId = "Vacinnation", label = "Vaccinated?", choices = c("Yes", "No")),
  selectInput(inputId = "Condition", label = "Which condition do you have from the following?", choices = Consonance$List.of.diseases)),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Plots and Statistics",
        verbatimTextOutput("summary"),
        plotOutput("barplot1"),
        plotOutput("barplot2")
      ),
      tabPanel("Information", 
        tags$h3("LIST OF ASSUMPTIONS"),
        hr(),
        tags$h5("Rate ratios of COVID 19 transmission, hospitalization and death"),
        tags$p("the data from CDC provides how much more the persons in that age group infected by COVID-19, or need hospitalization, or die compared to the reference group (18-29 years old)."),
        tags$a("Source: CDC", href = "https://www.cdc.gov/coronavirus/2019-ncov/covid-data/investigations-discovery/hospitalization-death-by-age.html"),
        tags$h5("Effectiveness of vaccination assumed to be a reduction of 80% in infection, hospitalization and death"),
        tags$p("Effectiveness of the vaccine is assumed to reduce 80% of the Probability of infection, hospitalization and death. In fact, effectiveness on severe or critical is reduction 73-82%. We assume it will reduce 80% for easier calculation."),
        tags$a("Source: CDC", href = "https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/fully-vaccinated-people.html"),
        tags$h5("Effectiveness on transmission reduction on partly vaccinated community: 0%"),
        tags$p("It is definitely effective but how effective is not clear. It is required the physical distancing, quarantine, school and business closed to be the same. Vaccination might reduce the seriousness of the society on these prevention measures as well. No country has reach community immunity at the moment. At the moment I conservatively assume it is 0%."),
        tags$h5("Assumption: Medical cost in USA, Canada and Mexico is twice higher than other OECD countries and four times higher than other countries."),
        tags$p("This is some source I found out about the medical costs in different countries:"),
        tags$a("Source: USA Today", href = "https://www.usatoday.com/story/money/2019/04/11/countries-that-spend-the-most-on-public-health/39307147/"),
        tags$p("At this moment I assume the ratio 4:2:1 for North America: Other OECD countries: Remaining countries for simple calculation."),
        tags$h5("We assume only serious COVID case being treated"),
        tags$p("Mild case (same with flu) will stay home or being in quarantine with minimal medical cost. Only hospitalization will generate medical bill to make insurance company to pay."),
        tags$h5(" Hospitalization rate is 9% for COVID 19"),
        tags$h5("People with pre-existing conditions is more likely to have more severe COVID 19"),
        tags$p("For example: diabetes or malignancy is proven to consonate with COVID 19 to make the disease more severe."),
        tags$a("Source: IJID Online", href = "https://www.ijidonline.com/article/S1201-9712(20)30572-5/abstract"),
        tags$h5("Probability of COVID 19 infection"),
        tags$p("Probability of contracting COVID-19 is assumed to be the probability of that country in the past 7 days if the trip is less than 7 days and each extra day above 7 days will be the probability of contracting COVID-19 from the beginning of the pandemic (assumed to be 01-01-2020)"),
        tags$a("Source: WHO", href = "https://covid19.who.int/table"),
        hr(),
        tags$h3("GLOSSARY"),
        tags$p(" The Pure Premium: Pure premium is the expected claim cost for which the insurance company has to pay. Pure premium will be close to the observed mean of claim cost if: the number of insured persons is large enough (for example: 1,000 persons) and the above assumptions are all true or very close to the true figure."),
        
        hr(),
        tags$h3("IMPORTANT NOTE"),
        tags$i("This application might not work properly sometimes because of the problem with the source code. For example, in July 24th, 2021, I found a problem in WHO data (they accidentally put an extra comma in the source file), which makes the server read the file in a wrong way, the probability of COVID 19 infection becomes abnormally high. I will try to maintain this application, if you find any abnormality, please report to me at hothieuminh@gmail.com")
        
          )
    )
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
    
    
    data1 <- data.frame(Global = 100* c(x11$ActualPofCOVID, x21$ActualPofCOVID, x31$ActualPofCOVID),
                        YourTrip = 100 * c(x12$ActualPofCOVID, x22$ActualPofCOVID, x32$ActualPofCOVID),
                        Vietnam = 100 * c(x13$ActualPofCOVID, x23$ActualPofCOVID, x33$ActualPofCOVID))
    
    
    data1 <- as.matrix(data1)
    
    bp1 <- barplot(data1,
                   main = "Probablity of COVID-19 infection by Area, Age and Vaccination status",
                   beside = TRUE,
                   width = 0.05, 
                   xlab = "Country",
                   ylab = "Probability(%)",
                   legend = c("Vaccinated 30 year old", "Your case", "Unvaccinated 30 year old"),
                   args.legend = list(title = "Adjustment by age and vaccination", x = "topright", cex = 1.2),
                   col = c("lightblue", "lavender", "mistyrose"),
                   las = 1
    )
    text(bp1, 0, round(data1, 2), cex = 0.7, pos = 3)
  })
  
  output$barplot2 <- renderPlot({
    x11 <- Rating("01-01-1990", "Global", 30, TRUE, "None of the above")
    x12 <- Rating("01-01-1990", input$destination, 30, TRUE, "None of the above")
    x13 <- Rating("01-01-1990", "Viet Nam", 30, TRUE, "None of the above")
    
    x21 <- Rating(input$date, "Global", input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    x22 <- Rating(input$date, input$destination, input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    x23 <- Rating(input$date, "Viet Nam", input$LengthOfStay, ifelse(input$Vacinnation== "Yes", TRUE, FALSE), input$Condition)
    
    x31 <- Rating("01-01-1990", "Global", 30, FALSE, "None of the above")
    x32 <- Rating("01-01-1990", input$destination, 30, FALSE, "None of the above")
    x33 <- Rating("01-01-1990", "Viet Nam", 30, FALSE, "None of the above")
    
    
    data1 <- data.frame(Global = c(x11$PP, x21$PP, x31$PP),
                        YourTrip = c(x12$PP, x22$PP, x32$PP),
                        Vietnam = c(x13$PP, x23$PP, x33$PP))
    
    
    data1 <- as.matrix(data1)
  
    bp1 <- barplot(data1,
                   main = "Extra Pure Premium Area, Age and Vaccination status",
                   beside = TRUE,
                   width = 0.05, 
                   xlab = "Country",
                   ylab = "Pure Premium (USD)",
                   legend = c("Vaccinated 30 year old", "Your case", "Unvaccinated 30 year old"),
                   args.legend = list(title = "Adjustment by age and vaccination", x = "topright", cex = 1.2),
                   col = c("lightblue", "lavender", "mistyrose"),
                   las = 1
    )
    text(bp1, 0, round(data1, 2), cex = 0.7, pos = 3)
  })
}
shinyApp(ui = ui, server = server)


