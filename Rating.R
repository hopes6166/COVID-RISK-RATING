  # Importing Data
source("PersonalInformation.R")
source("VaccineEffective.R")
library(dplyr)
#library(readr)


  #OECD Name list
OtherOECD <- read.csv("OtherOECD.csv", header = FALSE)
Consonance <- read.csv("Consonance.csv")
MedicalCost <- read.csv("MedicalCost.csv")

  # Number of days from 1-1-2020
DateFromCovidTime <-as.numeric(Sys.Date() - as.Date("2020-01-01"))

  # Import data from WHO # Fix the problem because of extra comma in WHO data from 24 July 2021
file <- "https://covid19.who.int/WHO-COVID-19-global-table-data.csv"
rows <- readLines(file)
rows[2] <- gsub(",$", "", rows[2])
WHO_data  <- read.csv(text=rows)

  # Rate of covid import
RateOfCOVID <- read.csv("RateOfCOVID.csv")

  # Create new features: contract rate per first 7 days, contract rate per day and death rate per day
WHO_data$ContractRatePerFirst7PD <- WHO_data$Cases...newly.reported.in.last.7.days.per.100000.population/100000/7
WHO_data$ContractRatePD <- WHO_data$Cases...cumulative.total.per.100000.population/100000/DateFromCovidTime
WHO_data$DeathRatePD <- WHO_data$Deaths...cumulative.total.per.100000.population/100000/DateFromCovidTime


Rating <- function(DateOfBirth, Destination, LengthOfStay, Vaccinated, Condition) {
  ProbabilityCOVIDFirst7PD <- filter(WHO_data, Name == Destination)[, "ContractRatePerFirst7PD"]
  ProbabilityCOVIDRemainingDayPD <- filter(WHO_data, Name == Destination)[, "ContractRatePD"]
  PofCOVID <- ifelse(LengthOfStay > 7, ProbabilityCOVIDRemainingDayPD * (LengthOfStay - 7) + ProbabilityCOVIDFirst7PD * 7, ProbabilityCOVIDFirst7PD * LengthOfStay)
  PofCOVID <- ifelse(PofCOVID > 1, 1, PofCOVID)
  ConsonanceMultiple <- filter(Consonance, Consonance$List.of.diseases == Condition)[, 4]
  ActualPofCOVID <- ifelse(Vaccinated, PofCOVID*(1 - VaccineOnTransmission), PofCOVID)
  
  Age <- floor(as.numeric(Sys.Date() - as.Date(DateOfBirth))/365.25)
  HospitalizationLength <- as.numeric(approx(RateOfCOVID$Age, RateOfCOVID[, 4], Age, method = "constant", yleft = 2, yright = 7.5)[2])
  
  # Death Rate Per day at the Destination
  DeathRatePD <- filter(WHO_data, Name == Destination)[, "DeathRatePD"]
  # Death Rate
  DeathRate <- (ifelse(Vaccinated, DeathRatePD * LengthOfStay * (1- VaccineOnDeath), DeathRatePD * LengthOfStay))*ConsonanceMultiple
  
  # Expected Medical Cost if getting COVID
  EM <- filter(MedicalCost, Country == Destination)[, 3] * HospitalizationLength
  
  # Expected PCV medical cost payment if having COVID 
  EPP <- ifelse(EM/ HospitalizationLength > 852, 852 * HospitalizationLength, EM)
  
  
  # Expected PCV repatriation cost if death
  EPR <- 2553
  
  # Pure Premium
  PP <- EPR * DeathRate + EPP * ActualPofCOVID
  return(list(ActualPofCOVID = ActualPofCOVID, HospitalizationLength = HospitalizationLength, DeathRate = DeathRate, EM = EM, EPP = EPP, EPR =EPR, PP = PP))
}
