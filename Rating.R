  # Importing Data
source("PersonalInformation.R")
source("VaccineEffective.R")
library(dplyr)

  #OECD Name list
OtherOECD <- read.csv("OtherOECD.csv", header = FALSE)
Consonance <- read.csv("Consonance.csv")
MedicalCost <- read.csv("MedicalCost.csv")

  # Number of days from 1-1-2020
DateFromCovidTime <-as.numeric(Sys.Date() - as.Date("2020-01-01"))

  # Import data from WHO
WHO_data <- read.csv("https://covid19.who.int/WHO-COVID-19-global-table-data.csv")

  # Create new feature: contract rate per first 7 days, contract rate per day and death rate per day
WHO_data$ContractRatePerFirst7PD <- WHO_data$Cases...newly.reported.in.last.7.days.per.100000.population/100000/DateFromCovidTime
WHO_data$ContractRatePD <- WHO_data$Cases...cumulative.total.per.100000.population/100000/DateFromCovidTime
WHO_data$DeathRatePD <- WHO_data$Deaths...cumulative.total.per.100000.population/100000/DateFromCovidTime


ProbabilityCOVIDFirst7PD <- filter(WHO_data, Name == Destination)[, "ContractRatePerFirst7PD"]
ProbabilityCOVIDRemainingDayPD <- filter(WHO_data, Name == Destination)[, "ContractRatePD"]
PofCOVID <- ifelse(LengthOfStay > 7, ProbabilityCOVIDRemainingDayPD * (LengthOfStay - 7) + ProbabilityCOVIDFirst7PD * 7, ProbabilityCOVIDFirst7PD * LengthOfStay)

ConsonanceMultiple <- filter(Consonance, Consonance$List.of.diseases == Conditions)[, 4]
ActualPofCOVID <- ifelse(Vaccination, PofCOVID*(1 - VaccineOnTransmission), PofCOVID)

RateOfCOVID <- read.csv("RateofCOVID.csv")
Age <- floor(as.numeric(Sys.Date() - as.Date(DateOfBirth))/365.25)
HospitalizationLength <- as.numeric(approx(RateOfCOVID$Age, RateOfCOVID[, 4], Age, method = "constant")[2])

DeathRatePD <- filter(WHO_data, Name == Destination)[, "DeathRatePD"]
DeathRate <- (ifelse(Vaccination, DeathRatePD * LengthOfStay * (1- VaccineOnDeath), DeathRatePD * LengthOfStay))*ConsonanceMultiple

# Expected Medical Cost if getting COVID
EM <- filter(MedicalCost, Country == Destination)[, 3] * HospitalizationLength

# Expected PCV medical cost payment if having COVID 
EPP <- ifelse(EM/ HospitalizationLength > 852, 852 * LengthOfStay, EM)
# Expected PCV repatriation cost if death
EPR <- 2553


