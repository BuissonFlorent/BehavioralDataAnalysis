### Part II Chapter 5 : Building CDs from scratch
## chap5 - loading and prepping data.R
##
## This script loads the data from Antonio, de Almeida & Nunes, 
## “Hotel booking demand datasets”, Data in Brief, 2019.  https://doi.org/10.1016/j.dib.2018.11.126.
## and gets it prepped for analysis

library(tidyverse)
setwd("C:/Users/Florent/Dropbox/Synchronised/Work_and_projects/Behavioral data science book/R scripts/Part II Analyzing observational data/Chapter 5 - Building CDs from scratch")

#Loading the datasets
data_loading_fun <- function(){
  dat_h1 <- read_csv("H1.csv", col_types = cols(
    IsCanceled = col_double(),
    LeadTime = col_double(),
    ArrivalDateYear = col_double(),
    ArrivalDateMonth = col_character(),
    ArrivalDateWeekNumber = col_double(),
    ArrivalDateDayOfMonth = col_double(),
    StaysInWeekendNights = col_double(),
    StaysInWeekNights = col_double(),
    Adults = col_double(),
    Children = col_double(),
    Babies = col_double(),
    Meal = col_character(),
    Country = col_character(),
    MarketSegment = col_character(),
    DistributionChannel = col_character(),
    IsRepeatedGuest = col_double(),
    PreviousCancellations = col_double(),
    PreviousBookingsNotCanceled = col_double(),
    ReservedRoomType = col_character(),
    AssignedRoomType = col_character(),
    BookingChanges = col_double(),
    DepositType = col_character(),
    Agent = col_character(),
    Company = col_character(),
    DaysInWaitingList = col_double(),
    CustomerType = col_character(),
    ADR = col_double(),
    RequiredCarParkingSpaces = col_double(),
    TotalOfSpecialRequests = col_double(),
    ReservationStatus = col_character(),
    ReservationStatusDate = col_date(format = "")
  ))
  
  dat_h2 <- read_csv("H2.csv", col_types = cols(
    IsCanceled = col_double(),
    LeadTime = col_double(),
    ArrivalDateYear = col_double(),
    ArrivalDateMonth = col_character(),
    ArrivalDateWeekNumber = col_double(),
    ArrivalDateDayOfMonth = col_double(),
    StaysInWeekendNights = col_double(),
    StaysInWeekNights = col_double(),
    Adults = col_double(),
    Children = col_double(),
    Babies = col_double(),
    Meal = col_character(),
    Country = col_character(),
    MarketSegment = col_character(),
    DistributionChannel = col_character(),
    IsRepeatedGuest = col_double(),
    PreviousCancellations = col_double(),
    PreviousBookingsNotCanceled = col_double(),
    ReservedRoomType = col_character(),
    AssignedRoomType = col_character(),
    BookingChanges = col_double(),
    DepositType = col_character(),
    Agent = col_character(),
    Company = col_character(),
    DaysInWaitingList = col_double(),
    CustomerType = col_character(),
    ADR = col_double(),
    RequiredCarParkingSpaces = col_double(),
    TotalOfSpecialRequests = col_double(),
    ReservationStatus = col_character(),
    ReservationStatusDate = col_date(format = "")
  ))
  dat <- rbind(dat_h1, dat_h2)
  #Removing duplicates
  dat <- distinct(dat)
  
  return(dat)
}

dat <- data_loading_fun()

##### Creating new variables #####

#Binary variable for No Deposit
dat <- dat %>%
  mutate(NRDeposit = as.integer(DepositType == "Non Refund"))

#Creating variable for quarter
dat <- dat %>%
  mutate(Quarter = case_when(
    ArrivalDateMonth %in% c("January", "February", "March") ~ "Q1",
    ArrivalDateMonth %in% c("April", "May", "June") ~ "Q2",
    ArrivalDateMonth %in% c("July", "August", "September") ~ "Q3",
    ArrivalDateMonth %in% c("October", "November", "December") ~ "Q4"
  ))

#Creating joint variable for Children and Babies
dat <- dat %>%
  mutate(Children = Children + Babies) %>%
  mutate(Children = ifelse(is.na(Children), 0, Children))

#Filtering out outlier in ADR
dat <- dat %>%
  filter(ADR<= 1000)

#Recategorizing Distribution channel and Market segment variables
dat <- dat %>%
  mutate(DistributionChannel = ifelse(DistributionChannel == "Undefined" | 
                                        DistributionChannel == "GDS","Other", DistributionChannel)) %>%
  mutate(MarketSegment = ifelse(MarketSegment %in% c("Complementary","Aviation","Undefined", "(Other)"),
                                "Other", 
                                MarketSegment))

#Binarizing Previous cancellation
dat <- dat %>%
  mutate(PreviousCancellations = ifelse(PreviousCancellations == 0, 0, 1)) 

#Selecting only variables used
dat <- dat %>%
  rename(Year = ArrivalDateYear) %>%
  select(NRDeposit, IsCanceled, DistributionChannel, MarketSegment, CustomerType,
         Children, ADR, PreviousCancellations, IsRepeatedGuest, Country, 
         Quarter, Year)

#Saving the prepped data
write_csv(dat, "chap5-hotel_booking_case_study.csv")