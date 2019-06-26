library(tidyverse)
library(lubridate)
library(stringi)

load_all_property_data <- function() {
  rawPropertyData <- read_delim('Cambridge_Property_Database_FY16-FY19.csv', 
                                delim = ',', guess_max = 10000)

  factorCols <- c(
  "PID",
    "StateClassCode",
    "PropertyClass",
    "Zoning",
    "TaxDistrict",
    "ResidentialExemption",
    "Owner_State",
    "Exterior_Style",
    "Exterior_occupancy",
    "Exterior_WallType",
    "Exterior_RoofType",
    "Exterior_RoofMaterial",
    "Interior_Flooring",
    "Interior_Layout",
    "Interior_LaundryInUnit",
    "Systems_HeatType",
    "Systems_HeatFuel",
    "Systems_CentralAir",
    "Systems_Plumbing",
    "Condition_InteriorCondition",
    "Condition_OverallCondition",
    "Condition_OverallGrade"
  )
  
  propertyData <- rawPropertyData %>% mutate_at(factorCols, as.factor)
  
  propertyData <- propertyData %>% mutate(
    GPSLoc = stri_extract_last_regex(Address, "\\(.*\\)"),
    ShortAddress = str_remove(Address, "\\n.*\\n.*"))
  
  baseDate <- as_date("01/01/1900", tz = "UTC", format = "%m/%d/%Y")
  
  propertyData <- propertyData %>%
    mutate(SaleYear = as.integer(stri_extract_last_regex(SaleDate, "[0-9]{4,5}"))) %>% 
    mutate(SaleDate = na_if(SaleDate, "01/05/0001")) %>% 
    mutate(SaleDate = na_if(SaleDate, "01/01/1900")) %>%
    mutate(SaleDate = if_else(
      SaleYear > 2100, 
      baseDate + as.period(SaleYear, unit = "days"), 
      as_date(SaleDate, tz = "UTC", format = "%m/%d/%Y")))
  
  return(propertyData)
}
