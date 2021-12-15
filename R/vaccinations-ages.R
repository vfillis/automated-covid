library(jsonlite)
library(httr)
library(dplyr)
library(zoo)
library(tidyr)
library(lubridate)
library(stringr)

# Covid vaccinations in England: percentage per dose per age group


#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
  endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
  results      <- list()
  current_page <- 1
  
  repeat {
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      timeout(10)
    ) -> response
    
    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1;
  }
  
  return(results)
  
}
# Create filters:
query_filters <- c(
  "areaType=nation",
  "areaName=England"
)
# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName",
  #code       = "areaCode"
  vaccination.ages = "vaccinationsAgeDemographics"
)
vaccinations.England.ages <- get_paginated_data(query_filters, query_structure)
list(
  "Shape"                = dim(vaccinations.England.ages),
  "Data (first 3 items)" = vaccinations.England.ages[0:3, 0:-1]
) -> report
print(report)

## Unnest the data frame

vaccinations.England.ages <- vaccinations.England.ages %>%
  unnest(vaccination.ages)

# Filter for latest data

today = lubridate::today()
right.date = today-3
vaccinations.England.ages.rightdate <- subset(vaccinations.England.ages, vaccinations.England.ages$date == right.date)

## Delete unnecessary columns

vaccinations.England.ages.rightdate <- vaccinations.England.ages.rightdate[-c(5,6,8,10,11,12,13)]

## First dose only 

vaccinations.England.ages.rightdate$FirstDoseOnly <- vaccinations.England.ages.rightdate$cumPeopleVaccinatedFirstDoseByVaccinationDate - vaccinations.England.ages.rightdate$cumPeopleVaccinatedSecondDoseByVaccinationDate 

# No vaccine 

vaccinations.England.ages.rightdate$NoVaccine <- vaccinations.England.ages.rightdate$VaccineRegisterPopulationByVaccinationDate - vaccinations.England.ages.rightdate$cumPeopleVaccinatedFirstDoseByVaccinationDate

# Combine age groups 

# our age groups: 12-15, 16-17, 18-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80-89, 90+

# 12 to 15
vaccinations.12to15 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "12_15")

vaccinations.12to15 <- vaccinations.12to15 %>% 
  rename(
    sum.vaccineRegister = VaccineRegisterPopulationByVaccinationDate, 
    sum.secondDose = cumPeopleVaccinatedSecondDoseByVaccinationDate,
    sum.firstDoseOnly = FirstDoseOnly,
    sum.noVaccine = NoVaccine
  ) %>%
  mutate_at("age", str_replace, "12_15", "12-15") %>% 
  select(-5)


# 16 to 17
vaccinations.16to17 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "16_17")

vaccinations.16to17 <- vaccinations.16to17 %>% 
  rename(
    sum.vaccineRegister = VaccineRegisterPopulationByVaccinationDate, 
    sum.secondDose = cumPeopleVaccinatedSecondDoseByVaccinationDate,
    sum.firstDoseOnly = FirstDoseOnly,
    sum.noVaccine = NoVaccine
  ) %>%
  mutate_at("age", str_replace, "16_17", "16-17") %>% 
  select(-5)

# 18 to 29
vaccinations.18to29 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "18_24" | vaccinations.England.ages.rightdate$age == "25_29")

vaccinations.18to29 <- vaccinations.18to29 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "25_29", "18-29") %>% 
  select(-c(4,5,6,7,8))

# 30 to 39
vaccinations.30to39 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "30_34" | vaccinations.England.ages.rightdate$age == "35_39")

vaccinations.30to39 <- vaccinations.30to39 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "35_39", "30-39") %>% 
  select(-c(4,5,6,7,8))

# 40 to 49 
vaccinations.40to49 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "40_44" | vaccinations.England.ages.rightdate$age == "45_49")

vaccinations.40to49 <- vaccinations.40to49 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "45_49", "40-49") %>% 
  select(-c(4,5,6,7,8))

# 50 to 59
vaccinations.50to59 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "50_54" | vaccinations.England.ages.rightdate$age == "55_59")

vaccinations.50to59 <- vaccinations.50to59 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "55_59", "50-59") %>% 
  select(-c(4,5,6,7,8))

# 60 to 69
vaccinations.60to69 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "60_64" | vaccinations.England.ages.rightdate$age == "65_69")

vaccinations.60to69 <- vaccinations.60to69 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "65_69", "60-69") %>% 
  select(-c(4,5,6,7,8))

# 70 to 79
vaccinations.70to79 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "70_74" | vaccinations.England.ages.rightdate$age == "75_79")

vaccinations.70to79 <- vaccinations.70to79 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "75_79", "70-79") %>% 
  select(-c(4,5,6,7,8))

# 80 to 89
vaccinations.80to89 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "80_84" | vaccinations.England.ages.rightdate$age == "85_89")

vaccinations.80to89 <- vaccinations.80to89 %>% 
  group_by(date) %>%
  mutate(sum.vaccineRegister = sum(VaccineRegisterPopulationByVaccinationDate)) %>%
  mutate(sum.secondDose = sum(cumPeopleVaccinatedSecondDoseByVaccinationDate)) %>%
  mutate(sum.firstDoseOnly = sum(FirstDoseOnly)) %>%
  mutate(sum.noVaccine = sum(NoVaccine)) %>%
  slice(-1) %>%
  mutate_at("age", str_replace, "85_89", "80-89") %>% 
  select(-c(4,5,6,7,8))

# 90+
vaccinations.90 <- subset(vaccinations.England.ages.rightdate, vaccinations.England.ages.rightdate$age == "90+")

vaccinations.90 <- vaccinations.90 %>% 
  rename(
    sum.vaccineRegister = VaccineRegisterPopulationByVaccinationDate, 
    sum.secondDose = cumPeopleVaccinatedSecondDoseByVaccinationDate,
    sum.firstDoseOnly = FirstDoseOnly,
    sum.noVaccine = NoVaccine
  ) %>% 
  select(-5)

# Merge rows 

vaccinations.England.grouped <- rbind(vaccinations.12to15, vaccinations.16to17, vaccinations.18to29, vaccinations.30to39, vaccinations.40to49, vaccinations.50to59, vaccinations.60to69, vaccinations.70to79, vaccinations.80to89, vaccinations.90)

# calculate % 

vaccinations.England.grouped$`Two doses` <- vaccinations.England.grouped$sum.secondDose / vaccinations.England.grouped$sum.vaccineRegister *100
vaccinations.England.grouped$`First dose only` <- vaccinations.England.grouped$sum.firstDoseOnly / vaccinations.England.grouped$sum.vaccineRegister *100
vaccinations.England.grouped$`No vaccine`  <- vaccinations.England.grouped$sum.noVaccine / vaccinations.England.grouped$sum.vaccineRegister *100


# export!!!! 

write.csv(vaccinations.England.grouped, file = "data/vaccinations-England-ages.csv")
