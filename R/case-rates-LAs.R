library(jsonlite)
library(httr)
library(dplyr)
library(zoo)
library(tidyr)
library(lubridate)
library(stringr)

# Cases by specimen date in local authorities: for table Upper-tier local authorities

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
  "areaType=utla"
)
# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily      = "newCasesBySpecimenDate"
)
cases.upperLA <- get_paginated_data(query_filters, query_structure)
list(
  "Shape"                = dim(cases.upperLA),
  "Data (first 3 items)" = cases.upperLA[0:3, 0:-1]
) -> report
print(report)


### Cases Lower-tier local authorities

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
  "areaType=ltla"
)
# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily      = "newCasesBySpecimenDate"
)
cases.lowerLA <- get_paginated_data(query_filters, query_structure)
list(
  "Shape"                = dim(cases.lowerLA),
  "Data (first 3 items)" = cases.lowerLA[0:3, 0:-1]
) -> report
print(report)

## Remove local authorities from Scotland and Wales

cases.upperLA$England <- grepl("[E]+", cases.upperLA$code)
cases.lowerLA$England <- grepl("[E]+", cases.lowerLA$code)

#Remove rows for LAs in Scotland and Wales
cases.upperLA <- subset(cases.upperLA, cases.upperLA$England == TRUE)
cases.lowerLA <- subset(cases.lowerLA, cases.lowerLA$England == TRUE)

## Merge upper and lower tier cases

cases.alltiers <- rbind(cases.lowerLA, cases.upperLA)

### Remove duplicate rows

cases.alltiers.distinct <- distinct(cases.alltiers)

## Rate of positive cases per 100k population: Import population estimates from mid 2020

population <- read.csv("https://raw.githubusercontent.com/vfillis/automated-covid/main/data/uk-population.csv", stringsAsFactors = FALSE)

# Lookup the population for the regions

# Note: In the population estimates data set from the ONS there are no population estimates for Aylesbury Vale, Chiltern, South Bucks, and Wycombe in Buckinghamshire. Instead they are all summarised under Buckinghamshire. This is why the **cases.alltiers.pop** data frame has less rows than the previous **cases.alltiers.distinct** data frame. 

cases.population <- merge(cases.alltiers.distinct, population, by = "name")
#Dropping duplicate columns
cases.population  <- cases.population[-c(5,6,7)]
#Rename column with populations estimates
cases.population  <- cases.population  %>%
  rename(
    code = code.x
  )


### Calculate cases per 100k

cases.population$case.rate <- (cases.population$daily/cases.population$Population)*100000

### Looking at cases in last 14 days 

## Variables for dates

today = lubridate::today()
lastweek.lastday = today-3
lastweek.firstday = lastweek.lastday-6
penultimateweek.lastday = today-10
penultimateweek.firstday = penultimateweek.lastday-6

cases.lastweek <- subset(cases.population, cases.population$date >= lastweek.firstday & cases.population$date <= lastweek.lastday)
cases.penultimateweek <- subset(cases.population, cases.population$date >= penultimateweek.firstday & cases.population$date <= penultimateweek.lastday)

### Sum of case rate in the last and penultimate seven days for each LA

sum.cases.lastweek <- cases.lastweek %>%
  group_by(name) %>%
  summarise(sum.caserate = sum(case.rate))
sum.cases.penultimateweek <- cases.penultimateweek %>%
  group_by(name) %>%
  summarise(sum.caserate = sum(case.rate))

#Join both data frames
sum.caserate.alltiers <- left_join(sum.cases.lastweek, sum.cases.penultimateweek, by = "name", suffix = c(".lastweek", ".penultimateweek"))

#Change between last and penultimate week 
sum.caserate.alltiers$change <- sum.caserate.alltiers$sum.caserate.lastweek - sum.caserate.alltiers$sum.caserate.penultimateweek

#percentage change
sum.caserate.alltiers$percentagechange <- (sum.caserate.alltiers$change/sum.caserate.alltiers$sum.caserate.penultimateweek)*100

# rename columns 

sum.caserate.alltiers <- sum.caserate.alltiers %>%
  rename(
    
  )

# trim down decimal points: only full numbers + add % to percentage change

## Export 

write.csv(sum.caserate.alltiers, file = "data/case-rates-LAs.csv")



