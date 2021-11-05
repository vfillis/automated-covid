library(jsonlite)
library(httr)
library(dplyr)
library(zoo)

# Covid cases in England 

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
  "areaType=nation"
)
# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  daily     = "newCasesByPublishDate",
  cumulative = "cumCasesByPublishDate"
)
cases <- get_paginated_data(query_filters, query_structure)
list(
  "Shape"                = dim(cases),
  "Data (first 3 items)" = cases[0:3, 0:-1]
) -> report
print(report)
#Rename columns for better distinction
cases <- cases %>%
  rename(
    `Daily cases by date reported` = daily,
    `Cumulative cases by date reported` = cumulative
  )

cases.England <- subset(cases, cases$name == "England")

cases.England <- cases.England %>%
  #dplyr::arrange(desc(date)) %>% 
  dplyr::mutate(`Seven-day average` = zoo::rollmean(`Daily cases by date reported`, k = 7, align="left", fill = NA)) 

write.csv(cases.England, file = "data/cases-England.csv")

# Covid deaths in England 
#Deaths within 28 days of positive test by date reported

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
  code       = "areaCode", 
  daily      = "newDeaths28DaysByPublishDate",
  cumulative = "cumDeaths28DaysByPublishDate"
)
deaths.England <- get_paginated_data(query_filters, query_structure)
list(
  "Shape"                = dim(deathsEngland),
  "Data (first 3 items)" = deathsEngland[0:3, 0:-1]
) -> report
print(report)
#Rename columns for better distinction
deaths.England <- deaths.England %>%
  rename(
    `Daily deaths by date reported` = daily,
    `Cumulative deaths by date reported` = cumulative
  )

deaths.England <- deaths.England %>%
  dplyr::mutate(`Seven-day average` = zoo::rollmean(`Daily deaths by date reported`, k = 7, align="left", fill = NA)) 

write.csv(deaths.England, file = "data/deaths-England.csv")

# Covid hospitalisations in England 

# includes: 
# - New admissions: Daily numbers of COVID-19 patients admitted to hospital
# - COVID-19 occupied beds with mechanical ventilators: Confirmed COVID-19 patients in mechanical ventilation beds
# - Hospital cases: Daily count of confirmed COVID-19 patients in hospital at midnight the preceding night

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
  code       = "areaCode", 
  admissions = "newAdmissions",
  mvbeds     = "covidOccupiedMVBeds",
  hospcases  = "hospitalCases"
)
healthcare.England <- get_paginated_data(query_filters, query_structure)
list(
  "Shape"                = dim(healthcareEngland),
  "Data (first 3 items)" = healthcareEngland[0:3, 0:-1]
) -> report
print(report)
#Rename columns for better distinction
healthcare.England <- healthcare.England %>%
  rename(
    `Daily new hospital admissions` = admissions,
    `Daily people in MV beds` = mvbeds,
    `Daily people in hospital with coronavirus` = hospcases
  )

healthcare.England <- healthcare.England %>%
  dplyr::mutate(`Seven-day average hospital admissions` = zoo::rollmean(`Daily new hospital admissions`, k = 7, align="left", fill = NA)) %>%
  dplyr::mutate(`Seven-day average MV beds` = zoo::rollmean(`Daily people in MV beds`, k = 7, align="left", fill = NA)) %>%
  dplyr::mutate(`Seven-day average hospital cases` = zoo::rollmean(`Daily people in hospital with coronavirus`, k = 7, align="left", fill = NA)) 

write.csv(healthcare.England, file = "data/healthcare-England.csv")

# Covid vaccinations in England 

