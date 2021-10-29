install.packages("httr")

library(jsonlite)
library(httr)
library(dplyr)
library(zoo)

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

