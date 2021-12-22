library(jsonlite)
library(httr)
library(dplyr)
library(zoo)
library(tidyr)
library(lubridate)
library(stringr)

raw <- read.csv("https://github.com/robert-koch-institut/SARS-CoV-2_Infektionen_in_Deutschland/raw/master/Aktuell_Deutschland_SarsCov2_Infektionen.csv", stringsAsFactors = FALSE)

population <- read.csv("https://raw.githubusercontent.com/vfillis/automated-covid/main/data/gemeinde-info.csv", stringsAsFactors = FALSE)
population <- population[(-1)]

raw.berlin <- raw %>%
  filter(between(IdLandkreis, 11001, 11012)) %>%
  group_by(Meldedatum) %>%
  summarise(sum.cases = sum(AnzahlFall)) 

raw.berlin$IdLandkreis <- 11000

# sum for each day for each landkreis
cases.sum.daily <- raw %>% 
  group_by(IdLandkreis, Meldedatum) %>%
  summarise(sum.cases = sum(AnzahlFall)) 

# merge with berlin 

cases.sum.daily.all <- rbind(cases.sum.daily, raw.berlin)

#NAs as 0 
cases.sum.daily.all <- cases.sum.daily.all %>% drop_na()

# rolling sum for each landkreis

cases.sum.rolling <- cases.sum.daily.all %>% 
  group_by(IdLandkreis) %>%
  arrange(desc(Meldedatum)) %>%
  mutate(sevenDaySum = rollsum(sum.cases, k = 7, align="left", fill = NA))

caserates <- merge(population, cases.sum.rolling, by = "IdLandkreis")

caserates <- caserates[-c(3,4,8,9)]

caserates$caserate <- caserates$sevenDaySum / caserates$Bevölkerunginsgesamt * 100000

caserates.latest <- caserates %>% 
  group_by(IdLandkreis) %>% 
  slice_max(Meldedatum)

write.csv(caserates.latest, file = "data/case-rates-latest.csv")

comparison <- caserates[-c(1,2,4,5,7,8)]

comparison <- comparison %>%
  pivot_wider(names_from = Region, values_from = caserate)

write.csv(comparison, file = "data/comparison-data-LKs.csv")
