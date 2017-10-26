library(bea.R)
library(tidyverse)
library(lubridate)

# bea.R documentation: https://github.com/us-bea/bea.R
# API documentation: https://www.bea.gov/API/bea_web_service_api_user_guide.htm
# NIPA tables: https://bea.gov/national/pdf/dpga.pdf

load("bea_key.RData")

# Basic method
beaSpecs <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10101',
  'Frequency' = 'Q',
  'Year' = 'X',
  'ResultFormat' = 'json'
)

test <- beaGet(beaSpecs, asWide = F)

# Function for standard call: All available dates, in tidy format

bea_all <- function(table, freq) {
  beaSpecs <- list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'NIPA',
    'TableName' = table,
    'Frequency' = freq,
    'Year' = 'X',
    'ResultFormat' = 'json'
  ) 
  df <- beaGet(beaSpecs, asWide = F)
  df <- df %>% mutate(date = ymd(paste0(substr(TimePeriod, 1, 4), "-", as.numeric(substr(TimePeriod, 6,6))*3-2, "-01"))) %>% 
    select(date, TableName, SeriesCode, LineNumber, LineDescription, CL_UNIT, DataValue) %>% as.tibble(.)
  return(df)
}

# Percent change at SAAR: (table 1)

gdp_saar <- bea_all("T10101", "Q")
