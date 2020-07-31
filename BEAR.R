# bea.R documentation: https://github.com/us-bea/bea.R
# API documentation: https://www.bea.gov/API/bea_web_service_api_user_guide.htm
# NIPA tables: https://bea.gov/national/pdf/dpga.pdf

# Basic method
load("~/FRED/bea_key.RData")
beaSpecs <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10101',
  'Frequency' = 'Q',
  'Year' = 'X',
  'ResultFormat' = 'json'
)

# test <- beaGet(beaSpecs, asWide = F)

# Function for standard call: All available dates, in tidy format

bea_all <- function(table, freq = 'Q', dataset = 'NIPA') {
  library(bea.R)
  library(tidyverse)
  library(lubridate)
  
  load("~/FRED/bea_key.RData")
  
  beaSpecs <- list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = dataset,
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

# gdp_saar <- bea_all("T10101", "Q")

recession_shade <- function(startdate) {
  load("C:/Users/208546/Documents/FRED/recession_dates.RData")
  geom_rect(data = recessions %>% filter(Peak >= lubridate::ymd(startdate)), inherit.aes = F, 
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = 0.2)
}


# Key numbers
key_numbers <- function() {
  gdp_tables %>% 
    filter(TableName == "T10101") %>% 
    filter(LineNumber %in% c(1, 2, 7, 9, 13, 16, 19, 22)) %>% 
    group_by(LineDescription) %>% 
    mutate(last = lag(DataValue, 1)) %>% 
    filter(date == max(date)) %>% 
    select(LineDescription, date, latest = DataValue, last) %>% 
    mutate(change = ifelse(latest > last, "faster",
                           ifelse(last > latest, "slower", "unchanged")))
}

# Final demand, etc.
final_demand <- function() {
  gdp_tables %>% 
    filter(TableName == "T10401", 
           date >= ymd("2010-01-01"),
           LineNumber %in% c(6, 8)) %>% 
    mutate(LineNumber = factor(LineNumber, levels = c(6,8), labels = c("final_domestic", "final_pvt_domestic"))) %>% 
    select(LineNumber, date, DataValue) %>% 
    spread(LineNumber, DataValue) %>% 
    arrange(desc(date))
}


inflation <- function() {
  gdp_tables %>% 
    filter(TableName == "T20304", 
           date >= ymd("2010-01-01"),
           LineNumber %in% c("1", "25")) %>% 
    mutate(LineNumber = factor(LineNumber, levels = c("1", "25"), labels = c("headline", "core"))) %>% 
    group_by(LineNumber) %>% 
    mutate(change = (DataValue/lag(DataValue, 1))^4 -1) %>% 
    select(LineNumber, date, change) %>% 
    spread(LineNumber, change) %>% 
    arrange(desc(date))
}

# Last quarter as good as this one:
last_good <- function(lineno, hilo = "hi", tablename = "T10101") {
  latest <- gdp_tables %>% 
    filter(TableName == tablename, 
           date == max(date), 
           LineNumber == lineno) %>% 
    select(DataValue) %>% 
    as.numeric()
  if (hilo == "hi") {
  gdp_tables %>% 
    filter(TableName == tablename, 
           (date == max(date) | DataValue >= latest), LineNumber == lineno) %>% 
    arrange(desc(date)) %>% 
    select(date, LineDescription, DataValue) 
  } else if (hilo == "lo") {
    gdp_tables %>% 
      filter(TableName == tablename, 
             (date == max(date) | DataValue <= latest), LineNumber == lineno) %>% 
      arrange(desc(date)) %>% 
      select(date, LineDescription, DataValue) 
  }
  
}

bea_trade <- function(indicator = "All", country = "AllCountries", freq = 'All', year = "All", dataset = 'ITA') {
  library(bea.R)
  library(tidyverse)
  library(lubridate)
  
  load("~/FRED/bea_key.RData")
  
  beaSpecs <- list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = dataset,
    'Indicator' = indicator,
    'AreaOrCountry' = country,
    'Frequency' = freq,
    'Year' = year,
    'ResultFormat' = 'json'
  ) 
  df <- beaGet(beaSpecs, asWide = F)
  if (nrow(df) == 0) return(NULL)
  
  df <- df %>%
    mutate(date = case_when(Frequency == "A" ~ ymd(paste(Year, 1, 1, sep = "-")),
                            TRUE ~ ymd(paste0(substr(TimePeriod, 1, 4), "-", as.numeric(substr(TimePeriod, 6,6))*3-2, "-01")))) %>%
    select(date, Frequency, Indicator, country = AreaOrCountry, series_name = TimeSeriesDescription, CL_UNIT, DataValue) %>%
    as_tibble(.)
  return(df)
}

saar <- function(var, periods = 1, periods_in_year = 4) {
  (var/lag(var, periods))^(periods_in_year/periods) - 1
}