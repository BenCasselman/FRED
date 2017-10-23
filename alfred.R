library(tidyverse)
library(httr)

# Look at different vintages in ALFRED

# This is the same basic idea as the `fred_setup` function, but with vintage_dates hardwired to allow for multiple entries

alfred_setup <- function(series_id, vintage_dates, ...) {
  params <- list(...)
  load("fred_key.RData")
  params$api_key <- fred_key
  params$file_type <- "json"
  params$series_id <- series_id
  params$vintage_dates <- vintage_dates
  resp <- httr::GET(
    url = "https://api.stlouisfed.org/",
    path = "fred/series/observations",
    query = params
  )
  return(resp)
}


alfred_tidy <- function(series_id, vintage_dates, ...) {
  raw <- alfred_setup(series_id, vintage_dates, ...)
  parsed <- content(raw)
  frame <- bind_rows(parsed$observations)
  frame <- frame %>% select(-realtime_start, -realtime_end) %>% 
    mutate(series_id = series_id,
           vintage = as.Date(vintage_dates),
           date = as.Date(date),
           value = as.numeric(value))
  return(frame)
}

# Allow for multiple series and combine into one tidy DF.
alfred_mult <- function(series_id, vintage_dates, ...) {
  df <- tibble(date = as.Date(character()),
               value = as.numeric(),
               series_id = character(),
               vintage = as.Date(character()))
  for (i in 1:length(series_id)) {
    for (j in 1:length(vintage_dates)){
    series <- series_id[i]
    vintage <- vintage_dates[j]
    a <- alfred_tidy(series, vintage, ...)
    df <- bind_rows(df, a)
    }
  }
  return(df)
}


# Need both of these for ALFRED
series_id <- c("GDPC1CTM", "PCECTPICTM")  
vintages <- c("2010-01-01", "2011-01-01") # comma-separated string of vintage dates

# These are all optional. These must be consistent for all series IDs.
startdate <- "2008-01-01"
enddate <- "" # Leave blank for latest data
units <- "" # Options are lin = levels, chg = change, ch1 = change from year ago, pch = pct change, pc1 = pct change from year ago, pca = compounded annual rate of change, cch = continuously compounded rate of change, cca = continuously compounded annual rate of change, log = natural log
freq <- "" # options are d = daily, w = weekly, bw = biweekly, m = monthly, q = quarterly, sa = semiannual, a = annual
agg <- "" # Aggregation method. Options are avg = average, sum = sum, eop = end of period
output <- 1 # Output type. Defaults to 1. Other options are:
# 1 = Observations by Real-Time Period
# 2 = Observations by Vintage Date, All Observations
# 3 = Observations by Vintage Date, New and Revised Observations Only
# 4 = Observations, Initial Release Only

realtime_start <- "" # Probably won't use these much. See explanation here: https://research.stlouisfed.org/docs/api/fred/realtime_period.html
realtime_end <- ""

df <- alfred_mult(series_id = series_id, 
                vintage_dates = vintages,
                observation_start = startdate, 
                observation_end = enddate, 
                units = units, 
                frequency = freq, 
                aggregation_method = agg,
                output_type = output,
                realtime_start = realtime_start,
                realtime_end = realtime_end)
