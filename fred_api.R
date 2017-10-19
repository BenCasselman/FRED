library(tidyverse)
library(httr)

# observations documentation: https://research.stlouisfed.org/docs/api/fred/series_observations.html
# Make sure function fred_setup, fred_tidy, and fred_mult are all loaded. Function below if they aren't.
# You need an API key, saved as object 'fred_key' in file 'fred_key.RData'

# Make selections

# This entry is REQUIRED. Can included multiple series
series_id <- c("PAYEMS", "USPRIV")  # Only required entry

# These are all optional. These must be consistent for all series IDs.
startdate <- "1980-01-01"
enddate <- "" # Leave blank for latest data
units <- "chg" # Options are lin = levels, chg = change, ch1 = change from year ago, pch = pct change, pc1 = pct change from year ago, pca = compounded annual rate of change, cch = continuously compounded rate of change, cca = continuously compounded annual rate of change, log = natural log
freq <- "" # options are d = daily, w = weekly, bw = biweekly, m = monthly, q = quarterly, sa = semiannual, a = annual
agg <- "" # Aggregation method. Options are avg = average, sum = sum, eop = end of period
output <- 1 # Output type. Defaults to 1. Other options are:
# 1 = Observations by Real-Time Period
# 2 = Observations by Vintage Date, All Observations
# 3 = Observations by Vintage Date, New and Revised Observations Only
# 4 = Observations, Initial Release Only
vintages <- "" # comma-separated string of vintage dates
realtime_start <- "" # Probably won't use these much. See explanation here: https://research.stlouisfed.org/docs/api/fred/realtime_period.html
realtime_end <- ""

# Assuming you just want the tidy data, run this:
df <- fred_mult(series_id = series_id, 
                  observation_start = startdate, 
                  observation_end = enddate, 
                  units = units, 
                  frequency = freq, 
                  aggregation_method = agg,
                  output_type = output,
                  vntage_dates = vintages,
                  realtime_start = realtime_start,
                  realtime_end = realtime_end)

# Example:
p <- ggplot(df, aes(date, value, fill = series_id)) + geom_bar(stat = "identity", position = "dodge")
p + recession_shade(startdate)

# Functions!
# This function pulls the relevant data from the API. 
# Run it alone if you want anything other than the straight tidy data (for ex., if you're using vintages).
fred_setup <- function(series_id, ...) {
  params <- list(...)
  load("fred_key.RData")
  params$api_key <- fred_key
  params$file_type <- "json"
  params$series_id <- series_id
  resp <- httr::GET(
    url = "https://api.stlouisfed.org/",
    path = "fred/series/observations",
    query = params
  )
  return(resp)
}

# This function extracts just the values and dates and collects it in a tidy way.
fred_tidy <- function(series_id, ...) {
  raw <- fred_setup(series_id, ...)
  parsed <- content(raw)
  frame <- bind_rows(parsed$observations)
  frame <- frame %>% select(-realtime_start, -realtime_end) %>% 
    mutate(series_id = series_id,
           date = as.Date(date),
           value = as.numeric(value))
  return(frame)
}

# Allow for multiple series and combine into one tidy DF.
fred_mult <- function(series_id, ...) {
  df <- tibble(date = as.Date(character()),
               value = as.numeric(),
               series_id = character())
  for (i in 1:length(series_id)) {
    series <- series_id[i]
    a <- fred_tidy(series, ...)
    df <- bind_rows(df, a)
  }
  return(df)
}


