library(tidyverse)
library(httr)

# observations documentation: https://research.stlouisfed.org/docs/api/fred/series_observations.html

# Make selections
series_id <- "GNPC" # Only required entry
startdate <- "1990-01-01"
enddate <- "" # Leave blank if want latest data
units <- ""
freq <- "q"

fred_setup <- function(...) {
  params <- list(...)
  load("fred_key.RData")
  params$api_key <- fred_key
  params$file_type <- "json"
  resp <- httr::GET(
    url = "https://api.stlouisfed.org/",
    path = "fred/series/observations",
    query = params
  )
  return(resp)
  print(resp$url)

}

test <- fred_setup(series_id = "GNPCA", observation_start = "1990-01-01", units = "")
parsed <- content(test)

frame <- bind_rows(parsed$observations)
    
  base <- "https://api.stlouisfed.org/fred/series/observations"
  params <- paste0("&api_key", fred_key, "&series_id", series_id, "&observation_start", startdate, "&observation_end", enddate)

  
url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=", fred_key, "&file_type=json")

test <- GET(url)

parsed <- content(test)

frame <- bind_rows(parsed$observations)

frame %>% mutate(date = as.Date(date), value = as.numeric(value)) %>% 
  ggplot(., aes(date, value)) + geom_line()