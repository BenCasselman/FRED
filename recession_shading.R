# Recession shading
library(tidyverse)
library(lubridate)

recessions <- read_csv("recession_dates.csv")
save(recessions, file = "recession_dates.RData")

p + geom_rect(data = recessions %>% filter(Peak >= ymd(startdate)), inherit.aes = F, 
  aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = 0.2)


recession_shade <- function(startdate) {
  load("C:/Users/208546/Documents/FRED/recession_dates.RData")
  geom_rect(data = recessions %>% filter(Peak >= lubridate::ymd(startdate)), inherit.aes = F, 
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = 0.2)
}

p + recession_shade(startdate)
