# GDP day code
# bea.R documentation: https://github.com/us-bea/bea.R
# API documentation: https://www.bea.gov/API/bea_web_service_api_user_guide.htm

library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(directlabels)
source("BEAR.R")

# Get data
table_list <- c("T10101", "T10102", "T10105", "T10104", "T10106", "T10401", "T10111", "T10501", "T10103", "T20100", "T10506", "T20301", "T20304", "T50302")
gdp_tables <- map_dfr(table_list, ~bea_all(.x))

# Non-annualized change
q_change() %>% 
  arrange(change)
q_change("2") %>% 
  arrange(change)

q_change("6") %>% 
  arrange(date) %>% 
  filter(year(date) >= 2004) %>% 
  rename(level = DataValue) %>% 
  write_csv("service_spending_by_q.csv")

covid_gdp_charts()
q_chart()
q_chart("9", "Business Investment")

# Functions
q_change <- function(line = "1", table = "T10106", df = gdp_tables) {
  df %>% 
    filter(TableName == table,
           LineNumber == line) %>% 
    arrange(date) %>% 
    mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1) %>% 
    select(date, LineDescription, DataValue, change) %>% 
    arrange(desc(date))
}

# Charts
recession_shade <- function(startdate, a = 0.2, latest = "2020-06-01") {
  load("c:/users/208546/Documents/FRED/recession_dates.RData")
  recessions <- recessions %>%
    bind_rows(tibble(Peak = ymd("2020-02-01"),
                     Trough = ymd(latest)))
  geom_rect(data = recessions %>% filter(Peak >= lubridate::ymd(startdate)), inherit.aes = F, 
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = a)
}
  
covid_gdp_charts <- function() {

# Quarterly change
p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>%
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down")) %>% 
  ggplot(aes(date, change, fill = updown)) + geom_col(show.legend = F) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Quarterly Change in Inflation-Adjusted G.D.P.",
       subtitle = "Non-annualized, seasonally adjusted. Shaded areas denote recessions.",
       caption = "Source: Bureau of Economic Analysis") +
  recession_shade("1947-01-01", latest = "2020-06-01") +
  theme(
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 20),
    plot.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "grey50", size = 16),
    axis.ticks = element_line(colour = "grey", size = 0.4),
    plot.caption = element_text(colour = "grey50", size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

ggsave(
  "charts/COVID_quarterly.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


# Levels
low_gdp <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>%
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(current = DataValue[date == max(date)]) %>% 
  filter(DataValue <= current) %>% 
  arrange(desc(date)) 

p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         year(date) >= 2005) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  ggplot(aes(date, DataValue/1000000)) +
  geom_line(show.legend = F, colour = "#00BFC4",
            size = 1) +
  geom_point(aes(y = change/1000000), colour = "#00BFC4", size = 3) +
  geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
                   xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
               linetype = "dashed", size = 1) +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x - 1,
                                 y = y - .5),
                        "last.points", cex = 1.5),
          colour = "#00BFC4") +
  recession_shade("2005-01-01", latest = "2020-06-01")

p <- p +
  labs(x = NULL, y = NULL,
       title = "Inflation-Adjusted Gross Domestic Product",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
  ylim(c(10, 20)) +
  theme(
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 20),
    plot.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.4),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour = "grey50", size = 16),
    axis.ticks = element_line(colour = "grey", size = 0.4),
    plot.caption = element_text(colour = "grey50", size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none"
  )

ggsave(
  "charts/COVID_levels.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

}

q_chart <- function(line = "2", label = "Consumer Spending") {
  p <- gdp_tables %>% 
    filter(TableName == "T10106",
           LineNumber == line) %>% 
    arrange(date) %>% 
    mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
           updown = case_when(change < 0 ~ "down")) %>%
    filter(year(date) >= 2005) %>% 
    ggplot(aes(date, change, fill = updown)) + geom_col(show.legend = F) +
    scale_y_continuous(labels = percent) +
    geom_hline(yintercept = 0) 
  
  p <- p +
    labs(x = NULL, y = NULL,
         title = paste0("Quarterly Change in ", label),
         subtitle = "Non-annualized, seasonally and inflation adjusted. Shaded areas denote recessions.",
         caption = "Source: Bureau of Economic Analysis") +
    recession_shade("2005-01-01", latest = "2020-06-01") +
    theme(
      plot.title = element_text(size = 26),
      plot.subtitle = element_text(size = 20),
      plot.background = element_rect(fill = "white"),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey", size = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "grey50", size = 16),
      axis.ticks = element_line(colour = "grey", size = 0.4),
      plot.caption = element_text(colour = "grey50", size = 16),
      strip.text = element_text(size = 16),
      legend.position = "none"
    )
  
  filename <- paste0("charts/COVID_", label, ".png")
  
  ggsave(
    filename,
    p,
    device = "png",
    width = 14.2,
    height = 8
  )
  
}