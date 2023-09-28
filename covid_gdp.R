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
table_list <- c("T10101", "T10102", "T10105", "T10104", "T10106", "T10401", "T10111", "T10501", "T10103", "T20100", "T10506", "T20301", "T20304", "T50302", "T11706", "T10109")
table_list <- c("T10101", "T10102", "T10105", "T10104", "T10106", "T10401", "T10111", "T10501", "T10103", "T10506")

gdp_tables <- map_dfr(table_list, ~bea_all(.x))

# w <- bea_all("T10101")

# Non-annualized change/gap
q_change()
q_change("13") 
q_change("2",table = "T10105")

# q_change("6") %>% 
#   arrange(date) %>% 
#   filter(year(date) >= 2004) %>% 
#   rename(level = DataValue) %>% 
#   write_csv("service_spending_by_q.csv")

covid_gdp_charts()
q_chart()
q_chart("9", "Business Investment")

# Functions
q_records <- function(line = "1", table = "T10106", df = gdp_tables) {
  df %>%
    filter(TableName == table,
           LineNumber == line) %>%
    arrange(date) %>%
    mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1) %>%
    select(date, LineDescription, DataValue, change) %>%
    arrange(desc(date))
}

q_change <- function(line = "1", table = "T10106", df = gdp_tables) {
  df %>% 
    filter(TableName == table,
           LineNumber == line,
           date >= ymd("2019-10-01")) %>% 
    arrange(date) %>% 
    mutate(q_change = 100*(DataValue/lag(DataValue, 1, na.pad = T) -1),
           cum_change = 100*(DataValue/first(DataValue) - 1)) %>% 
    select(date, LineDescription, DataValue, q_change, cum_change) %>% 
    arrange(desc(date))
}

# Charts
recession_shade <- function(startdate, a = 0.2, latest = "2021-01-01") {
  load("c:/users/208546/Documents/FRED/recession_dates.RData")
  # recessions <- recessions %>%
  #   bind_rows(tibble(Peak = ymd("2020-02-01"),
  #                    Trough = ymd(latest)))
  geom_rect(data = recessions %>% filter(Peak >= lubridate::ymd(startdate)), inherit.aes = F, 
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = Inf), alpha = a)
}

covid_gdp_charts <- function() {

# Quarterly change
p <- gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "1") %>% 
  arrange(date) %>% 
  mutate(change = DataValue/100,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         latest = case_when(date == max(date) ~ "latest",
                            TRUE ~ "other")) %>% 
  filter(date >= ymd("2008-01-01")) %>% 
  ggplot(aes(date, change, fill = updown, alpha = latest)) + 
  geom_col(show.legend = F) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1))

p <- p +
  # recession_shade("2008-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly Change in Inflation-Adjusted G.D.P.",
       subtitle = "Seasonally adjusted annual rate. Latest data is preliminary",
       caption = "Source: Bureau of Economic Analysis") +
  # recession_shade("1947-01-01") +
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
  "charts/COVID charts/quarterly_change.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


# Levels
# low_gdp <- gdp_tables %>% 
#   filter(TableName == "T10106",
#          LineNumber == "1") %>%
#   # bind_rows(tibble(date = ymd("2020-04-01"),
#   #                  DataValue = 17039.8*1000)) %>% 
#   arrange(date) %>% 
#   mutate(current = DataValue[date == max(date)]) %>% 
#   filter(DataValue <= current) %>% 
#   arrange(desc(date)) 


# Levels vs trend
p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         year(date) >= 2015) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  vs_trend(DataValue) +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") 


# NEW VERSION, USING CBO DATA
# cbo_project_2020_01 <- read_csv("cbo_project_2020_01.csv")
# cbo_project_2020_01 <- cbo_project_2020_01 %>% 
#   gather(real_nominal, value, -date) %>% 
#   mutate(series = "trend",
#          date = mdy(date))

# p <- gdp_tables %>% 
#   filter(TableName == "T10106",
#          LineNumber == "1",
#          year(date) >= 2015) %>% 
#   # bind_rows(tibble(date = ymd("2020-04-01"),
#   #                  DataValue = 17039.8*1000)) %>% 
#   arrange(date) %>% 
#   mutate(DataValue = DataValue/1000000,
#          label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
#          change = case_when(date == max(date) ~ DataValue)) %>% 
#   select(date, DataValue, label, change) %>% 
#   left_join(cbo_project_2020_01 %>% 
#               filter(series == "trend", real_nominal == "Inflation-adjusted") %>% 
#               select(date, trend = value), by = "date") %>% 
#   ggplot(aes(date, DataValue)) +
#   geom_line(colour = "#1f78b4", size = 1) +
#   geom_line(aes(y = trend/1000), colour = "black", linetype = "dashed", size = 1) +
#   geom_dl(aes(label = label),
#           method = list(dl.trans(x = x + .2),
#                         "last.points", cex = 1.5),
#           colour = "#1f78b4") 


# p <- p +
#   labs(x = NULL, y = NULL,
#        title = "Inflation-Adjusted Gross Domestic Product vs Prepandemic Trend",
#        subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
#        caption = "Source: Bureau of Economic Analysis") +
#   ylim(c(15, 21)) +
#   theme(
#     plot.title = element_text(size = 26),
#     plot.subtitle = element_text(size = 20),
#     plot.background = element_rect(fill = "white"),
#     panel.background = element_blank(),
#     panel.grid.major = element_line(colour = "grey", size = 0.4),
#     panel.grid.minor = element_blank(),
#     axis.text = element_text(colour = "grey50", size = 16),
#     axis.ticks = element_line(colour = "grey", size = 0.4),
#     plot.caption = element_text(colour = "grey50", size = 16),
#     strip.text = element_text(size = 16),
#     legend.position = "none"
#   )
# 
# ggsave(
#   "charts/COVID charts/real_vs_trend.png",
#   p,
#   device = "png",
#   width = 14.2,
#   height = 8
# )

p <- gdp_tables %>% 
  filter(TableName == "T10105",
         LineNumber == "1",
         year(date) >= 2015) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  vs_trend(DataValue) +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Non-Inflation-Adjusted Gross Domestic Product vs Prepandemic Trend",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/nominal_vs_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "2",
         year(date) >= 2015) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  vs_trend(DataValue) +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Inflation-Adjusted Consumer Spending vs Prepandemic Trend",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/real_PCE_vs_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

p <- gdp_tables %>% 
  filter(TableName == "T10105",
         LineNumber == "2",
         year(date) >= 2015) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  vs_trend(DataValue) +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Non-Inflation-Adjusted Consumer Spending vs Prepandemic Trend",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/nominal_PCE_vs_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


p <- gdp_tables %>% 
  filter(SeriesCode == "A713RX",
         year(date) >= 2015) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  vs_trend(DataValue) 
  # geom_point(aes(y = change), colour = "#1f78b4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  # geom_dl(aes(label = label),
  #         method = list(dl.trans(x = x + .2),
  #                       "last.points", cex = 1.5),
  #         colour = "#1f78b4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Inflation-Adjusted Demand vs Prepandemic Trend",
       subtitle = "Real final domestic sales in trillions of dollars, seasonally adjusted.",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/real_demand_vs_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber %in% c("3", "6"),
         date >= ymd("2019-10-01")) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend =  F) +
  geom_hline(yintercept = 0) 

p <- p +
  scale_color_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  scale_x_date(limits = c(ymd("2019-10-01"), ymd("2021-12-01")), 
               date_labels = as.yearqtr) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real consumer spending on goods vs services",
       subtitle = "Change since fourth quarter 2019, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = LineDescription),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
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
    legend.position = "none")

ggsave("charts/COVID charts/goods_v_svces_q.png", p, device = "png", width = 14.2, height = 8)

p <- gdp_tables %>% 
  filter(TableName == "T10401",
         LineNumber == 8) %>% 
  mutate(updown = case_when(DataValue < 0 ~ "down",
                            TRUE ~ "up"),
         latest = case_when(date == max(date) ~ "latest",
                            TRUE ~ "other"),
         DataValue = DataValue/100) %>% 
  filter(date >= ymd("2008-01-01")) %>% 
  ggplot(., aes(date, DataValue, fill = updown, alpha = latest)) + 
  geom_col()

p <- p + 
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1)) +
  # recession_shade("2005-01-01") +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Final domestic private demand (GDP without inventory/trade effects or gov't spending)",
       subtitle = "Seasonally adjusted annual rate of change from prior quarter. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black") +
  theme(
    plot.title = element_text(size = 25),
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

ggsave("~/FRED/charts/COVID charts/final_pvt_demand.png", p, device = "png", width = 14.2, height = 8)


p <- gdp_tables %>% 
  filter(TableName == "T10401",
          LineNumber == 6) %>% 
  mutate(updown = case_when(DataValue < 0 ~ "down",
                            TRUE ~ "up"),
         latest = case_when(date == max(date) ~ "latest",
                            TRUE ~ "other"),
         DataValue = DataValue/100) %>% 
  filter(date >= ymd("2008-01-01")) %>% 
  ggplot(., aes(date, DataValue, fill = updown, alpha = latest)) + 
  geom_col()

p <- p + 
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1)) +
  # recession_shade("2005-01-01") +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Final domestic demand (GDP without inventory/trade effects)",
       subtitle = "Seasonally adjusted annual rate of change from prior quarter. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black") +
  theme(
    plot.title = element_text(size = 25),
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

ggsave("~/FRED/charts/COVID charts/final_demand.png", p, device = "png", width = 14.2, height = 8)


}

q_chart <- function(line = "2", label = "Consumer Spending") {
  p <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == line) %>% 
    arrange(date) %>% 
    mutate(change = DataValue/100,
           updown = case_when(change < 0 ~ "down",
                              TRUE ~ "up"),
           latest = case_when(date == max(date) ~ "latest",
                              TRUE ~ "other")) %>%
    filter(year(date) >= 2005) %>% 
    ggplot(aes(date, change, fill = updown, alpha = latest)) + 
    geom_col(show.legend = F) +
    scale_y_continuous(labels = percent) +
    geom_hline(yintercept = 0) 

  p <- p +
    scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
    scale_alpha_manual(values = c(latest = .5, other = 1)) +
    labs(x = NULL, y = NULL,
         title = paste0("Quarterly Change in ", label),
         subtitle = "Seasonally adjusted annual rate, adjusted for inflation. Shaded areas denote recessions.",
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









# Quarterly annualized jobs
q_jobs <- read_tsv("date	quarter	jobs
10/01/2019	10/01/2019	151553.0
11/01/2019	10/01/2019	151814.0
12/01/2019	10/01/2019	151998.0
01/01/2020	01/01/2020	152212.0
02/01/2020	01/01/2020	152463.0
03/01/2020	01/01/2020	151090.0
04/01/2020	04/01/2020	130303.0
05/01/2020	04/01/2020	133028.0
06/01/2020	04/01/2020	137809.0
07/01/2020	07/01/2020	139570.0
08/01/2020	07/01/2020	141059.0
09/01/2020	07/01/2020	141720.0
") %>% 
  mutate(date = mdy(date))

p <- q_jobs %>% 
  arrange(date) %>% 
  mutate(change = jobs / lag(jobs, 1, na.pad = T) - 1) %>% 
  filter(year(date) == 2020) %>% 
  ggplot(aes(date, change, fill = quarter)) + 
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086"))

p <- p +
  scale_y_continuous(label = percent) +
  geom_hline(yintercept = 0) +
  labs(
    x = NULL,
    y = NULL,
    title = "Monthly job growth",
    subtitle = "Change from prior month, seasonally adjusted",
    caption = "Source: Bureau of Labor Statistics, via FRED"
  ) +
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
  "charts/monthly_jobs.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


p <- q_jobs %>% 
  group_by(quarter) %>% 
  summarize(jobs = sum(jobs)) %>%
  mutate(date = mdy(quarter)) %>% 
  arrange(date) %>% 
  mutate(change = (jobs/lag(jobs, 1, na.pad = T))^4 - 1) %>% 
  filter(year(date) == 2020) %>% 
  ggplot(aes(date, change, fill = quarter)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086")) 

p <- p +
  scale_y_continuous(label = percent) +
  scale_x_date(date_labels = c("Q1", "Q2", "Q3")) +
  geom_hline(yintercept = 0) +
  labs(
    x = NULL,
    y = NULL,
    title = "Quarterly job growth",
    subtitle = "Change from prior quarter, seasonally adjusted annual rate",
    caption = "Source: Bureau of Labor Statistics, via FRED"
  ) +
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
  "charts/quarterly_jobs.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  select(date, LineDescription, DataValue) %>% 
  write_csv("GDP_history.csv")





is_recession <- function(dt) {
  df <- recessions %>% 
    filter(dt >= recessions$Peak)
  if (dt <= df$Trough[1]) {rec <- "recession"} else
    {rec <- "expansion"}
  
  rec
}

recession_compare <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("1980-01-01")) %>% 
  slice(1:20) %>% 
  mutate(quarter = row_number(),
         change = DataValue/first(DataValue) - 1) %>% 
  select(quarter, r_1980 = change)

recession_compare <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("1990-07-01")) %>% 
  slice(1:20) %>% 
  mutate(quarter = row_number(),
         change = DataValue/first(DataValue) - 1) %>% 
  select(quarter, r_1990 = change) %>% 
  left_join(recession_compare, by = "quarter")


recession_compare <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("2001-01-01")) %>% 
  slice(1:20) %>% 
  mutate(quarter = row_number(),
         change = DataValue/first(DataValue) - 1) %>% 
  select(quarter, r_2001 = change) %>% 
  left_join(recession_compare, by = "quarter")

recession_compare <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("2007-10-01")) %>% 
  slice(1:20) %>% 
  mutate(quarter = row_number(),
         change = DataValue/first(DataValue) - 1) %>% 
  select(quarter, r_2007 = change) %>% 
  left_join(recession_compare, by = "quarter")


recession_compare <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("2019-10-01")) %>% 
  slice(1:20) %>% 
  mutate(quarter = row_number(),
         change = DataValue/first(DataValue) - 1) %>% 
  select(quarter, r_2020 = change) %>% 
  right_join(recession_compare, by = "quarter")


p <- recession_compare %>% 
  filter(quarter <= 15) %>% 
  gather(recession, change, -quarter) %>% 
  mutate(recession = factor(recession, levels = c("r_1980", "r_1990", "r_2001",
                                                  "r_2007", "r_2020"),
                            labels = c("1980", "1990", "2001", "2007", "2020"))) %>% 
  ggplot(aes(quarter, change, colour = recession, size = recession)) +
  geom_line(show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(`1980` = "#bdc9e1", `1990` = "#74a9cf", `2001` = "#2b8cbe",
                                `2007` = "#045a8d", `2020` = "#ca0020")) +
  scale_size_manual(values = c(`1980` = 1, `1990` = 1, `2001` = 1,
                               `2007` = 1, `2020` = 1.2))

p <- p +
  scale_y_continuous(labels = percent) +
  labs(x = "Quarters since recession start", y = NULL,
       title = "Output has rebounded much faster than after the last recession",
       subtitle = "Cumulative percent change in inflation-adjusted G.D.P. from the start of past five recessions",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = recession),
          method = list(dl.trans(x = x + .1), "last.points", cex = 1.5))  +
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
    legend.position = "none",
    axis.title = element_text(colour = "grey50", size = 16))
  

ggsave("charts/COVID charts/recession_compare.png", p, device = "png", width = 14.2, height = 8)


  

gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  mutate(recession = map_chr(date, ~is_recession(.x)))


gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  select(date, real_gdp = DataValue) %>% 
  write_csv("gdp_levels_for_ella.csv")

recession_compare %>% 
  write_csv("recession_compare.csv")


p <- gdp_tables %>% 
  filter(TableName == "T20100",
         LineNumber == "1",
         year(date) >= 2015) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  vs_trend(DataValue) +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Personal income, not adjusted for inflation, vs prepandemic trend",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/income_q.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

p <- gdp_tables %>% 
  filter(TableName == "T20100",
         LineNumber == "37",
         year(date) >= 2015) %>% 
  arrange(date) %>% 
  mutate(label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue),
         DataValue = DataValue/1000000) %>% 
  vs_trend(DataValue) +
  # ggplot(aes(date, DataValue/1000000)) +
  # geom_line(show.legend = F, colour = "#00BFC4",
  #           size = 1) +
  geom_point(aes(y = change/1000000), colour = "#1f78b4", size = 3) +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x - 1,
                                 y = y - .5),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "After-Tax Personal Income, Adjusted for Inflation",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/real_dpi.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

  
  
  
p <- gdp_tables %>% 
  filter(TableName == "T20100",
         LineNumber %in% c("1", "2", "16"),
         year(date) >= 2015) %>% 
  arrange(date) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  rename(total = `1`, comp = `2`, transfer = `16`) %>% 
  mutate(other = total - comp - transfer) %>% 
  select(-total) %>% 
  gather(series, value, -date) %>% 
  mutate(series = factor(series, levels = c("transfer", "other", "comp"),
                         labels = c("Transfers", "Other", "Compensation"))) %>% 
  ggplot(aes(date, value/1000000, fill = series)) +
  geom_col()

p <- p +
  scale_fill_manual(values = c("#b2df8a", "#a6cee3", "#1f78b4")) +
  scale_alpha_manual(values = c(1, .5, 1)) +
  labs(x = NULL, y = NULL, 
       title = "Nominal Personal Income, by Source",
       subtitle = "Trillions of dollars, seasonally adjusted annual rates",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave(
  "charts/COVID_income_components_q.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)




p <- gdp_tables %>% 
  filter(TableName == "T20100",
         LineNumber == "34",
         date >= ymd("2015-01-01")) %>% 
  arrange(date) %>% 
  mutate(covid = case_when(year(date) >= 2020 ~ "covid",
                           TRUE ~ "not")) %>% 
  ggplot(aes(date, DataValue/1000000, fill = covid)) +
  geom_col(show.legend = F)

p <- p +
  scale_fill_manual(values = c("#1f78b4", "grey50")) +
  labs(x = NULL, y = NULL, 
       title = "Quarterly Change in Total Personal Savings",
       subtitle = "Trillions of dollars, seasonally adjusted annual rates. Not adjusted for inflation.",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave(
  "charts/COVID charts/savings_q.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


p <- gdp_tables %>% 
  filter(TableName == "T20100",
         LineNumber == "35",
         date >= ymd("2015-01-01")) %>% 
  mutate(DataValue = DataValue/100,
         pre_covid = case_when(date < ymd("2020-01-01") ~ DataValue),
         pre_avg = mean(pre_covid, na.rm = T))

p <- p %>% 
  ggplot(aes(date, DataValue)) +
  geom_line(size = 1, color = "#1f78b4") +
  geom_line(aes(y = pre_avg), linetype = "dashed", colour = "black", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Personal saving rate",
       subtitle = "Savings as a share of after-tax income, seasonally adjusted. Dashed line is pre-Covid average.",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "none")

ggsave("charts/COVID charts/saving_rate_q.png", p, device = "png", width = 14.2, height = 8)


p <- gdp_tables %>% 
  filter(TableName == "T20100",
         LineNumber == "34",
         date >= ymd("2019-01-01")) %>% 
  mutate(cum_savings = cumsum(DataValue/4000000)) %>% 
  select(date, DataValue, cum_savings)

r <- p %>% 
  filter(date < ymd("2020-01-01")) %>% 
  lm(cum_savings ~ date, data = .)

p <- p %>% 
  mutate(trend = map_dbl(date, ~predict(r, newdata = tibble(date = .x))))

p %>% 
  mutate(excess = cum_savings - trend) %>% 
  arrange(desc(date))

p <- p %>% 
  ggplot(aes(date, cum_savings)) +
  geom_line(size = 1, color = "#1f78b4") +
  geom_line(aes(y = trend), linetype = "dashed", size = 1) +
  geom_ribbon(
    # data = subset(p, date >= ymd("2020-01-01")),
              aes(ymin = trend, ymax = cum_savings),
              fill = "#a6cee3", alpha = 0.5)

p <- p +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, 
       title = "Cumulative personal savings in the pandemic",
       subtitle = "Trillions of dollars, seasonally adjusted. Not adjusted for inflation.",
       caption = "Note: Quarterly data, not annualized. | Source: Bureau of Economic Analysis") +
  annotate("text", x = ymd("2021-06-01"), y = 4.8,
           label = "Pandemic savings", colour = "#1f78b4", size = 5.5) +
  geom_curve(aes(x = ymd("2020-04-01"), y = 1.5,
                 xend = ymd("2020-03-01"), yend = 1.7),
             curvature = -.2, arrow = arrow(length = unit(.02, "npc"))) +
  annotate("text", x = ymd("2020-06-05"), y = 1.5,
           label = "Pre-pandemic trend", colour = "black", size = 4.75) +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave(
  "charts/COVID charts/excess_savings.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


r1 <- gdp_tables %>% 
  filter(TableName %in% c("T10105"),
         LineNumber == "1",
         date >= ymd("2017-01-01"),
         date < ymd("2020-01-01")) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  lm(change ~ date, data = .)

r2 <- gdp_tables %>% 
  filter(TableName %in% c("T10106"),
         LineNumber == "1",
         date >= ymd("2017-01-01"),
         date < ymd("2020-01-01")) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  lm(change ~ date, data = .)

p <- gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10106"),
         LineNumber == "1",
         date >= ymd("2017-01-01")) %>% 
  group_by(TableName) %>% 
  mutate(change = DataValue/first(DataValue) - 1,
         trend = case_when(TableName == "T10105" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           TableName == "T10106" ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x)))
         )
  ) %>% 
  ungroup() %>% 
  mutate(TableName = factor(TableName, levels = c("T10105", "T10106"),
                            labels = c("Unadjusted", "Inflation-adjusted"))) 


# NEW VERSION, USING CBO DATA
cbo_project_2020_01 <- read_csv("cbo_project_2020_01.csv")
cbo_project_2020_01 <- cbo_project_2020_01 %>% 
  gather(real_nominal, value, -date) %>% 
  mutate(series = "trend",
         date = mdy(date))

p <- gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10106"),
         LineNumber == "1",
         date >= ymd("2017-01-01")) %>% 
  mutate(real_nominal = factor(TableName, levels = c("T10105", "T10106"),
                            labels = c("Unadjusted", "Inflation-adjusted")),
         series = "value") %>%  
  select(date, series, real_nominal, value = DataValue) %>% 
  bind_rows(cbo_project_2020_01) %>% 
  group_by(series, real_nominal) %>% 
  mutate(change = value/first(value) - 1) 

p %>% 
  select(date, real_nominal, series, change) %>% 
  pivot_wider(names_from = c(series, real_nominal),
              values_from = change) %>% 
  filter(!is.na(value_Unadjusted)) %>% 
  write_csv("real_vs_nominal.csv")

p <- p %>% 
  select(-value) %>% 
  spread(series, change) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(date, value, colour = real_nominal)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = real_nominal), linetype = "dashed", size = 1)

p <- p +
  geom_dl(aes(label = real_nominal),
          method = list(dl.trans(x = x + .1),
                        "last.points", cex = 1.5)) +
  scale_color_manual(values = c(`Inflation-adjusted` = "#1f78b4",
                                Unadjusted = "#33a02c")) +
  xlim(ymd("2017-01-01"), ymd("2023-09-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal GDP vs trend",
       subtitle = "Change since to Q1 2017, seasonally adjusted",
       caption = "Note: Seasonally adjusted. Trends are based on Jan. 2020 CBO forecast. | Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/nominal_vs_real.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)




r1 <- gdp_tables %>% 
  filter(TableName %in% c("T10105"),
         LineNumber == "2",
         date >= ymd("2015-01-01"),
         date < ymd("2020-01-01")) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  lm(change ~ date, data = .)

r2 <- gdp_tables %>% 
  filter(TableName %in% c("T10106"),
         LineNumber == "2",
         date >= ymd("2015-01-01"),
         date < ymd("2020-01-01")) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  lm(change ~ date, data = .)

p <- gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10106"),
         LineNumber == "2",
         date >= ymd("2015-01-01")) %>% 
  group_by(TableName) %>% 
  mutate(change = DataValue/first(DataValue) - 1,
         trend = case_when(TableName == "T10105" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           TableName == "T10106" ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x)))
         )
  ) %>% 
  ungroup() %>% 
  mutate(TableName = factor(TableName, levels = c("T10105", "T10106"),
                            labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  ggplot(aes(date, change, colour = TableName)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = TableName), linetype = "dashed", size = 1)

p <- p +
  geom_dl(aes(label = TableName),
          method = list(dl.trans(x = x + .1),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2015-01-01"), ymd("2023-09-01")) +
  scale_color_manual(values = c(`Inflation-adjusted` = "#1f78b4",
                                Unadjusted = "#33a02c")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal consumer spending vs trend",
       subtitle = "Change since to Q1 2015, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/nominal_vs_real_pce.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


trendline <- function(df = ., datemax, change) {
  r <- df %>% 
    filter(date < datemax) %>% 
    lm(change ~ date, data = .)
  
  df %>% 
    mutate(trend = map_dbl(date, ~predict(r, newdata = tibble(date = .x))))

}


gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10106"),
         LineNumber == "2",
         date >= ymd("2015-01-01")) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  nest(-TableName) %>% 
  mutate(data = trendline(df = data, datemax = ymd("2020-01-01")))

%>% 
  ungroup() %>% 
  mutate(TableName = factor(TableName, levels = c("T10105", "T10106"),
                            labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  ggplot(aes(date, change, colour = TableName)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = TableName), linetype = "dashed", size = 1)




r <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         year(date) >= 2015,
         year(date) < 2020) %>% 
  mutate(DataValue = DataValue/1000000) %>% 
  lm(DataValue ~ date, data = .)

p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         year(date) >= 2015) %>% 
  # bind_rows(tibble(date = ymd("2020-04-01"),
  #                  DataValue = 17039.8*1000)) %>% 
  arrange(date) %>% 
  mutate(DataValue = DataValue/1000000,
         trend = map_dbl(date, ~predict(r, newdata = tibble(date = .x))),
         label = case_when(date == max(date) ~ percent(DataValue/lag(DataValue, 1) - 1)),
         change = case_when(date == max(date) ~ DataValue)) %>% 
  ggplot(aes(date, DataValue)) +
  geom_line(show.legend = F, colour = "#00BFC4",
            size = 1) +
  geom_line(aes(y = trend), linetype = "dashed", size = 1) +
  geom_point(aes(y = change), colour = "#00BFC4", size = 3) +
  # geom_segment(aes(x = low_gdp$date[2] + months(3), y = low_gdp$DataValue[1]/1000000,
  #                  xend = max(date) - months(1), yend = low_gdp$DataValue[1]/1000000),
  #              linetype = "dashed", size = 1) +
  # recession_shade("2005-01-01", latest = "2020-06-01") +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#00BFC4") 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Inflation-Adjusted Gross Domestic Product",
       subtitle = "In trillions of dollars, seasonally adjusted. Change is calculated as nonannualized change from prior quarter.",
       caption = "Source: Bureau of Economic Analysis") +
  ylim(c(15, 20)) +
  xlim(ymd("2015-01-01"), ymd("2023-09-31")) +
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
  "charts/COVID_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)





autos <- gdp_tables %>% 
  filter(TableName == "T10501",
         LineNumber == "5",
         date >= ymd("2010-01-01")) %>% 
  mutate(latest = ifelse(date == max(date), "latest", "other"),
         updown = case_when(DataValue >= 0 ~ "up",
                            TRUE ~ "down")) %>% 
  ggplot(aes(date, DataValue/100, fill = updown, alpha = latest)) + 
  geom_col(show.legend = F) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1))

autos <- autos + 
  labs(x = NULL, y = NULL,
       title = "Quarterly change in real consumer spending on automobiles",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source:") +
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
  ) +
  geom_hline(yintercept = 0, colour = "black") 

ggsave("~/FRED/charts/COVID charts/autos.png", autos, device = "png", width = 7.1, height = 4)



pce_product <- bea_all("T20305")

pce_product %>% 
  filter(LineNumber %in% c("2", "4")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_auto = `2` - `4`) %>% 
  select(date, total = `2`, ex_auto, auto = `4`) %>% 
  gather(series, value, -date) %>% 
  group_by(series) %>% 
  mutate(change = value/lag(value, 1,na.pad = T) - 1) %>% 
  arrange(desc(date))
  
real_pce_product <- bea_all("T20306")

real_pce_product %>% 
  filter(LineNumber %in% c("2", "4")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_auto = `2` - `4`) %>% 
  select(date, total = `2`, ex_auto, auto = `4`) %>% 
  gather(series, value, -date) %>% 
  group_by(series) %>% 
  mutate(change = value/lag(value, 1,na.pad = T) - 1) %>% 
  arrange(desc(date))


real_pce_product %>% 
  filter(LineNumber %in% c("3", "4")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_auto = `3` - `4`) %>% 
  select(date, total = `3`, ex_auto, auto = `4`) %>% 
  gather(series, value, -date) %>% 
  group_by(series) %>% 
  mutate(change = value/lag(value, 1,na.pad = T) - 1) %>% 
  arrange(desc(date))


real_pce_product %>% 
  filter(LineNumber %in% c("2", "4")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_auto = `2` - `4`) %>% 
  select(date, total = `2`, ex_auto, auto = `4`) %>% 
  gather(series, value, -date) %>% 
  group_by(series) %>% 
  mutate(change = value - lag(value, 1,na.pad = T)) %>% 
  arrange(desc(date))



real_pce_product %>% 
  filter(LineNumber %in% c("19")) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         change = change * 100) %>% 
  arrange(desc(date))



p <- real_pce_product %>% 
  filter(LineNumber %in% c("3", "4")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_auto = `3` - `4`) %>% 
  select(date, total = `3`, `Other durable goods` = ex_auto, `Autos` = `4`) %>% 
  gather(series, value, -date, -total) %>% 
  group_by(series) %>% 
  mutate(change = value - lag(value, 1,na.pad = T),
         contrib = change/lag(total, 1, na.pad = T),
         total_change = total/lag(total, 1, na.pad = T) - 1) %>% 
  filter(year(date) >= 2016) %>% 
  ggplot(aes(date, contrib, fill = series)) +
  geom_col() +
  geom_line(aes(y = total_change), size = 1)

p <- p +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Motor vehicles drove the decline in durables spending",
       subtitle = "Contributions to quarterly change in real durable goods consumption. Black line is total change.",
       caption = "Note: Seasonally adjusted. | Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID_ex_autos.png", p, device = "png",  
       width = 14.2,
       height = 8)

p <- real_pce_product %>% 
  filter(LineNumber %in% c("13", "19")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_covid = `13` - `19`) %>% 
  select(date, total = `13`, `Other services` = ex_covid, `Food & accom.` = `19`) %>% 
  gather(series, value, -date, -total) %>% 
  mutate(series = factor(series, levels = c("Other services", "Food & accom."))) %>% 
  group_by(series) %>% 
  mutate(change = value - lag(value, 1,na.pad = T) - 1,
         contrib = change/lag(total, 1, na.pad = T),
         total_change = total/lag(total, 1, na.pad = T) - 1) %>% 
  filter(year(date) >= 2018) %>% 
  ggplot(aes(date, contrib, fill = series)) +
  geom_col() +
  scale_fill_manual(values = c(`Food & accom.` = "#00BFC4", `Other services` = "grey75")) +
  geom_line(aes(y = total_change), size = 1)

p <- p +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Restaurants and hotel spending slowed during Delta",
       subtitle = "Contributions to quarterly change in real services consumption. Black line is total change.",
       caption = "Note: Seasonally adjusted. | Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID_delta_svces.png", p, device = "png",  
       width = 14.2,
       height = 8)







p <- gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10106"),
         LineNumber == "1",
         date >= ymd("2015-01-01")) %>% 
  bind_rows(tibble(date = ymd("2022-10-01"), TableName = c("T10105", "T10106"),
                   DataValue = c(23828807, 19751598))) %>% 
  group_by(TableName) %>% 
  mutate(change = DataValue/first(DataValue) - 1,
         trend = case_when(TableName == "T10105" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           TableName == "T10106" ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x)))
         )
  ) %>% 
  ungroup() %>% 
  mutate(TableName = factor(TableName, levels = c("T10105", "T10106"),
                            labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  ggplot(aes(date, change, colour = TableName)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = TableName), linetype = "dashed", size = 1)

p <- p +
  geom_dl(aes(label = TableName),
          method = list(dl.trans(x = x + .1),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2015-01-01"), ymd("2022-09-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal GDP vs trend",
       subtitle = "Change since Q1 2015, seasonally adjusted. Q4 2021 is projected.",
       caption = "Source: Bureau of Economic Analysis") +
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


# Inventories:
inventories <- map_dfr(c("T50805B", "T50806B"), ~bea_all(.x)) %>% 
  mutate(real = case_when(TableName == "T50805B" ~ "Nominal",
                          TRUE ~ "Real"))

p <- inventories %>% 
  filter(real == "Real",
         LineNumber %in% c("18", "19"),
         year(date) >= 2005
         ) %>% 
  mutate(LineNumber = factor(LineNumber, levels = c("18", "19"),
                             labels = c("Durable", "Non-durable"))) %>% 
  ggplot(aes(date, DataValue/1000000, fill = LineNumber)) +
  geom_col()

p <- p +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4")) +
  labs(x = NULL, y = NULL, 
       title = "Real Private Inventories",
       subtitle = "Trillions of 2012 dollars, seasonally adjusted annual rates",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/inventories.png", p, device = "png",  
       width = 14.2,
       height = 8)


inventory_detail <- bea_all(table = 'U001B', dataset = 'NIUnderlyingDetail')

p <- inventory_detail %>% 
  filter(LineNumber %in% c("12", "30", "52")) %>% 
  group_by(date) %>% 
  summarize(DataValue = sum(DataValue)) %>% 
  mutate(series = "Autos") %>% 
  bind_rows(inventories %>% 
              filter(real == "Real", LineNumber == "1") %>% 
              select(date, DataValue) %>% 
              mutate(series = "Total")) %>% 
  spread(series, DataValue) %>% 
  mutate(`Everything else` = Total - Autos,
         total_change = Total- lag(Total, 1, na.pad = T)) %>% 
  select(-Total) %>% 
  gather(series, value, -date, -total_change) %>% 
  mutate(series = factor(series, levels = c("Everything else", "Autos"))) %>% 
  group_by(series) %>% 
  mutate(change = value - lag(value, 1, na.pad = T)) %>% 
  filter(year(date) >= 2005) 

p <- p %>% 
  ggplot(aes(date, change/1000, fill = series)) +
  geom_col() +
  geom_line(aes(y = total_change/1000), size = 1) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(Autos = "#1f78b4", `Everything else` = "grey75")) +
  labs(x = NULL, y = NULL, 
       title = "Change in Real Private Inventories",
       subtitle = "Billions of 2012 dollars, seasonally adjusted annual rates. Black line is aggregate change.",
       caption = "Note: 'Autos' includes manufacturer, wholeesale and retail inventories of motor vehicles and parts.\nSource: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/inventories_autos.png", p, device = "png",  
       width = 14.2,
       height = 8)


# Full-year (by two measures)
p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(value = mean(DataValue)) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1,
         fill = case_when(change >= 0 ~ "high",
                          TRUE ~ "not")) %>% 
  filter(year >= 1960) %>%
  ggplot(aes(year, change, fill = fill)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(high = "#1f78b4", not = "#b2df8a")) +
  geom_hline(yintercept = 0)

p <- p +
  scale_y_continuous(labels = percent, breaks = c(-.025, 0, .025, .05, .075)) +
  labs(x = NULL, y = NULL, 
       title = "Change in Annual Inflation-Adjusted GDP",
       subtitle = "Annual GDP calculated as four-quarter calendar-year average",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/full_year.png", p, device = "png",  
       width = 14.2,
       height = 8)


p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         month(date) == 10) %>% 
  mutate(year = year(date)) %>% 
  rename(value = DataValue) %>% 
  # bind_rows(tibble(year = 2021, value = 19732119)) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1,
         fill = case_when(change >= 0 ~ "high",
                          TRUE ~ "not")) %>% 
  filter(year >= 1960) %>%
  ggplot(aes(year, change, fill = fill)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(high = "#1f78b4", not = "#b2df8a")) +
  geom_hline(yintercept = 0)

p <- p +
  scale_y_continuous(labels = percent, breaks = c(-.025, 0, .025, .05, .075, .1, .125)) +
  labs(x = NULL, y = NULL, 
       title = "Annual Change in Inflation-Adjusted GDP",
       subtitle = "Percent change, fourth quarter to fourth quarter",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/q4q4.png", p, device = "png",  
       width = 14.2,
       height = 8)



p <- gdp_tables %>% 
  filter(TableName == "T10105",
         LineNumber == "1",
         month(date) == 10) %>% 
  mutate(year = year(date)) %>% 
  rename(value = DataValue) %>% 
  # bind_rows(tibble(year = 2021, value = 23828807)) %>%
  mutate(change = value/lag(value, 1, na.pad = T) - 1,
         fill = case_when(change >= last(change) ~ "high",
                          TRUE ~ "not")) %>% 
  filter(year >= 1960) %>%
  ggplot(aes(year, change, fill = fill)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(high = "#1f78b4", not = "grey75")) +
  geom_hline(yintercept = 0)

p <- p +
  scale_y_continuous(labels = percent, breaks = c(-.025, 0, .025, .05, .075, .1, .125)) +
  labs(x = NULL, y = NULL, 
       title = "Annual Change in Nominal GDP",
       subtitle = "Percent change, fourth quarter to fourth quarter",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/q4q4_nominal.png", p, device = "png",  
       width = 14.2,
       height = 8)


# Final domestic demand vs trend
# Trend function

vs_trend <- function(.data, var, reg_end = "2020-01-01") {
  df <- .data
  
  df %>% 
    ggplot(aes(date, {{var}})) +
    geom_line(size = 1, colour = "#1f78b4") +
    stat_smooth(data = subset(df, date < ymd(reg_end)), 
                method = "lm", 
                fullrange = TRUE,
                linetype = "dashed",
                se = FALSE,
                colour = "black") 
}





p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber %in% c("3", "6"),
         date >= ymd("2017-01-01")) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/first(DataValue) - 1) 

p <- p %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend =  F) +
  geom_hline(yintercept = 0) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) 

p <- p +
  scale_color_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  scale_x_date(limits = c(ymd("2017-01-01"), ymd("2023-06-01")), 
               date_labels = as.yearqtr) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real consumer spending on goods and services vs prepandemic trends",
       subtitle = "Change since first quarter 2017, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = LineDescription),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
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
    legend.position = "none")

ggsave("charts/COVID charts/goods_v_svces_vs_trend.png", p, device = "png", width = 14.2, height = 8)




p <- gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "1",
         date >= ymd("2015-01-01")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/100) %>% 
  bind_rows(tibble(date = ymd("2022-01-01"),
                   change = -.0003)) %>% 
  mutate(updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         latest = case_when(date == max(date) ~ "latest",
                            TRUE ~ "other")) %>% 
  ggplot(aes(date, change, fill = updown, alpha = latest)) + 
  geom_col(show.legend = F) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Quarterly Change in Inflation-Adjusted G.D.P.",
       subtitle = "Seasonally adjusted annual rate. Final quarter is IHS forecast.",
       caption = "Sources: Bureau of Economic Analysis, IHS") +
  # recession_shade("1947-01-01") +
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
  "charts/COVID charts/quarterly_change_fcast.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)






p <- gdp_tables %>% 
  filter(TableName == "T10401",
         date >= ymd("2015-01-01"),
         LineNumber == 6) %>% 
  bind_rows(tibble(date = ymd("2022-01-01"),
                   DataValue = 3.1)) %>% 
  mutate(updown = case_when(DataValue < 0 ~ "down",
                            TRUE ~ "up"),
         latest = case_when(date == max(date) ~ "latest",
                            TRUE ~ "other"),
         DataValue = (1 + DataValue/100)^.25-1) %>% 
  ggplot(., aes(date, DataValue, fill = updown, alpha = latest)) + 
  geom_col()

p <- p + 
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1)) +
  recession_shade("2005-01-01") +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Final domestic demand (GDP without inventory/trade effects)",
       subtitle = "Seasonally adjusted change from prior quarter. Final quarter is IHS forecast..",
       caption = "Sources: Bureau of Economic Analysis, IHS") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black") +
  theme(
    plot.title = element_text(size = 25),
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

ggsave("~/FRED/charts/COVID charts/final_demand_fcast.png", p, device = "png", width = 14.2, height = 8)



p <- gdp_tables %>% 
  filter(TableName == "T10102",
         LineNumber %in% c("2", "8", "14", "15", "22")) %>% 
  mutate(grp = case_when(LineNumber %in% c("2", "8", "22") ~ "dom_demand",
                         LineNumber == "14" ~ "invent",
                         LineNumber == "15" ~ "trade")) %>% 
  group_by(date, grp) %>% 
  summarize(contrib = sum(DataValue)) %>% 
  bind_rows(tibble(date = ymd("2022-01-01"),
                   grp = c("invent", "trade", "dom_demand"),
                   contrib = c(-1.2, -2.1, 3.2))) %>% 
  ungroup() %>% 
  mutate(grp = factor(grp, levels = c("invent", "trade", "dom_demand"),
                      labels = c("Inventories", "Net exports", "Final domestic demand")),
         latest = case_when(date == max(date) ~ "latest", TRUE ~ "not")) %>% 
  group_by(date) %>% 
  mutate(total = sum(contrib)) %>% 
  filter(year(date) >= 2018) %>% 
  ggplot(aes(date, contrib, fill = grp)) +
  geom_col(alpha = .8, aes(color = latest)) +
  scale_color_manual(values = c(latest = "red", not = "grey75"), guide = "none") +
  geom_line(aes(y = total), size = 1, show.legend = F) +
  geom_point(aes(y = total), size = 3, show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(`Final domestic demand` = "#084594", `Net exports` = "#6baed6", `Inventories` = "#9ecae1")) 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Final domestic demand and other contributions to quarterly change in G.D.P.",
       subtitle = "Contributions to real annualized change. Black line is total change. Final quarter is IHS forecast.",
       caption = "Note: Seasonally adjusted. | Sources: Bureau of Economic Analysis, IHS") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/final_demand_fcast.png", p, device = "png",  
       width = 14.2,
       height = 8)



# Recreating Josh Bivens table
prices_costs <- bea_all("T11500")

prices_costs %>% 
  filter(date %in% c(ymd("1979-01-01"), ymd("2019-10-01"), ymd("2020-04-01"), ymd("2021-10-01")),
         LineNumber %in% c("2", "3", "7")) %>% 
  select(date, LineNumber, LineDescription, DataValue) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue - lag(DataValue, 1, na.pad = T)) %>% 
  group_by(date) %>% 
  filter(date != ymd("1979-01-01"),
         date != ymd("2020-04-01")) %>% 
  mutate(share = change/sum(change))


p <- prices_costs %>% 
  filter(LineNumber %in% c("2", "3", "7")) %>% 
  mutate(year = year(date)) %>% 
  group_by(year, LineNumber, LineDescription) %>% 
  summarize(value = mean(DataValue)) %>% 
  filter(year %in% 1979:2019) %>% 
  group_by(year) %>% 
  mutate(share = value/sum(value)) %>% 
  group_by(LineNumber, LineDescription) %>% 
  summarize(share = mean(share, na.rm = T)) %>% 
  mutate(period = "1979-2019")

p <- prices_costs %>% 
  filter(date %in% c(ymd("2020-04-01"), ymd("2022-01-01")),
         LineNumber %in% c("2", "3", "7")) %>% 
  select(date, LineNumber, LineDescription, DataValue) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue - lag(DataValue, 1, na.pad = T)) %>% 
  group_by(date) %>% 
  filter(date != ymd("2020-04-01")) %>% 
  mutate(share = change/sum(change)) %>% 
  ungroup() %>% 
  select(LineNumber, LineDescription, share) %>% 
  mutate(period = "Q2 2020 - Q1 2022") %>% 
  bind_rows(p)

p <- prices_costs %>% 
  filter(date %in% c(ymd("2021-01-01"), ymd("2022-01-01")),
         LineNumber %in% c("2", "3", "7")) %>% 
  select(date, LineNumber, LineDescription, DataValue) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue - lag(DataValue, 1, na.pad = T)) %>% 
  group_by(date) %>% 
  filter(date != ymd("2020-04-01")) %>% 
  mutate(share = change/sum(change)) %>% 
  ungroup() %>% 
  select(LineNumber, LineDescription, share) %>% 
  mutate(period = "Q1 2021 - Q1 2022") %>% 
  bind_rows(p)

p <- prices_costs %>% 
  filter(date %in% c(ymd("2019-10-01"), ymd("2022-01-01")),
         LineNumber %in% c("2", "3", "7")) %>% 
  select(date, LineNumber, LineDescription, DataValue) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue - lag(DataValue, 1, na.pad = T)) %>% 
  group_by(date) %>% 
  filter(date != ymd("2019-10-01")) %>% 
  mutate(share = change/sum(change)) %>% 
  ungroup() %>% 
  select(LineNumber, LineDescription, share) %>% 
  mutate(period = "Q4 2019 - Q1 2022") %>% 
  bind_rows(p)


p <- p %>% 
  mutate(period = factor(period, levels = c("Q1 2021 - Q1 2022", "Q4 2019 - Q1 2022", "1979-2019", "Q2 2020 - Q1 2022")),
         LineNumber = factor(LineNumber, levels = c("2", "3", "7"),
                             labels = c("Unit labor costs", "Nonlabor costs", "Corp. profits"))) %>% 
  ggplot(aes(LineNumber, share, fill = period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("#b2df8a", "#33a02c", "#a6cee3", "#1f78b4")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Contributions to growth in unit prices",
       subtitle = "Nonfinancial corporate sector",
       caption = "Note: Blue columns replicate Bivens (2022). | Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/bivens_rep.png", p, device = "png",  
       width = 14.2,
       height = 8)


gdi <- bea_all("T11000")
gdi_vs_gdp <- bea_all("T11706")


gdi_vs_gdp %>% 
  arrange(desc(date))

gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2"),
         year(date) >= 2018) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI")) %>% 
  select(date, label, DataValue) %>% 
  spread(label, DataValue) %>% 
  mutate(dollar_gap = GDI - GDP,
         pct_gap = GDI/GDP - 1) %>% 
  arrange(desc(date))


p <- gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2"),
         year(date) >= 2018) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI")) %>% 
  ggplot(aes(date, DataValue/1000000, colour = label)) +
  geom_line(show.legend = F, size = 1) 

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic products vs gross domestic income",
       subtitle = "In trillions of chained 2012 dollars, annualized",
       caption = "Source: Bureau of Economic Analysis") +
  scale_color_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.qp", cex = 1.5)) +
  xlim(ymd("2018-01-01"), ymd("2023-06-01")) +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/gdp_vs_gdi.png", p, device = "png",  
       width = 14.2,
       height = 8)


gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2"),
         year(date) >= 2012) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI"),
         DataValue = DataValue/1000000) %>% 
  select(date, label, DataValue) %>% 
  spread(label, DataValue) %>% 
  write_csv("gdp_vs_gdi.csv")






p <- gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2")) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI")) %>% 
  group_by(label) %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         # date = date + months(2)
         ) %>% 
  filter(year(date) >= 2020) %>% 
  ggplot(aes(date, chg, fill = label)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c"))

p <- p +
  labs(x = NULL, y = NULL, 
       title = "Quarterly Change in Real GDP and GDI",
       subtitle = "Change from prior quarter, not annualized. Seasonally adjusted.",
       caption = "Source: Bureau of Economic Analysis") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/gdp_vs_gdi_quarterly.png", p, device = "png",  
       width = 14.2,
       height = 8)



p <- gdi_vs_gdp %>% 
  filter(LineNumber == "3",
         year(date) >= 2019) %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         updown = case_when(chg < 0 ~ "down",
                            TRUE ~ "up")) %>% 
  ggplot(aes(date, chg, fill = updown)) +
  geom_col(show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c(up = "#1f78b4", down = "#fb9a99"))

p <- p +
  labs(x = NULL, y = NULL, 
       title = "Quarterly Change in Average of GDP and GDI",
       subtitle = "Change from prior quarter, not annualized. Seasonally adjusted.",
       caption = "Source: Bureau of Economic Analysis") +
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
    strip.text = element_text(size = 16))

ggsave("~/FRED/charts/COVID charts/gdo.png", p, device = "png",  
       width = 14.2,
       height = 8)


gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2"),
         year(date) >= 2018) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI")) %>% 
  group_by(label) %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         chg = chg * 100,
         ann_chg = (DataValue/lag(DataValue, 1, na.pad = T))^4 - 1,
         ann_chg = 100 * ann_chg) %>% 
  arrange(desc(date)) %>% 
  select(date, LineDescription, label, chg, ann_chg)


# Q3 2022 revisions
gdi_old <- gdi
gdi_vs_gdp_old <- gdi_vs_gdp





gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         ann_change = (DataValue/lag(DataValue, 1, na.pad = T)) ^ 4 - 1) %>% 
  filter(date >= ymd("2005-01-01")) %>% 
  select(date, TableName, SeriesCode, LineDescription, level = DataValue, change, ann_change) %>% 
  write_csv("gdp_data_for_ashwin.csv")

gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "2") %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         ann_change = (DataValue/lag(DataValue, 1, na.pad = T)) ^ 4 - 1,
         date = date + months(2)) %>% 
  filter(date >= ymd("2005-01-01")) %>% 
  select(date, TableName, SeriesCode, LineDescription, level = DataValue, change, ann_change) %>% 
  write_csv("spending_data_for_ashwin.csv")



p <- gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "2") %>% 
  arrange(date) %>% 
  mutate(change = DataValue/100,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         latest = case_when(date == max(date) ~ "latest",
                            TRUE ~ "other")) %>% 
  filter(date >= ymd("20-01-01")) %>% 
  ggplot(aes(date, change, fill = updown, alpha = latest)) + 
  geom_col(show.legend = F) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  scale_alpha_manual(values = c(latest = .5, other = 1))

p <- p +
  # recession_shade("2008-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly Change in Inflation-Adjusted Consumer Spending",
       subtitle = "Seasonally adjusted annual rate. Latest data is preliminary",
       caption = "Source: Bureau of Economic Analysis") +
  # recession_shade("1947-01-01") +
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
  "charts/COVID charts/quarterly_change_pce.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)



p <- gdp_tables %>% 
  filter(TableName == "T10102",
         LineNumber %in% c("1", "14", "15")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(trade_and_inventories = `14` + `15`,
         final_demand = `1` - trade_and_inventories) %>% 
  select(date, trade_and_inventories, final_demand, total = `1`) %>% 
  gather(series, value, -date, -total) %>% 
  mutate(series = factor(series, levels = c("trade_and_inventories", "final_demand"),
                         labels = c("Trade and inventories", "Domestic demand"))) %>% 
  filter(year(date) >= 2021) %>% 
  ggplot(aes(date, value/100, fill = series)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(`Trade and inventories` = "#a6cee3", `Domestic demand` = "#1f78b4")) +
  geom_point(aes(y = total/100), colour = "black", size = 3, show.legend = F)

p <- p +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Volatile components obscure steadily slowing demand",
       subtitle = "Contributions to quarterly change in real Gross Domestic Product. Seasonally adjusted annual rate.\nDots indicate total change.",
       caption = "Source: Bureau of Economic Analysis") +
  # recession_shade("1947-01-01") +
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16)
  )

ggsave(
  "charts/COVID charts/underlying_change.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "13",
         date >= ymd("2021-10-01")) %>% 
  mutate(change = DataValue/first(DataValue) - 1)


oecd <- read_csv("QNA_26012023154321744.csv")

oecd %>% 
  mutate(date = ymd(paste(substr(TIME, 1, 4), as.numeric(substr(TIME, 7, 7)) * 3 - 2, 1, sep = "-"))) %>% 
  filter(MEASURE == "LNBQRSA",
         SUBJECT == "B1_GE"
         ) %>% 
  group_by(Country) %>% 
  mutate(change = Value/first(Value) - 1) %>% 
  ggplot(aes(date, change, colour = Country)) +
  geom_line() +
  geom_dl(aes(label = Country),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5)) +
  geom_hline(yintercept = 0) +
  xlim(ymd("2019-01-01"), ymd("2023-12-01"))
  
  
  select(LOCATION, Country, SUBJECT, Subject, date, Value)

oecd %>% filter(LOCATION == "EA19", SUBJECT == "B1_GE") %>% 
  count(MEASURE, Measure)


gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "13",
         year(date) >= 2000) %>% 
  arrange(DataValue)


gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "13",
         month(date) == 10) %>% 
  mutate(change = DataValue/lag(DataValue, 1) - 1) %>% 
  arrange(change)





p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber %in% c("3", "6"),
         date >= ymd("2017-01-01")) 

r1 <- p %>% 
  filter(date < ymd("2020-01-01"),
         LineDescription == "Goods") %>% 
  lm(DataValue ~ date, data = .)

r2 <- p %>% 
  filter(date < ymd("2020-01-01"),
         LineDescription == "Services") %>% 
  lm(DataValue ~ date, data = .)

p <- p %>% 
  mutate(trend = case_when(LineDescription == "Goods" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           LineDescription == "Services" ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x))))) %>%
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/first(DataValue) - 1,
         trend = trend/first(trend) - 1) 


p %>%  
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = LineDescription), linetype = "dashed", size = 1) +
  scale_color_manual(values = c(Services = "#33a02c", Goods = "#1f78b4"))

p %>% 
  select(date, series = LineDescription, change, trend) %>% 
  pivot_wider(names_from = c("series"),
              values_from = c("change", "trend")) %>% 
  write_csv("goods_vs_svces_for_karl.csv")


investments <- bea_all("T50302")

p <- investments %>% 
  filter(LineNumber == "5") %>% 
  select(date, mfg_struct = DataValue)

p <- gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "1") %>% 
  select(date, total = DataValue) %>% 
  left_join(p, by = "date") %>% 
  filter(year(date) >= 2000)

p %>% 
  mutate(rest = total - mfg_struct) %>% 
  select(-total) %>% 
  gather(series, value, -date) %>% 
  ggplot(aes(date, value, fill = series)) +
  geom_col()

p <- investments %>% 
  filter(LineNumber %in% c("1", "5")) %>%
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(share = `5`/`1`)

p <- gdp_tables %>% 
  filter(TableName == "T10102",
         LineNumber == "8") %>% 
  select(date, fixed = DataValue) %>% 
  left_join(p, by = "date")

p <- p %>% 
  filter(year(date) >= 1958) %>% 
  mutate(contrib = fixed * share,
         updown = case_when(contrib >= 0 ~ "up",
                            TRUE ~ "down")) %>% 
  ggplot(aes(date, contrib, fill = updown)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(down = "#b2df8a", up = "#1f78b4")) +
  recession_shade("1958-01-01") +
  labs(x = NULL, y = NULL,
       title = "Real investment in manufacturing structures",
       subtitle = "Quarterly contribution to annualized real G.D.P. growth, percentage points",
       caption = "Note: Seasonally adjusted. Shaded areas denote recessions.\nSource: Bureau of Economic Analysis") +
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
  "charts/COVID charts/mfg_structures.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)
