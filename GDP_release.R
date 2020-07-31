# GDP day code
# bea.R documentation: https://github.com/us-bea/bea.R
# API documentation: https://www.bea.gov/API/bea_web_service_api_user_guide.htm

library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
source("BEAR.R")

# Get data
table_list <- c("T10101", "T10102", "T10105", "T10104", "T10106", "T10401", "T10111", "T10501", "T10103", "T20100", "T10506", "T20301", "T20304", "T50302")
gdp_tables <- map_dfr(table_list, ~bea_all(.x))

# Tables:
# GDP SAAR: T10101
# Contributions to growth: T10102
# Quantity indexes: T10103
# Nominal GDP: T10105
# Real GDP: T10106
# Detailed breakdown: T10401
# Price indexes: T10111
# Products: T10506


# Check latest date
max(gdp_tables$date)

# Key numbers:
key_numbers()
last_good(1, "lo") # Enter line number
final_demand()
inflation()

# charts
gdp_charts()

# GDP per quarter
gdp_charts <- function() {

  watermark <- paste0("Chart date: ", 
                      today(),
                      " | Data through ", max(gdp_tables$date),
                      " | Source: Bureau of Economic Analysis")
  
  p1 <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == "1", date >= ymd("2005-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  p1 <- p1 + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in inflation-adjusted U.S. gross domestic product",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 20),
          plot.caption = element_text(colour = "grey50", size = 14),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50", size = 16),
          axis.ticks = element_line(colour = "grey", size = 0.15)) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"), breaks = c(-7.5, -5, -2.5, 0, 2.5, 5))
  
  # Save in 16x9 format
  ggsave("~/FRED/charts/gdp.png", p1, device = "png", width = 14.2, height = 8)
  
  # Consumption
  pce <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == "2", date >= ymd("2005-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  pce <- pce + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in inflation-adjusted consumer spending",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"), breaks = c(-7.5, -5, -2.5, 0, 2.5, 5))
  
  # Save in 16x9 format
  ggsave("~/FRED/charts/consumption.png", pce, device = "png", width = 7.1, height = 4)
  
  # Business investment
  biz <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == "9", date >= ymd("2005-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  biz <- biz + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in inflation-adjusted business investment",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  # Save in 16x9 format
  ggsave("~/FRED/charts/biz_invest.png", biz, device = "png", width = 7.1, height = 4)
  
  # components chart
  # gdp_saar %>% 
  #   filter(LineNumber %in% c("2", "7", "16", "19", "22"),
  #          date >= ymd("2014-01-01")) %>% 
  #   mutate(series = factor(LineNumber, levels = c("2", "7", "16", "19", "22"), 
  #                          labels = c("Consumer spending", "Business investment", "Exports", "Imports", "Government spending"))) %>% 
  #   ggplot(., aes(date, DataValue, fill = series)) + geom_bar(position = "dodge", stat = "identity")
  
  
  # Final sales
  p2 <- gdp_tables %>% 
    filter(TableName == "T10401",
           date >= ymd("2005-01-01"),
           LineNumber == 7) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  p2 <- p2 + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in final sales (GDP without inventory effects)",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 22),
          plot.subtitle = element_text(size = 18),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50", size = 14),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50", size = 14)) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  ggsave("~/FRED/charts/final_sales.png", p2, device = "png", width = 14.2, height = 8)
  
  
  # Final domestic demand
  p3 <- gdp_tables %>% 
    filter(TableName == "T10401",
           date >= ymd("2005-01-01"),
           LineNumber == 6) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  p3 <- p3 + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Final domestic demand (GDP without inventory and trade effects)",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 22),
          plot.subtitle = element_text(size = 18),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50", size = 14),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50", size = 14)) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  ggsave("~/FRED/charts/final_demand.png", p3, device = "png", width = 14.2, height = 8)
  
  # Final domestic *private* demand
  priv_final <- gdp_tables %>% 
    filter(TableName == "T10401",
           date >= ymd("2005-01-01"),
           LineNumber == 8) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  priv_final <- priv_final + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Final domestic private demand (GDP without inventory/trade effects or government spending)",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 22),
          plot.subtitle = element_text(size = 18),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50", size = 14),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50", size = 14)) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"), breaks = c(-7.5, -5, -2.5, 0, 2.5, 5))
  
  ggsave("~/FRED/charts/final_pvt_demand.png", priv_final, device = "png", width = 14.2, height = 8)
  
  
  # Price indexes
  p4 <- gdp_tables %>% 
    filter(TableName == "T10111",
           date >= ymd("2005-01-01"),
           LineNumber %in% c("39", "40")) %>% 
    ggplot(., aes(date, DataValue, colour = LineNumber)) + geom_line()
  
  p4 <- p4 + recession_shade("2005-01-01") +
    scale_colour_manual(values = c(`39` = "grey70", `40` = "#F8766D")) +
    labs(x = NULL, y = NULL,
         title = "Annual change in consumer prices",
         subtitle = "Personal Consumption Expenditure Price Index, change from a year earlier",
         caption = watermark) +
    theme(legend.position = "none") +
    annotate("text", x = ymd("2012-05-01"), y = 2.85, label = "bold(Total)", parse = TRUE, colour = "grey70", size = 3) +
    annotate("text", x = ymd("2011-05-01"), y = .7, label = "bold(`Excluding food and energy`)", parse = TRUE, colour = "#F8766D", size = 3) +
    geom_line(size = 1.1) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_hline(yintercept = 2, linetype = "dashed") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  ggsave("~/FRED/charts/prices_annual.png", p4, device = "png", width = 7.1, height = 4)
  
  prices <- gdp_tables %>% 
    filter(TableName == "T20304", 
           date >= ymd("2005-01-01"),
           LineNumber %in% c("1", "25")) %>% 
    mutate(LineNumber = factor(LineNumber, levels = c("1", "25"), labels = c("headline", "core"))) %>% 
    group_by(LineNumber) %>% 
    mutate(change = (DataValue/lag(DataValue, 1))^4 -1) %>% 
    ggplot(., aes(date, change, colour = LineNumber)) + geom_line()
  
  prices <- prices + recession_shade("2005-01-01") +
    scale_colour_manual(values = c(headline = "grey70", core = "#F8766D")) +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in consumer prices",
         subtitle = "Personal Consumption Expenditure Price Index, seasonally adjusted at an annual rate",
         caption = watermark) +
    theme(legend.position = "none") +
    annotate("text", x = ymd("2009-10-01"), y = -.01, label = "bold(Total)", parse = TRUE, colour = "grey70", size = 3) +
    annotate("text", x = ymd("2014-01-01"), y = .028, label = "bold(`Excluding food and energy`)", parse = TRUE, colour = "#F8766D", size = 3) +
    geom_line(size = 1.1) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_hline(yintercept = .02, linetype = "dashed") +
    scale_y_continuous(label = percent)
  
  ggsave("~/FRED/charts/prices.png", prices, device = "png", width = 7.1, height = 4)
  
  # Car sales
  # autos <- gdp_tables %>% 
  #   filter(TableName == "T10501", LineNumber == "5",
  #          date >= ymd("2000-01-01")) %>% 
  #   mutate(color = ifelse(date == max(date), "new", "not")) %>% 
  #   ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  # 
  # autos <- autos + recession_shade("2000-01-01") +
  #   scale_colour_manual(values = c(not = "grey70", new = "#F8766D")) +
  #   labs(x = NULL, y = NULL,
  #        title = "Quarterly change in consumer spending on automobiles, adjusted for inflation",
  #        subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
  #        caption = watermark) +
  #   theme(legend.position = "none") +
  #   theme(plot.title = element_text(size = 16),
  #         plot.subtitle = element_text(size = 12),
  #         plot.background = element_rect(fill = "grey92"),
  #         panel.grid.major = element_line(colour = "grey", size = 0.15),
  #         panel.grid.minor = element_blank(),
  #         axis.text = element_text(colour = "grey50"),
  #         axis.ticks = element_line(colour = "grey", size = 0.15),
  #         plot.caption = element_text(colour = "grey50")) +
  #   geom_hline(yintercept = 0, colour = "black") +
  #   scale_y_continuous(label = function(x) paste0(x, "%"))
  
  autos <- gdp_tables %>% 
    filter(TableName == "T10501",
           LineNumber == "5",
           date >= ymd("2010-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new", "not")) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  autos <- autos + 
    scale_colour_manual(values = c(not = "grey70", new = "#F8766D")) +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in real consumer spending on automobiles",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  ggsave("~/FRED/charts/autos.png", autos, device = "png", width = 7.1, height = 4)
  
  # Goods vs services
  spend2 <- gdp_tables %>% 
    filter(TableName == "T10103",
           LineNumber %in% c("3", "6"),
           date >= ymd("2007-10-01")) %>% 
    group_by(LineDescription) %>% 
    mutate(change = DataValue/DataValue[1] -1) %>% 
    ggplot(., aes(date, change, color = LineDescription)) + geom_line()
  
  spend2 <- spend2 + 
    labs(x = NULL, y = NULL,
         title = "Goods spending is outpacing services since the recession",
         subtitle = "Change in consumer spending since Q4 2007, adjusted for inflation",
         caption = watermark) +
    theme(legend.position = "none") +
    annotate("text", x = ymd("2014-01-01"), y = 0.2, label = "bold(Goods)", parse = TRUE, colour = "#F8766D", size = 3) +
    annotate("text", x = ymd("2015-06-01"), y = .07, label = "bold(Services)", parse = TRUE, colour = "#00BFC4", size = 3) +
    geom_line(size = 1.1) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = percent)
  
  ggsave("~/FRED/charts/spend2.png", spend2, device = "png", width = 7.1, height = 4)
  
  # Biz investment
  biz <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber %in% c("10", "11", "12"),
           date >= ymd("2012-01-01")) %>% 
    mutate(LineDescription = factor(LineDescription, levels = c("Equipment", "Structures", "Intellectual property products"), labels = c("Equipment", "Structures", "Intellectual property"))) %>% 
    ggplot(., aes(date, DataValue, fill = LineDescription)) + geom_bar(stat = "identity", position = "dodge")
  
  biz <- biz + labs(x = NULL, y = NULL,
                    title = "Quarterly change in business investment",
                    subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
                    caption = watermark) +
    scale_fill_manual(values = c(Equipment = "#F8766D", Structures = "grey50", `Intellectual property` = "grey60")) +
    theme(legend.position = c(.2, .2),
          legend.title = element_blank(),
          legend.background = element_blank()) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  ggsave("~/FRED/charts/biz.png", biz, device = "png", width = 7.1, height = 4)
  
  housing <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == "13", date >= ymd("2005-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  housing <- housing + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in real residential investment",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  # Save in 16x9 format
  ggsave("~/FRED/charts/housing.png", housing, device = "png", width = 7.1, height = 4)
  
  govt <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == "22", date >= ymd("2005-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  govt <- govt + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in real government spending",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  # Save in 16x9 format
  ggsave("~/FRED/charts/govt.png", govt, device = "png", width = 7.1, height = 4)
  
  federal <- gdp_tables %>% 
    filter(TableName == "T10101",
           LineNumber == "23", date >= ymd("2005-01-01")) %>% 
    mutate(color = ifelse(date == max(date), "new",
                          ifelse(DataValue < 0, "grow", "shrink"))) %>% 
    ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
  
  federal <- federal + recession_shade("2005-01-01") +
    labs(x = NULL, y = NULL,
         title = "Quarterly change in real federal spending",
         subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
         caption = watermark) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50"),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50")) +
    geom_hline(yintercept = 0, colour = "black") +
    scale_y_continuous(label = function(x) paste0(x, "%"))
  
  # Save in 16x9 format
  ggsave("~/FRED/charts/federal.png", federal, device = "png", width = 7.1, height = 4)
  
  
  annual_change <- gdp_tables %>% 
    filter(TableName == "T10106",
           LineNumber == "1") %>% 
    group_by(year = year(date)) %>%
    summarize(annual = sum(DataValue))
  
  annual_change <- gdp_tables %>% 
    filter(TableName == "T10106",
           LineNumber == "1",
           month(date) == 10) %>% 
    mutate(year = year(date)) %>% 
    select(year, q4 = DataValue) %>% 
    left_join(annual_change, by = "year")
  
  change_plot <- annual_change %>% 
    mutate(q4 = q4/lag(q4, 1) - 1,
           annual = annual/lag(annual, 1) -1) %>% 
    gather(series, value, -year) %>% 
    filter(year >= 2000) %>% 
    mutate(series = factor(series, levels = c("annual", "q4"), labels = c("Full-year", "Q4/Q4")),
           projection = ifelse(year == 2018, "proj", "not")) %>% 
    ggplot(., aes(year, value, fill = series)) +
    geom_bar(stat = "identity", position = "dodge") 
  
  annual_change_plot <- change_plot +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = .03, linetype = "dashed") +
    scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(-.03, .04, by = .01)) +
    geom_rect(data = tibble(start = 2017.5, end = 2018.5), inherit.aes = F, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), alpha = 0.2) +
    labs(x = NULL, y = NULL,
         title = "3 Percent Growth? It Depends How You Measure.",
         subtitle = "Annual GDP growth, full-year vs fourth-quarter-over-fourth-quarter",
         caption = watermark) +
    theme(legend.title = element_blank(),
          legend.position = c(.8, .2),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey85"),
          plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 20),
          plot.caption = element_text(colour = "grey50", size = 14),
          axis.text = element_text(size = 16),
          legend.text = element_text(size = 16)) +
    annotate("text", x = 2018, y = .035, label = "bold('    2018\npreliminary')", parse = T, size = 5)
  
  
  ggsave("~/FRED/charts/annual_change.png", annual_change_plot, device = "png", width = 14.2, height = 8)
  
  
}


# Residual seasonality

# seas <- gdp_tables %>% 
#   filter(TableName =="T10101",
#          LineNumber == "1", date >= ymd("2005-01-01")) %>% 
#   mutate(color = ifelse(month(date) == 1, "Q1", "other")) %>% 
#   ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
# 
# seas <- seas + recession_shade("2005-01-01") +
#   labs(x = NULL, y = NULL,
#        title = "Quarterly change in inflation-adjusted U.S. gross domestic product",
#        subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
#        caption = watermark) +
#   theme(legend.position = "none") +
#   theme(plot.title = element_text(size = 16),
#         plot.subtitle = element_text(size = 12),
#         plot.background = element_rect(fill = "grey92"),
#         panel.grid.major = element_line(colour = "grey", size = 0.15),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(colour = "grey50"),
#         axis.ticks = element_line(colour = "grey", size = 0.15),
#         plot.caption = element_text(colour = "grey50")) +
#   geom_hline(yintercept = 0, colour = "black") +
#   scale_y_continuous(label = function(x) paste0(x, "%"), breaks = c(-7.5, -5, -2.5, 0, 2.5, 5))
# 
# ggsave("~/FRED/charts/seasonality.png", seas, device = "png", width = 7.1, height = 4)




# GDP by major product
cars <- gdp_tables %>% 
  filter(TableName == "T10506",
         LineNumber %in% c("2", "5"), date >= ymd("2012-01-01")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(without_cars = `2` -`5`) %>% 
  rename(with_cars = `2`) %>% 
  select(-`5`) %>% 
  mutate(without_cars = (without_cars/lag(without_cars, 1))^4-1,
         with_cars = (with_cars/lag(with_cars, 1))^4-1) %>% 
  gather(category, value, -date) %>% 
  filter(date >= ymd("2013-01-01")) %>% 
  group_by(category) %>% 
  mutate(avg = mean(value)) %>% 
  ungroup() %>% 
  mutate(category = factor(category, levels = c("with_cars", "without_cars"), labels = c("Total", "Without motor vehicles")),
         color = ifelse(date %in% c(ymd("2018-04-01", "2018-07-01")), "new", "old")) %>% 
  ggplot(., aes(date, value)) + geom_bar(stat = "identity", aes(fill = color)) + 
  geom_line(aes(y = avg), colour = "#F8766D", size = 1.1) +
  facet_wrap(~category) 

cars <- cars + scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#00BFC4", "grey70")) + 
  labs(x = NULL, y = NULL,
       title = "Auto Sales Drive Volatility in Consumer Spending",
       subtitle = "Quarterly change at seasonally adjusted annual rate",
       caption = watermark) +
  theme(legend.position = "none") +
  annotate("text", x = ymd("2013-03-01"), y = .029, label = "bold(Average)", parse = T, colour = "#F8766D", size = 5) +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 14))


ggsave("~/FRED/charts/cars.png", cars, device = "png", width = 14.2, height = 8)






save(gdp_saar, gdp_raw, gdp_quantity, gdp_levels, gdp_detail, gdp_contrib, gdp_breakdown, pce, prods, income, file = "pre_revision_data.RData")


contribs_chart <- function(line = "2") {
  colors <- c("2" = "grey70", "9" = "grey70", "13" = "grey70", "14" = "grey70", "15" = "grey70", "22" = "grey70")
  colors[line] <- "#F8766D"
  gdp_tables %>% 
    filter(TableName == "T10102",
           LineNumber %in% c("2", "9", "13", "14", "15", "22"),
           date >= ymd("2012-01-01")) %>% 
    mutate(LineNumber = factor(LineNumber, levels = c("2", "9", "13", "14", "15", "22"))) %>% 
    ggplot(., aes(date, DataValue, fill = LineNumber)) + geom_bar(stat = "identity", color = "grey90") +
    scale_fill_manual(values = colors) + theme(legend.position = "none") +
    scale_y_continuous(label = function(x) paste0(x, "pp"))
}

contrib_inventories <- contribs_chart("14") + 
  labs(x = NULL, y = NULL,
       title = "Inventories Have Been a Huge Factor in Recent GDP Swings",
       subtitle = "Contributions to quarterly change in real GDP",
       caption = watermark) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))

ggsave("~/FRED/charts/contrib_invent.png", contrib_inventories, device = "png", width = 7.1, height = 4)

contrib_pce <- contribs_chart("2") + 
  labs(x = NULL, y = NULL,
       title = "Consumers Are Driving the Recovery",
       subtitle = "Contributions to quarterly change in real GDP",
       caption = watermark) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))

ggsave("~/FRED/charts/contrib_pce.png", contrib_pce, device = "png", width = 7.1, height = 4)

contrib_trade <- contribs_chart("15") + 
  labs(x = NULL, y = NULL,
       title = "Trade Helped Boost Q4 Growth",
       subtitle = "Contributions to quarterly change in real GDP",
       caption = watermark) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10))

ggsave("~/FRED/charts/contrib_trade.png", contrib_trade, device = "png", width = 7.1, height = 4)


contrib_names <- c("Consumption", "Business investment", "Housing", "Inventories", "Trade", "Government spending")

library(magick)

# map(1:6, ~contribs_chart(.x) +
#        labs(x = NULL, y = NULL,
#                                      title = contrib_names[.x],
#                                      subtitle = "Contributions to quarterly change in real GDP",
#                                      caption = watermark) +
#        theme(plot.title = element_text(size = 16),
#              plot.subtitle = element_text(size = 12),
#              plot.caption = element_text(size = 10)) %>%
#       ggsave(paste0("~/FRED/charts/contrib", .x), ., device = "png", width = 7.1, height = 4))

# png(file = "contrib_%d.png", width = 200, height = 100)

plots <- tibble(grp = 1:6) %>% 
  mutate(plot = map(grp, ~contribs_chart(.x) +
                      labs(x = NULL, y = NULL,
                           title = contrib_names[.x],
                           subtitle = "Contributions to quarterly change in real GDP",
                           caption = watermark) +
                      theme(plot.title = element_text(size = 16),
                            plot.subtitle = element_text(size = 12),
                            plot.caption = element_text(size = 10))),
         filename = unlist(map(grp, ~paste0("file_", .x, ".png")))) %>% 
  select(plot, filename)

pwalk(plots, ggsave, path = "~/FRED/charts/", width = 7.1, height = 4)

# 
# 
# chart_table <- tibble(no = 1:6) %>% 
#   mutate(filename = paste0("~/FRED/charts/file_", no, ".png")) %>% 
#   mutate(chart = image_read(filename))
# 
# 
# map_chr(1:6, ~paste0("~/FRED/charts/file_", .x, ".png")) %>% 
  

gif_images <- map_chr(1:6, ~paste0("~/FRED/charts/file_", .x, ".png")) %>% 
  map(image_read)

image_animate(image_scale(image_join(gif_images), "1000x500"), fps = 1)
  

investments <- bea_all("T50302")

investments %>% 
  filter(LineNumber %in% c("2", "7")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(without_oil = `2` - `7`) %>% 
  rename(with_oil = `2`) %>% 
  filter(!is.na(without_oil)) %>% 
  select(-`7`) %>% 
  filter(date >= ymd("2016-01-01")) %>% 
  gather(category, value, -date) %>% 
  group_by(category) %>% 
  mutate(avg = mean(value)) %>% 
  ggplot(., aes(date, value)) + geom_bar(stat = "identity") + facet_wrap(~category) + 
  geom_line(aes(y = avg), colour = "#F8766D", size = .75) +
  ggtitle("Business investment with and without mining contribution")


biz_invest <- investments %>% 
  filter(LineNumber %in% c("2", "7")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(without_oil = `2` - `7`) %>% 
  rename(with_oil = `2`) %>% 
  filter(!is.na(without_oil)) %>% 
  select(-`7`) %>% 
  gather(category, value, -date) %>% 
  mutate(category = factor(category, levels = c("with_oil", "without_oil"), labels = c("Total", "Excluding mining"))) %>% 
  group_by(category) %>% 
  mutate(roll = rollmean(value, 4, align = "right", na.pad = TRUE)) %>% 
  filter(date >= ymd("2012-01-01")) %>% 
  ggplot(., aes(date, value)) + geom_bar(stat = "identity", fill = "grey70") + facet_wrap(~category) + 
  geom_line(aes(y = roll), colour = "#F8766D", size = 1) + 
  geom_vline(xintercept = ymd("2018-01-01"), linetype = 2) +
  theme(strip.text = element_text(size = 14),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 18),
        panel.grid.major = element_line(colour = "grey", size = 0.15),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50", size = 14),
        axis.ticks = element_line(colour = "grey", size = 0.15),
        plot.caption = element_text(colour = "grey50", size = 14)
        ) +
  labs(x = NULL, y = NULL,
       title = "Business investment with and without mining contribution",
       subtitle = "Quarterly contributions to annualized GDP growth, and 4-quarter trailing average",
       caption = watermark) +
  annotate("text", x = ymd("2013-09-01"), y = 9, label = "bold(`4-quarter average`)", parse = TRUE, colour = "#F8766D", size = 6) +
  annotate("text", x = ymd("2016-10-01"), y = -3, label = "`Tax cut takes effect`", parse = TRUE, colour = "grey10", size = 5.5)

ggsave("~/FRED/charts/biz-invest-detail.png", biz_invest, device = "png", width = 14.2, height = 8)


investments %>% 
  filter(LineNumber %in% c("2", "7")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(without_oil = `2` - `7`) %>% 
  rename(with_oil = `2`) %>% 
  filter(!is.na(without_oil)) %>% 
  select(-`7`) %>% 
  gather(category, value, -date) %>% 
  mutate(category = factor(category, levels = c("with_oil", "without_oil"), labels = c("Total", "Excluding mining"))) %>% 
  group_by(category) %>% 
  mutate(four_qtr_avg = rollmean(value, 4, align = "right", na.pad = TRUE)) %>% 
  filter(date >= ymd("2000-01-01")) %>% 
  write.csv("bis_invest_for_jim_and_matt.csv")



conor_chart <- gdp_tables %>% 
  filter(TableName == "T10102",
         LineNumber %in% c(1, 13)) %>% 
  select(date, LineDescription, DataValue) %>% 
  spread(LineDescription, DataValue) %>% 
  mutate(Other = case_when(Residential < 0 & `Gross domestic product` > 0 ~ `Gross domestic product`,
                           TRUE ~ `Gross domestic product` - Residential)) %>% 
  select(-`Gross domestic product`) %>%
  gather(series, value, -date) %>% 
  filter(date >= ymd("1980-01-01")) %>% 
  mutate(series = factor(series, levels = c("Other", "Residential"), labels = c("Total", "Housing"))) %>% 
  ggplot(., aes(date, value, fill = series)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = c(Housing = "#00BFC4", Total = "grey75")) 

conor_chart <- conor_chart +
  recession_shade("1980-01-01") +
  labs(x = NULL, y = NULL,
       title = "Housing Isn't Driving the Economy This Time Around",
       subtitle = "Housing ordinarily accounts for a big piece of the monthly swings in GDP. But lately it has played less of a role.",
       caption = watermark) +
  scale_y_continuous(breaks = seq(-8, 10, by = 2)) +
  theme(legend.position = c(.9, .15),
        legend.background = element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey85")) +
  annotate("text", 
           x = ymd("2011-01-01"), 
           y = 7.5, 
           label = "-- Recessions", colour = "grey40", size = 3)
  
ggsave("housing_contributions.svg", conor_chart, width = 16, height = 12, device = "svg")

gdp_tables %>% 
  filter(TableName == "T10102",
         LineNumber %in% c(1, 13)) %>% 
  select(date, LineDescription, DataValue) %>% 
  spread(LineDescription, DataValue) %>% 
  mutate(Total = case_when(Residential < 0 & `Gross domestic product` > 0 ~ `Gross domestic product`,
                           TRUE ~ `Gross domestic product` - Residential)) %>% 
  write.csv("housing_contributions.csv")


gdp_tables %>% 
  filter(TableName == "T10105",
         LineNumber %in% c(1, 13)) %>% 
  select(date, LineDescription, DataValue) %>% 
  spread(LineDescription, DataValue) %>% 
  
  
gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == 13,
         date >= ymd("1960-01-01")) %>% 
  mutate(falling = ifelse(DataValue < 0, "down", "up")) %>% 
  ggplot(., aes(date, DataValue, fill = falling)) + geom_bar(stat = "identity") + 
  recession_shade("1960-01-01") +
  scale_fill_manual(values = c(down = "#00BFC4", up = "grey70")) +
  theme(legend.position = "none")



  


p1 <- gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "1", date >= ymd("2005-01-01")) %>% 
  select(date, DataValue) %>% 
  bind_rows(tibble(date = ymd("2018-10-01"), DataValue = 2.2)) %>% 
  mutate(color = ifelse(date == max(date), "new",
                        ifelse(DataValue < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

p1 <- p1 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in inflation-adjusted U.S. gross domestic product",
       subtitle = "Seasonally adjusted at an annual rate. Data for fourth quarter 2018 is projected.",
       caption = watermark) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, colour = "black") +
  scale_y_continuous(breaks = seq(-8, 4, by = 2)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey85"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(colour = "grey50", size = 14),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))
  
# Save in 16x9 format
ggsave("~/FRED/charts/gdp_proj.png", p1, device = "png", width = 14.2, height = 8)


theme(plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.background = element_rect(fill = "grey92"),
      panel.grid.major = element_line(colour = "grey", size = 0.15),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = "grey50"),
      axis.ticks = element_line(colour = "grey", size = 0.15),
      plot.caption = element_text(colour = "grey50"))



library(tidyseasonal)
nsa <- bea_all("T80106")

nsa <- nsa %>% 
  filter(LineNumber == 1) 

nsa$my_adjust <- seas_adjust(nsa, date, DataValue)

nsa <- nsa %>% 
  mutate(change = ((my_adjust/lag(my_adjust, 1))^4 -1)*100) %>%
  filter(date >= ymd("2005-01-01"))

resid_seas <- gdp_tables %>% 
  filter(TableName == "T10101", LineNumber == "1", date >= ymd("2005-01-01")) %>% 
  left_join(nsa %>% select(date, change), by = "date")

# resid_seas$my_adjust <- seas_adjust(nominal, date, change)

resid_seas %>% 
  select(date, DataValue, change) %>% 
  gather(series, value, -date) %>% 
  mutate(series = factor(series, levels = c("change", "DataValue"), labels = c("Re-adjusted", "Official")),
         qtr = case_when(month(date) == 1 ~ "q1",
                        TRUE ~ "other")) %>% 
  ggplot(., aes(date, value, fill = series, alpha = qtr)) + geom_bar(stat = "identity", position = "dodge") +
  scale_alpha_manual(values = c(.4, 1))


nsa %>% 
  select(date, DataValue, my_adjust) %>% 
  gather(series, value, -date) %>% 
  ggplot(., aes(date, value, colour = series)) + geom_line()





# unrevised_data <- gdp_tables

revision_comp <- gdp_tables %>% 
  filter(TableName == "T10101",
         date >= ymd("2014-01-01")) %>% 
  mutate(revise = "revised") %>% 
  bind_rows(unrevised_data %>% 
              filter(TableName == "T10101",
                     date >= ymd("2014-01-01")) %>% 
              mutate(revise = "unrevised"))

revision_comp %>% 
  filter(LineNumber == "1") %>% 
  ggplot(., aes(date, DataValue, fill = revise)) + 
  geom_bar(stat = "identity", position  = "dodge") +
  labs(x = NULL, y = NULL,
       title = "How Revisions Affected GDP Growth",
       subtitle = "Quarterly GDP growth before and after 5-year revisions",
       caption = watermark) +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .2),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey85"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(colour = "grey50", size = 14),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  geom_hline(yintercept = 0)


revision_comp %>% 
  filter(LineNumber %in% c("2", "9", "13", "16", "19", "22")) %>% 
  ggplot(., aes(date, DataValue, fill = revise)) + 
  geom_bar(stat = "identity", position  = "dodge") +
  facet_wrap(~ LineDescription, scales = "free_y")


unrevised_data %>% 
  filter(TableName == "T10106",
         LineNumber == "1") %>% 
  mutate(change = DataValue/lag(DataValue, 4) - 1,
         revise = "Unrevised") %>% 
  filter(year(date) >= 2010) %>% 
  bind_rows(gdp_tables %>% 
              filter(TableName == "T10106",
                     LineNumber == "1") %>% 
              mutate(change = DataValue/lag(DataValue, 4) - 1,
                     revise = "Revised") %>% 
              filter(year(date) >= 2010)) %>% 
  ggplot(., aes(date, change, colour = revise)) + geom_line()


unrevised_data %>% 
  filter(TableName == "T10101",
         LineNumber == "1", date >= ymd("2005-01-01")) %>% 
  arrange(desc(date))


old_gdp <- map_dfr(c("2019-06-27", "2019-07-26"),
                   ~alfred_tidy("GDPC1", vintage_dates = .x))


revisions <- old_gdp %>% 
  group_by(vintage) %>% 
  mutate(change = value/lag(value, 4) - 1) %>% 
  filter(year(date) >= 2012) %>% 
  ungroup() %>% 
  mutate(vintage = factor(vintage, levels = c("2019-06-27", "2019-07-26"),
         labels = c("Unrevised", "Revised"))) %>% 
  ggplot(., aes(date, change, colour = vintage)) + geom_line() +
  scale_y_continuous(label = percent) +
  

ggsave("~/FRED/charts/revisions.png", revisions, device = "png", width = 14.2, height = 8)



tibble(a = 0:10) %>% 
  mutate(breaks = cut(a, breaks = c(0, 5, 8, Inf), include.lowest = T))

tibble(a = 0:10) %>% 
  mutate(breaks = cut(a, breaks = seq(-1, 100, by = 5), 
                      include.lowest = T, align.right = T))


df <- matrix(rexp(42, rate = .1), ncol = 7)

df %>% 
  as_tibble() %>% 
  mutate(calc = rowMeans(.[2:4]))
  group_by(row_number()) %>% 
  nest() %>% 
  mutate(calc = map_dbl(data[2:3], 
                        ~rowMeans(.x)))
  
  
  set.seed(1234)
  mat <- matrix(rnorm(6*7), ncol = 7)
  inx <- matrix(sample(7, 2*6, TRUE), ncol = 2)

  inxMeans <- function(X, I, na.rm = FALSE){
    inx1 <- cbind(seq_len(nrow(I)), I[, 1])
    inx2 <- cbind(seq_len(nrow(I)), I[, 2])
    rowMeans(cbind(X[inx1], X[inx2]), na.rm = na.rm)
  }
  
  inxMeans(mat, inx)
  
  
  rowMeans(mat[, 5])  
  inx %>% 
    as_tibble()
  
  map(seq_along(inx[,1]), ~mean(mat[.x, inx[.x,]])) %>% 
    unlist
  
  
investments_levels <- bea_all("T50306")
investments_levels %>% 
  filter(LineNumber %in% c("2", "7")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_oil = `2` - `7`,
         change = ex_oil/lag(ex_oil, 4) - 1) %>% 
  filter(year(date) >= 2010) %>% 
  ggplot(., aes(date, change)) + geom_line()




investments_levels %>% 
  filter(LineNumber %in% c("2", "7"),
         date >= ymd("2017-10-01")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(without_oil = `2` - `7`) %>% 
  rename(with_oil = `2`,
         oil = `7`) %>% 
  filter(!is.na(without_oil)) %>% 
  gather(category, value, -date) %>% 
  mutate(category = factor(category, levels = c("with_oil", "without_oil", "oil"), 
                           labels = c("Total", "Excluding mining", "Mining"))) %>% 
  group_by(category) %>% 
  arrange(date) %>% 
  mutate(cum = value/first(value) -1) %T>% View() %>% 
  ggplot(., aes(date, cum, colour = category)) + geom_line(size = 0.8) +
  ggtitle("Change in real quarterly investment spending since Q4 2014")



year_end_charts <- function() {

  annual_change <- gdp_tables %>%
    filter(TableName == "T10106",
           LineNumber == "1") %>%
    group_by(year = year(date)) %>%
    summarize(annual = sum(DataValue))
  
  annual_change <- gdp_tables %>%
    filter(TableName == "T10106",
           LineNumber == "1",
           month(date) == 10) %>%
    mutate(year = year(date)) %>%
    select(year, q4 = DataValue) %>%
    left_join(annual_change, by = "year")
  
  change_plot <- annual_change %>%
    arrange(year) %>% 
    mutate(q4 = q4 / lag(q4, 1) - 1,
           annual = annual / lag(annual, 1) - 1) %>%
    gather(series, value,-year) %>%
    filter(year >= 2000) %>%
    mutate(
      series = factor(
        series,
        levels = c("annual", "q4"),
        labels = c("Full-year", "Q4/Q4")
      )
    ) %>%
    ggplot(., aes(year, value, fill = series)) +
    geom_bar(stat = "identity", position = "dodge")
  
  annual_change_plot <- change_plot +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = .03, linetype = "dashed") +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       breaks = seq(-.03, .04, by = .01)) +
    geom_rect(
      data = tibble(start = 2018.5, end = 2019.5),
      inherit.aes = F,
      aes(
        xmin = start,
        xmax = end,
        ymin = -Inf,
        ymax = Inf
      ),
      alpha = 0.2
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Two Measures of Full-Year GDP Growth.",
      subtitle = "Annual GDP growth, full-year vs fourth-quarter-over-fourth-quarter",
      caption = watermark
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = c(.8, .2),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(colour = "grey85"),
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 20),
      plot.caption = element_text(colour = "grey50", size = 14),
      axis.text = element_text(size = 16),
      legend.text = element_text(size = 16)
    ) +
    annotate(
      "text",
      x = 2019,
      y = .035,
      label = "bold('    2019\npreliminary')",
      parse = T,
      size = 5
    )
  
  
  ggsave(
    "~/FRED/charts/annual_change.png",
    annual_change_plot,
    device = "png",
    width = 14.2,
    height = 8
  )
  
  y_o_y <- gdp_tables %>%
    filter(TableName == "T10106",
           LineNumber == "1") %>%
    arrange(date) %>% 
    mutate(change = DataValue/lag(DataValue, 4, na.pad = TRUE, align = "right") - 1) %>% 
    filter(year(date) >= 2004) %>% 
    ggplot(., aes(date, change)) + geom_line(colour = "#00BFC4", size = 1.1) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 16),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey85"),
          plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 20),
          plot.caption = element_text(colour = "grey50", size = 14),
          axis.text = element_text(size = 16)) +
    geom_hline(yintercept = 0, colour = "black") +
    labs(x = NULL, y = NULL,
         title = "Year-over-year Change in Real Gross Domestic Product",
         subtitle = "Percent change from year-ago quarter",
         caption = paste0(watermark, "Source: Bureau of Economic Analysis")) +
    recession_shade("2005-01-01")
  
  ggsave(
    "~/FRED/charts/y_o_y.png",
    y_o_y,
    device = "png",
    width = 14.2,
    height = 8
  )
  

}



food <- gdp_tables %>% 
  filter(TableName == "T20301",
         LineNumber %in% c("9", "19"),
         year(date) >= 2015) %>%
  mutate(title = factor(LineNumber, levels = c("9", "19"),
                        labels = c("Food at home", "Restaurants, bars and hotels")),
         DataValue = DataValue/100) %>% 
  ggplot(., aes(date, DataValue, fill = title)) +
  geom_col(position = "dodge")

food <- food +
  labs(x = NULL, y = NULL,
       title = "COVID-19 Shifted Spending Patterns",
       subtitle = "Quarterly change in consumer spending, seasonally adjusted annual rate") +
  scale_y_continuous(labels = percent) +
  # theme(legend.title = element_blank(),
  #       legend.position = c(.2, .2),
  #       legend.background = element_blank()) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey85"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(colour = "grey50", size = 14),
        axis.text = element_text(size = 16)) +
  geom_hline(yintercept = 0, colour = "black") 

ggsave(
  "~/FRED/charts/food.png",
  food,
  device = "png",
  width = 14.2,
  height = 8
)


svces <- gdp_tables %>% 
  filter(TableName == "T20301",
         LineNumber %in% c("3", "8", "13"),
         year(date) >= 1980) %>% 
  mutate(pce = factor(LineNumber, levels = c("3", "8", "13"),
                      labels = c("Durable goods", "Nondurable goods", "Services")),
         down = ifelse(DataValue < 0, "down", "up"),
         DataValue = DataValue/100) %>% 
  ggplot(., aes(date, DataValue, fill = down)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black") +
  recession_shade("1980-01-01") +
  facet_wrap(~pce) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey85"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(colour = "grey50", size = 14),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 14)) +
  labs(x = NULL, y = NULL,
       title = "Quarterly Change in Consumer Spending",
       subtitle = "Percent change from prior quarter at a seasonally adjusted annual rate",
       caption = "Source: Bureau of Economic Analysis")


ggsave(
  "~/FRED/charts/svces.png",
  svces,
  device = "png",
  width = 14.2,
  height = 8
)



gdp_tables %>% 
  filter(TableName == "T20301",
         LineNumber %in% c("9", "19", "3", "8", "13")) %>% 
  mutate(pce = factor(LineNumber, levels = c("9", "19", "3", "8", "13"),
                      labels = c("Food at home", "Restaurants, bars and hotels", "Durable goods", "Nondurable goods", "Services"))) %>% 
  select(date, pce, DataValue) %>% 
  spread(pce, DataValue) %>% 
  write_csv("pce_outputs.csv")
         
         

gdp_tables %>% 
  filter(TableName == "T10501",
         LineNumber == "5") %>% 
  arrange(date) %>% 
  select(date, autos = DataValue) %>% 
  write_csv("autos.csv")

gdp_tables %>% 
  filter(TableName == "T10101",
         LineNumber == "9", date >= ymd("2005-01-01")) %>% 
  arrange(date) %>% 
  select(date, biz_invest = DataValue) %>% 
  write_csv("biz_invest.csv")


monthly <- map_dfr(c("T20805", "T20801"), ~bea_all(.x, freq = "M"))

monthly %>% 
  filter(TableName == "T20805",
         LineNumber == "1") %>% 
  arrange(date) %>% View()
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(change)