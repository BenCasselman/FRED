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

# GDP per quarter
p1 <- gdp_saar %>% 
  filter(LineNumber == "1", date >= ymd("2005-01-01")) %>% 
  mutate(color = ifelse(date == max(date), "new",
                        ifelse(DataValue < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

p1 <- p1 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in inflation-adjusted U.S.+0.21 gross domestic product",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
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

p1 

# Save in 16x9 format
ggsave("~/FRED/charts/gdp.png", p1, device = "png", width = 7.1, height = 4)

# components chart
gdp_saar %>% 
  filter(LineNumber %in% c("2", "7", "15", "22"))


# Contributions to change:
# Percent change at SAAR: (table 1)

gdp_contrib <- bea_all("T10102", "Q")

# Final sales
p2 <- gdp_contrib %>% 
  filter(date >= ymd("2005-01-01"),
         LineNumber %in% c("1", "14")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(final = `1` - `14`, 
         color = ifelse(date == max(date), "new",
                        ifelse(final < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, final, fill = color)) + geom_bar(stat = "identity")

p2 <- p2 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in final sales (GDP without inventory effects)",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
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

ggsave("~/FRED/charts/final_sales.png", p2, device = "png", width = 7.1, height = 4)


# Final domestic demand
p3 <- gdp_contrib %>% 
  filter(date >= ymd("2005-01-01"),
         LineNumber %in% c("1", "14", "15")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(final = `1` - `14` - `15`, 
         color = ifelse(date == max(date), "new",
                        ifelse(final < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, final, fill = color)) + geom_bar(stat = "identity")

p3 <- p3 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Final domestic demand (GDP without inventory and trade effects)",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
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

ggsave("~/FRED/charts/final_demand.png", p3, device = "png", width = 7.1, height = 4)

# Price indexes
pce <- bea_all("T10111", "Q")

p4 <- pce %>% 
  filter(date >= ymd("2005-01-01"),
         LineNumber %in% c("39", "40")) %>%
  ggplot(., aes(date, DataValue, colour = LineNumber)) + geom_line()
  
p4 <- p4 + recession_shade("2005-01-01") +
  scale_colour_manual(values = c(`39` = "grey70", `40` = "#F8766D")) +
  labs(x = NULL, y = NULL,
       title = "Annual change in consumer prices",
       subtitle = "Personal Consumption Expenditure Price Index, change from a year earlier",
       caption = "Source: Bureau of Economic Analysis") +
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
  scale_y_continuous(label = function(x) paste0(x, "%"))

ggsave("~/FRED/charts/inflation.png", p4, device = "png", width = 7.1, height = 4)
