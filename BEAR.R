library(bea.R)
library(tidyverse)
library(lubridate)
library(scales)

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
       title = "Quarterly change in inflation-adjusted U.S. gross domestic product",
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
  scale_y_continuous(label = function(x) paste0(x, "%"), breaks = c(-7.5, -5, -2.5, 0, 2.5, 5))

p1 

# Save in 16x9 format
ggsave("~/FRED/charts/gdp.png", p1, device = "png", width = 7.1, height = 4)

# components chart
gdp_saar %>% 
  filter(LineNumber %in% c("2", "7", "16", "19", "22"),
         date >= ymd("2014-01-01")) %>% 
  mutate(series = factor(LineNumber, levels = c("2", "7", "16", "19", "22"), 
                         labels = c("Consumer spending", "Business investment", "Exports", "Imports", "Government spending"))) %>% 
  ggplot(., aes(date, DataValue, fill = series)) + geom_bar(position = "dodge", stat = "identity")
  


# Contributions to change:
# Percent change at SAAR: (table 1)

gdp_contrib <- bea_all("T10102", "Q")

# Final sales
gdp_breakdown <- bea_all("T10401", "Q")

p2 <- gdp_breakdown %>% 
  filter(date >= ymd("2005-01-01"),
         LineNumber == 7) %>% 
  mutate(color = ifelse(date == max(date), "new",
                        ifelse(DataValue < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

p2 <- p2 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in final sales (GDP without inventory effects)",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
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
p3 <- gdp_breakdown %>% 
  filter(date >= ymd("2005-01-01"),
         LineNumber == 6) %>% 
  mutate(color = ifelse(date == max(date), "new",
                        ifelse(DataValue < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

p3 <- p3 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Final domestic demand (GDP without inventory and trade effects)",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
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
priv_final <- gdp_breakdown %>% 
  filter(date >= ymd("2005-01-01"),
         LineNumber == 8) %>% 
  mutate(color = ifelse(date == max(date), "new",
                        ifelse(DataValue < 0, "grow", "shrink"))) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

priv_final <- priv_final + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Final domestic private demand (GDP without inventory/trade effects or government spending)",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +
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

ggsave("~/FRED/charts/final_pvt_demand.png", priv_final, device = "png", width = 14.2, height = 8)


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
  geom_hline(yintercept = 2, linetype = "dashed") +
  scale_y_continuous(label = function(x) paste0(x, "%"))

ggsave("~/FRED/charts/inflation.png", p4, device = "png", width = 7.1, height = 4)

# Car sales
gdp_detail <- bea_all("T10501", "Q")

# autos <- gdp_detail %>% 
#   filter(LineNumber == "5",
#          date >= ymd("2000-01-01")) %>% 
#   mutate(color = ifelse(date == max(date), "new", "not")) %>% 
#   ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")
# 
# autos <- autos + recession_shade("2000-01-01") +
#   scale_colour_manual(values = c(not = "grey70", new = "#F8766D")) +
#   labs(x = NULL, y = NULL,
#        title = "Quarterly change in consumer spending on automobiles, adjusted for inflation",
#        subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
#        caption = "Source: Bureau of Economic Analysis") +
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

autos <- gdp_detail %>% 
  filter(LineNumber == "5",
         date >= ymd("2010-01-01")) %>% 
  mutate(color = ifelse(date == max(date), "new", "not")) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

autos <- autos + 
  scale_colour_manual(values = c(not = "grey70", new = "#F8766D")) +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in consumer spending on automobiles, adjusted for inflation",
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


ggsave("~/FRED/charts/autos.png", autos, device = "png", width = 7.1, height = 4)

gdp_detail %>% 
  filter(LineNumber == "5",
         date >= ymd("2000-01-01")) %>% 
  mutate(color = ifelse(date == max(date), "new", "not")) %>% View()



# Goods vs services
gdp_quantity <- bea_all("T10103", "Q")

spend2 <- gdp_quantity %>% 
  filter(LineNumber %in% c("3", "6"),
         date >= ymd("2007-10-01")) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/DataValue[1] -1) %>% 
  ggplot(., aes(date, change, color = LineDescription)) + geom_line()

spend2 <- spend2 + 
  labs(x = NULL, y = NULL,
       title = "Goods spending is outpacing services since the recession",
       subtitle = "Change in consumer spending since Q4 2007, adjusted for inflation",
       caption = "Source: Bureau of Economic Analysis") +
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
biz <- gdp_saar %>% 
  filter(LineNumber %in% c("10", "11", "12"),
         date >= ymd("2012-01-01")) %>% 
  mutate(LineDescription = factor(LineDescription, levels = c("Equipment", "Structures", "Intellectual property products"), labels = c("Equipment", "Structures", "Intellectual property"))) %>% 
  ggplot(., aes(date, DataValue, fill = LineDescription)) + geom_bar(stat = "identity", position = "dodge")

biz <- biz + labs(x = NULL, y = NULL,
     title = "Quarterly change in business investment",
     subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
     caption = "Source: Bureau of Economic Analysis") +
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


# Personal income
income <- bea_all("T20100", "Q")


gdp_quantity %>% filter(LineNumber == "1") %>% write.csv(., file = "gdpqi.csv")




# Residual seasonality

seas <- gdp_saar %>% 
  filter(LineNumber == "1", date >= ymd("2005-01-01")) %>% 
  mutate(color = ifelse(month(date) == 1, "Q1", "other")) %>% 
  ggplot(., aes(date, DataValue, fill = color)) + geom_bar(stat = "identity")

seas <- seas + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in inflation-adjusted U.S. gross domestic product",
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
  scale_y_continuous(label = function(x) paste0(x, "%"), breaks = c(-7.5, -5, -2.5, 0, 2.5, 5))




# GDP by major product

prods <- bea_all("T10506", "Q")

prods %>% 
  filter(LineNumber %in% c("5", "6", "7", "8"), date >= ymd("2012-01-01")) %>% 
  ggplot(., aes(date, DataValue, fill = LineDescription)) + geom_bar(stat = "identity", position = "dodge")
  
  ggplot(., aes(date, DataValue)) + geom_bar(stat = "identity")
 

cars <- prods %>% 
  filter(LineNumber %in% c("2", "5"), date >= ymd("2012-01-01")) %>% 
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
         color = ifelse(date %in% c(ymd("2017-10-01", "2018-01-01")), "new", "old")) %>% 
  ggplot(., aes(date, value)) + geom_bar(stat = "identity", aes(fill = color)) + 
  geom_line(aes(y = avg), colour = "#F8766D", size = 1.1) +
  facet_wrap(~category) 

cars <- cars + scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#00BFC4", "grey70")) + 
  labs(x = NULL, y = NULL,
       title = "Auto Sales Drive Volatility in Consumer Spending",
       subtitle = "Quarterly change at seasonally adjusted annual rate",
       caption = "Source: Bureau of Economic Analysis") +
  theme(legend.position = "none") +
  annotate("text", x = ymd("2013-03-01"), y = .029, label = "bold(Average)", parse = T, colour = "#F8766D", size = 5) +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 14))


ggsave("~/FRED/charts/cars.png", cars, device = "png", width = 14.2, height = 8)






