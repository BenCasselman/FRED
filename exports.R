library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# Data: https://www.bea.gov/international/detailed_trade_data.htm

soy <- read_csv("soy_exports.csv")

soy %>% 
  mutate(date = mdy(date)) %>% 
  group_by(date, commodity) %>% 
  summarize(exports = sum(weekly_exports)) %>% 
  ggplot(., aes(date, exports, fill = commodity)) + geom_bar(stat = "identity", position = "stack")
  
exports <- read_xls(unzip("IDS0182.zip", unzip("IDS0182.zip", list = T)[1,1]), sheet = 4, skip = 1) # Want BOP basis

exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-"))) %>% 
  arrange(date) %>% 
  ggplot(., aes(date, exports)) + geom_line()

unzip("IDS0182_Hist_1999_2017.zip", list = T)

exports_hist <- read_xls(unzip("IDS0182_Hist_1999_2017.zip", unzip("IDS0182_Hist_1999_2017.zip", list = T)[1,1]), sheet = 4, skip = 1) # Want BOP basis

exports <- bind_rows(exports, exports_hist)

exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-"))) %>% 
  arrange(date) %>% 
  ggplot(., aes(date, exports)) + geom_line() +
  ggtitle("Soybean exports by month")


exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-"))) %>% 
  arrange(date) %>% filter(date >= "2015-01-1") %>% 
  ggplot(., aes(date, exports)) + geom_line() +
  ggtitle("Soybean exports by month")


p1 <- p1 + recession_shade("2005-01-01") +
  labs(x = NULL, y = NULL,
       title = "Quarterly change in inflation-adjusted U.S. gross domestic product",
       subtitle = "Seasonally adjusted at an annual rate. Data for most recent quarter is preliminary.",
       caption = "Source: Bureau of Economic Analysis") +

theme_ben <- function() {
  theme(legend.position = "none") +
    theme(plot.title = element_text(size = 22),
          plot.subtitle = element_text(size = 18),
          plot.background = element_rect(fill = "grey92"),
          panel.grid.major = element_line(colour = "grey", size = 0.15),
          panel.grid.minor = element_blank(),
          axis.text = element_text(colour = "grey50", size = 14),
          axis.ticks = element_line(colour = "grey", size = 0.15),
          plot.caption = element_text(colour = "grey50", size = 14))
}


soybeans <- exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-"))) %>% 
  arrange(date) %>% filter(date >= "2014-01-1") %>% 
  ggplot(., aes(date, exports/1000)) + geom_line(colour = "#00BFC4", size = .8) +
  labs(x = NULL, y = NULL,
       title = "Soybean Exports by Month",
       subtitle = "Seasonally adjusted. Billions of nominal dollars, not adjusted for inflation.",
       caption = "Source: Bureau of Economic Analysis") + 
  scale_y_continuous(labels = dollar) + scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  theme_ben()

ggsave("~/FRED/charts/soybeans.png", soybeans, device = "png", width = 14.2, height = 8)


# Q3 2016: exports up 6.4% annualized. Next Q down 3.8
# net exports contrib 0.36. exports (gross) 0.74

exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-"))) %>% 
  arrange(date) %>% 
  mutate(change = exports/lag(exports, 12) -1) %>% filter(date >= "2014-01-1") %>% 
  ggplot(., aes(date, change)) + geom_line() + scale_y_continuous(label = percent)

exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-"))) %>% 
  arrange(date) %>% 
  View("soybeans")



exports %>% 
  filter(`Enduse Code` == "X00100") %>% 
  select(-`EU Description`) %>% 
  gather(month, exports, -`Enduse Code`, -Year) %>% 
  mutate(date = mdy(paste(month, "01", Year, sep = "-")),
         month_cat = case_when(month == "Jun" ~ "June",
                               month == "Sep" ~ "Sep",
                               TRUE ~ "Other")) %>% 
  arrange(date) %>% 
  ggplot(., aes(date, exports, fill = month_cat)) + geom_bar(stat = "identity")
  
  filter(date >= "2015-01-1") %>% 
  ggplot(., aes(date, exports)) + geom_line() +
  ggtitle("Soybean exports by month")

