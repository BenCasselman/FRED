# Distributional Financial Accounts
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)

unzip("dfa (2).zip", list = T)

dfa <- read_csv(unz("dfa (2).zip", "dfa-income-levels-detail.csv"))
dfa <- dfa %>% 
  mutate(Date = ymd(paste(substr(Date, 1, 4), as.numeric(substr(Date, 7, 7)) * 3 - 2, "01", sep = "-" )))

dfa %>% 
  select(Date, Category, Deposits) %>% 
  left_join(gdp_tables %>% 
              filter(TableName == "T10104", LineNumber == "2") %>% 
              select(date, pce = DataValue),
            by = c("Date" = "date")) %>% 
  mutate(real = 100*Deposits /pce) %>%
  filter(year(Date) >= 2000,
         Category != "pct60to80") %>% 
  ggplot(aes(Date, real/1000000, colour = Category)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Real deposits by income quantile",
       subtitle = "In trillions of 2012 dollars, deflated by PCE price index",
       caption = "Sources: Federal Reserve, Bureau of Economic Analysis") +
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
    legend.position = c(.15, .81),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 14)
  )

ggsave(
  "charts/monthly_jobs.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

  
gdp_tables %>% 
  filter(TableName == "T010104", 
         date >= ymd("2005-01-01"),
         LineNumber %in% c("2")) %>% 
  select(date, LineDescription, DataValue) %>% 
  arrange(desc(date))


cpi_q <- fred_tidy("CPIAUCSL", freq = "q")


dfa %>% 
  select(Date, Category, Deposits) %>% 
  left_join(cpi_q %>% 
              select(date, cpi = value),
            by = c("Date" = "date")) %>% 
  mutate(real = 100*Deposits /cpi) %>%
  filter(year(Date) >= 2000,
         Category != "pct60to80") %>% 
  ggplot(aes(Date, real/1000000, colour = Category)) +
  geom_line() +
  geom_hline(yintercept = 0)



dfa_old <- read_csv(unz("dfa.zip", "dfa-income-levels-detail.csv")) %>% 
  mutate(Date = ymd(paste(substr(Date, 1, 4), as.numeric(substr(Date, 7, 7)) * 3 - 2, "01", sep = "-" )))

dfa_old %>% 
  select(Date, Category, Deposits = `Checkable deposits and currency`) %>% 
  left_join(gdp_tables %>% 
              filter(TableName == "T10104", LineNumber == "2") %>% 
              select(date, pce = DataValue),
            by = c("Date" = "date")) %>% 
  mutate(real = Deposits  * pce/122) %>%
  filter(year(Date) >= 2000,
         Category != "pct60to80") %>% 
  ggplot(aes(Date, real/1000000, colour = Category)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1, 1.2, 1.4,1.6, 1.8, 2))


dfa_old %>% 
  select(Date, Category, Deposits = `Checkable deposits and currency`) %>% 
  left_join(cpi_q %>% 
              select(date, cpi = value),
            by = c("Date" = "date")) %>% 
  mutate(real = Deposits  * cpi/289) %>%
  filter(year(Date) >= 2000,
         Category != "pct60to80") %>% 
  ggplot(aes(Date, real/1000000, colour = Category)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1, 1.2, 1.4,1.6, 1.8, 2))




p <- dfa %>% 
  select(Date, Category, Deposits) %>% 
  filter(year(Date) >= 2000) %>% 
  mutate(Category = factor(Category,
                           levels = c("pct00to20", "pct20to40", "pct40to60", "pct60to80", "pct80to99", "pct99to100"),
                           labels = c("Bottom 20%", "20th-40th pctile", "Middle 20%", "60th-80th pctile", "80th-99th pctile", "Top 1%"))) %>% 
  ggplot(aes(Date, Deposits/1000000, colour = Category)) +
  geom_line(size = 1, show.legend = F) +
  geom_hline(yintercept = 0) +
  geom_dl(aes(label = Category),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
  labs(x = NULL, y = NULL,
       title = "Deposits by income quantile",
       subtitle = "Holdings of checkable deposits, savings accounts and other short-term investments, in trillions",
       caption = "Sources: Federal Reserve") +
  xlim(ymd("2000-01-01"), ymd("2025-01-01")) +
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
    legend.position = c(.15, .81),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 14)
  )

ggsave(
  "charts/COVID charts/nominal_dfa.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)



p <- dfa %>% 
  select(Date, Category, Deposits) %>% 
  filter(year(Date) >= 2000) %>% 
  left_join(gdp_tables %>% 
              filter(TableName == "T10104", LineNumber == "2") %>% 
              select(date, pce = DataValue),
            by = c("Date" = "date")) %>% 
  mutate(real = 100*Deposits /pce, 
         Category = factor(Category,
                           levels = c("pct00to20", "pct20to40", "pct40to60", "pct60to80", "pct80to99", "pct99to100"),
                           labels = c("Bottom 20%", "20th-40th pctile", "Middle 20%", "60th-80th pctile", "80th-99th pctile", "Top 1%"))) %>% 
  ggplot(aes(Date, real/1000000, colour = Category)) +
  geom_line(size = 1, show.legend = F) +
  geom_hline(yintercept = 0) +
  geom_dl(aes(label = Category),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
  labs(x = NULL, y = NULL,
       title = "Real deposits by income quantile",
       subtitle = "Holdings of checkable deposits, savings accounts and other short-term investments, in trillions of 2012 dollars",
       caption = "Sources: Federal Reserve, BEA") +
  xlim(ymd("2000-01-01"), ymd("2025-01-01")) +
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
    legend.position = c(.15, .81),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 14)
  )

ggsave(
  "charts/COVID charts/real_dfa.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)
