# ECI
library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(directlabels)

eci_series <- read_tsv("https://download.bls.gov/pub/time.series/ci/ci.series")
eci_industries <- read_tsv("https://download.bls.gov/pub/time.series/ci/ci.industry")

temp <- tempfile()
download.file("https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData", temp)
eci_data <- read_tsv("https://download.bls.gov/pub/time.series/ci/ci.data.1.AllData")

eci_data <- read_tsv("ci.data.1.AllData")
eci_data <- eci_data %>% 
  mutate(value = as.numeric(value),
         date = as.numeric(substr(period, 2, 3)) * 3,
         date = ymd(paste(year, date, "01", sep = "-")))

eci_data <- left_join(eci_data, eci_series, by = "series_id")

# Private W&S excluding incentive pay:
# 12mo
eci_data %>% 
  filter(series_id == "CIU2020000000710A") %>% 
  arrange(desc(date))

# 3mo
eci_data %>% 
  filter(series_id == "CIU2020000000710Q") %>% 
  arrange(desc(date))

p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("01", "02"),
    owner_code == 1,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code == "000000",
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, estimate_code, value) %>% 
  mutate(estimate_code = factor(estimate_code, levels = c("01", "02"),
                                labels = c("Total compensation", "Wages & salaries"))) %>% 
  group_by(estimate_code) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1) %>%
  filter(year(date) >= 2010) %>% 
  ggplot(aes(date, change, colour = estimate_code)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(`Wages & salaries` = "#1f78b4", `Total compensation` = "#33a02c")) +
  xlim(ymd("2010-01-01"), ymd("2025-06-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in employee compensation",
       subtitle = "Change in Employment Cost Index, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = estimate_code),
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

ggsave("charts/COVID charts/eci_overall.png", p, device = "png", width = 14.2, height = 8)

p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, estimate_code, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  group_by(industry_code) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1) %>%
  filter(year(date) >= 2010) %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_color_manual(values = c(`Leisure & hosp.` = "#1f78b4", `Total private` = "#33a02c")) +
  geom_hline(yintercept = 0) +
  xlim(ymd("2010-01-01"), ymd("2025-06-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in private-sector wages and salaries",
       subtitle = "Change in Employment Cost Index, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
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

ggsave("charts/COVID charts/eci_leis.png", p, device = "png", width = 14.2, height = 8)


# Annual
p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("01", "02"),
    owner_code == 1,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code == "000000",
    seasonal == "U"
  ) %>%
  select(date, industry_code, seasonal, estimate_code, value) %>% 
  mutate(estimate_code = factor(estimate_code, levels = c("01", "02"),
                                labels = c("Total compensation", "Wages & salaries"))) %>% 
  group_by(estimate_code) %>% 
  mutate(change = value/lag(value, 4, na.pad = T) - 1) %>%
  filter(year(date) >= 2010) %>% 
  ggplot(aes(date, change, colour = estimate_code)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(`Wages & salaries` = "#1f78b4", `Total compensation` = "#33a02c")) +
  xlim(ymd("2010-01-01"), ymd("2025-06-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Twelve-month change in employee compensation",
       subtitle = "Change from a year earlier in Employment Cost Index, not seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = estimate_code),
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

ggsave("charts/COVID charts/eci_overall_yy.png", p, device = "png", width = 14.2, height = 8)


p <- eci_data %>%
  filter(series_id %in% c("CIU1010000000000T", "CIU1020000000000T")) %>% 
  mutate(series_id = factor(series_id, 
                            levels = c("CIU1010000000000T", "CIU1020000000000T"),
                            labels = c("Total compensation", "Wages & salaries"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 4, na.pad = T) - 1) %>% 
  filter(year(date) >= 2010) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(`Wages & salaries` = "#1f78b4", `Total compensation` = "#33a02c")) +
  xlim(ymd("2010-01-01"), ymd("2025-06-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Twelve-month change in employee compensation, adjusted for inflation",
       subtitle = "Change from a year earlier in constant dollar Employment Cost Index, not seasonally adjusted",       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = series_id),
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

ggsave("charts/COVID charts/eci_overall_real.png", p, device = "png", width = 14.2, height = 8)


p <- eci_data %>%
  filter(series_id %in% c("CIU2020000000710I", "CIU2020000000000I")) %>% 
  mutate(series_id = factor(series_id, 
                            levels = c("CIU2020000000710I", "CIU2020000000000I"),
                            labels = c("Excluding incentive-paid", "All occupations"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 4, na.pad = T) - 1) %>% 
  filter(year(date) >= 2010) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(`Excluding incentive-paid` = "#1f78b4", `All occupations` = "#a6cee3")) +
  xlim(ymd("2010-01-01"), ymd("2026-03-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Private wages and salaries, with and without incentive-paid occupations",
       subtitle = "Change from a year earlier in Employment Cost Index, not seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = series_id),
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

ggsave("charts/COVID charts/eci_incentive_yy.png", p, device = "png", width = 14.2, height = 8)





p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, estimate_code, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  group_by(industry_code) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1) %>%
  filter(year(date) >= 2015) %>% 
  ggplot(aes(date, value, colour = industry_code)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  xlim(ymd("2010-01-01"), ymd("2024-05-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Three-month change in private-sector wages and salaries",
       subtitle = "Change in Employment Cost Index, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
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

ggsave("charts/eci_leis.png", p, device = "png", width = 14.2, height = 8)




eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000", "412000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000", "412000"),
                                labels = c("Total private", "Leisure & hospitality", "Retail"))) %>%
  group_by(industry_code) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) - 1,
         change = 100*change) %>%
  filter(year(date) >= 2015) %>% 
  select(-value) %>% 
  spread(industry_code, change) %>% 
  write_csv("eci_wage_growth.csv")



# Real vs Nominal
cpi_data <- fred_tidy("CPIAUCSL")

p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(industry_code) %>% 
  mutate(change = value/first(value) - 1)

p <- p %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(`Leisure & hosp.` = "#1f78b4", `Total private` = "#33a02c")) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) +
  geom_hline(yintercept = 0) +
  # xlim(ymd("2016-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL,
       title = "Nominal wages and salaries vs prepandemic trend",
       subtitle = "Cumulative change since Q1 2016, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
          method = list(dl.trans(x = x - 5), "last.qp", cex = 1.5))  +
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

ggsave("charts/COVID charts/eci_nominal_trend.png", p, device = "png", width = 14.2, height = 8)


p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  left_join(cpi_data %>% select(date, cpi = value), by = "date") %>% 
  mutate(real = value/cpi) %>% 
  select(date, industry_code, value = real) %>% 
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(industry_code) %>% 
  mutate(change = value/first(value) - 1)

p <- p %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(`Leisure & hosp.` = "#1f78b4", `Total private` = "#33a02c")) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) +
  geom_hline(yintercept = 0) +
  # xlim(ymd("2016-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL,
       title = "Real wages and salaries vs prepandemic trend",
       subtitle = "Cumulative change since Q1 2016, seasonally adjusted",
       caption = "Note: Inflation based on Consumer Price Index | Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
          method = list(dl.trans(x = x - 5.5, y = y - .5), "last.qp", cex = 1.5))  +
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

ggsave("charts/COVID charts/eci_real_trend.png", p, device = "png", width = 14.2, height = 8)


# Same but for total comp.
p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("01"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(industry_code) %>% 
  mutate(change = value/first(value) - 1)

p <- p %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) +
  geom_hline(yintercept = 0) +
  # xlim(ymd("2016-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL,
       title = "Nominal total compensation vs prepandemic trend",
       subtitle = "Cumulative change since Q1 2016, seasonally adjusted",
       caption = "Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
          method = list(dl.trans(x = x - 5), "last.qp", cex = 1.5))  +
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

ggsave("charts/eci_nominal_comp_trend.png", p, device = "png", width = 14.2, height = 8)


p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("01"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  left_join(cpi_data %>% select(date, cpi = value), by = "date") %>% 
  mutate(real = value/cpi) %>% 
  select(date, industry_code, value = real) %>% 
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(industry_code) %>% 
  mutate(change = value/first(value) - 1)

p <- p %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) +
  geom_hline(yintercept = 0) +
  # xlim(ymd("2016-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = NULL,
       title = "Real total compensation vs prepandemic trend",
       subtitle = "Cumulative change since Q1 2016, seasonally adjusted",
       caption = "Note: Inflation based on Consumer Price Index | Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
          method = list(dl.trans(x = x - 5), "last.qp", cex = 1.5))  +
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

ggsave("charts/eci_real_comp_trend.png", p, device = "png", width = 14.2, height = 8)





p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  left_join(cpi_data %>% select(date, cpi = value), by = "date") %>% 
  mutate(real = value/cpi) %>% 
  select(date, industry_code, nominal = value, real) %>% 
  gather(series, value, -date, -industry_code) %>% 
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(series, industry_code) %>% 
  mutate(change = value/first(value) - 1)

p %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1) +
  facet_wrap(~series) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE)


# NSA CPI

cpi_data_nsa <- fred_tidy("CPIAUCNS")

p <- eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "U"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  left_join(cpi_data_nsa %>% select(date, cpi = value), by = "date") %>% 
  mutate(real = (cpi_data_nsa$value[cpi_data_nsa$date == ymd("2005-12-01")]) * value/cpi)

library(tidyseasonal)

p <- p %>% 
  nest(-industry_code) %>% 
  mutate(value_sa = map(data, ~seas_adjust(.x, date, value)),
         real_sa = map(data, ~seas_adjust(.x, date, real))) %>% 
  unnest

p

p %>% 
  filter(industry_code == "Leisure & hosp.") %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = real)) +
  geom_line(aes(y = real_sa), linetype = "dashed")


p <- p %>% 
  select(date, industry_code, value = real_sa) %>% 
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(industry_code) %>% 
  mutate(change = value/first(value) - 1)

q <- p %>% 
  ggplot(aes(date, change, colour = industry_code)) +
  geom_line(size = 1) +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) +
  geom_hline(yintercept = 0) +
  # xlim(ymd("2016-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_colour_manual(values = c(`Leisure & hosp.` = "#1f78b4", `Total private` = "#33a02c")) +
  labs(x = NULL, y = NULL,
       title = "Real wages and salaries vs prepandemic trend",
       subtitle = "Cumulative change since Q1 2016, seasonally adjusted",
       caption = "Note: Inflation based on Consumer Price Index | Source: Bureau of Labor Statistics") +
  geom_dl(aes(label = industry_code),
          method = list(dl.trans(x = x - 5.5, y = y - .5), "last.qp", cex = 1.5))  +
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

ggsave("charts/eci_real_trend.png", q, device = "png", width = 14.2, height = 8)



eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  left_join(cpi_data %>% select(date, cpi = value), by = "date") %>% 
  mutate(real = value/cpi) %>% 
  select(date, industry_code, nominal  = value, real) %>% 
  filter(date >= ymd("2015-12-01")) %>% 
  group_by(industry_code) %>% 
  mutate(nominal_change = nominal/first(nominal) - 1,
         real_change = real/first(real) - 1) %>% 
  rename(nominal_level = nominal, real_level = real) %>% 
  pivot_wider(names_from = industry_code,
              values_from = c("nominal_level", "real_level", "nominal_change", "real_change")) %>% 
  write_csv("real_wages_for_jeanna.csv")


p <- eci_data %>%
  filter(series_id %in% c("CIU2020000000710I"))

p$SA <- tidyseasonal::seas_adjust(p, date, value)

p %>% 
  select(date, value, SA) %>% 
  mutate(value = value/lag(value, 12, na.pad = T) - 1,
         SA = SA/lag(SA, 12, na.pad = T) - 1) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = SA), colour = "blue") +
  geom_line(aes(y = value), linetype = "dashed")
  
  >% 
  nest() %>% 
  mutate(sa = map_dbl(data, ~tidyseasonal::seas_adjust(.x, date, value)))



eci_data %>%
  filter(
    area_code == "99999",
    estimate_code %in% c("02"),
    owner_code == 2,
    periodicity_code == "I",
    subcell_code == "00",
    occupation_code == "000000",
    industry_code %in% c("000000", "700000"),
    seasonal == "S"
  ) %>%
  select(date, industry_code, seasonal, estimate_code, value) %>% 
  mutate(industry_code = factor(industry_code, levels = c("000000", "700000"),
                                labels = c("Total private", "Leisure & hosp."))) %>%
  group_by(industry_code) %>% 
  mutate(change = (value/lag(value, 1, na.pad = T))^4 - 1) %>%
  filter(year(date) >= 2010) %>% 
  arrange(desc(date)) %>% 
  filter(industry_code == "Leisure & hosp.")
  