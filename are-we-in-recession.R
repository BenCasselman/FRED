# Are we in a recession
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(ggtext)

# Get data
indicators <- c("PAYEMS", "UNRATE", "ICSA", "JTSJOR",
                "W875RX1", "AHETPI", "CPIAUCSL",
                "PCEC96", "UMCSENT", "RSAFS", "CMRMTSPL",
                "INDPRO", "PERMIT", "NEWORDER")

rec_inds <- fred_mult(series_id = indicators)

rec_inds %>% 
  filter(series_id == "UMCSENT") %>% 
  arrange(desc(date))

rec_inds <- tibble(date = c(ymd("2023-04-01")),
                   value = c(63.5),
                   series_id = "UMCSENT") %>% 
  bind_rows(rec_inds, .)

# rec_inds <- tibble(date = ymd("2022-08-01"),
#                    value = 295.271,
#                    series_id = "CPIAUCSL") %>% 
#   bind_rows(rec_inds, .)

rec_inds <- rec_inds %>% 
  mutate(value = case_when(series_id %in% c("UNRATE", "JTSJOR") ~ log(value),
                           TRUE ~ value))

rec_inds <- rec_inds %>% 
  mutate(group = case_when(series_id %in% c("PAYEMS", "UNRATE", "ICSA", "JTSJOR") ~ "jobs",
                           series_id %in% c("W875RX1", "AHETPI", "CPIAUCSL") ~ "income",
                           series_id %in% c("PCEC96", "UMCSENT", "RSAFS", "CMRMTSPL") ~ "spending",
                           series_id %in% c("INDPRO", "PERMIT", "NEWORDER") ~ "production"),
         type = case_when(series_id %in% c("PAYEMS", "W875RX1",  
                                           "PCE96", "RSAFS", "CMRMTSPL") ~ "trend",
                          series_id %in% c("CPIAUCSL", "AHETPI") ~ "chg",
                          TRUE ~ "level"),
         series_name = factor(series_id, 
                              levels = c("PAYEMS", "UNRATE", "ICSA", "JTSJOR",
                                        "W875RX1", "AHETPI", "CPIAUCSL",
                                        "PCEC96", "UMCSENT", "RSAFS", "CMRMTSPL",
                                        "INDPRO", "PERMIT", "NEWORDER"),
                              labels = c("payroll_jobs", "unemp_rate", "initial_claims", "openings_rate",
                                         "personal_income", "hourly_earnings", "consumer_prices",
                                         "consumer_spending", "mich_sentiment", "retail_sales", "mfg_trade_sales",
                                         "ind_prod", "bldg_permits", "capital_goods"))
  )

w <- rec_inds %>%
  filter(!is.na(value)) %>%
  group_by(series_id) %>%
  nest(date, value) %>%
  mutate(trend = map(data, function(x) {
    r <- x %>%
      filter(date >= ymd("2017-01-01"),
             date <= ymd("2019-12-31")) %>%
      lm(value ~ date, data = .)

    predict(r, newdata = x)
  })) %>%
  unnest()

w <- w %>% 
  group_by(series_id) %>% 
  mutate(value = case_when(type == "chg" ~ value/lag(value, 12, na.pad = T),
                           TRUE ~ value),
         prepandemic = case_when(year(date) %in% 2010:2019 ~ value,
                                 TRUE ~ NA_real_),
         prepandemic = mean(prepandemic, na.rm = T))

w <- w %>% 
  filter(year(date) >= 2010) %>% 
  mutate(measure = case_when(type == "trend" ~ value/trend,
                             type %in% c("level", "chg") ~ value/prepandemic),
         measure = case_when(series_id %in% c("UNRATE", "ICSA", "CPIAUCSL") ~ measure * (-1),
                             TRUE ~ measure),
         scaled = scale(measure),
         change = value/lag(value, 3, na.pad = T),
         change = case_when(series_id %in% c("UNRATE", "ICSA", "CPIAUCSL") ~ change * (-1),
                             TRUE ~ change),
         scaled_change = scale(change))

w2 <- w %>% 
  filter(year(date) >= 2010) %>% 
  mutate(measure = case_when(type == "trend" ~ value/trend,
                             type %in% c("level", "chg") ~ value/prepandemic),
         measure = case_when(series_id %in% c("UNRATE", "ICSA", "CPIAUCSL") ~ measure * (-1),
                             TRUE ~ measure),
         measure = case_when(type == "trend" & year(date) < 2017 ~ NA_real_,
                             TRUE ~ measure),
         scaled = scale(measure),
         change = value/lag(value, 3, na.pad = T),
         change = case_when(series_id %in% c("UNRATE", "ICSA", "CPIAUCSL") ~ change * (-1),
                            TRUE ~ change),
         change = case_when(type == "trend" & year(date) < 2017 ~ NA_real_,
                            TRUE ~ change),
         scaled_change = scale(change))


p <- w2 %>% 
  mutate(label = case_when(series_name== "payroll_jobs" ~ "Payroll\nemployment", 
                           series_name == "unemp_rate" ~ "Unemployment\nrate",
                           series_name == "initial_claims" ~ "Unemployment\nclaims", 
                           series_name == "openings_rate" ~ "Job\nopenings",
                           series_name == "personal_income" ~ "Personal\nincome", 
                           series_name == "hourly_earnings" ~ "Hourly\nearnings\ngrowth", 
                           series_name == "consumer_prices" ~ "Inflation",
                           series_name == "consumer_spending" ~ "Consumer\nspending", 
                           series_name == "mich_sentiment" ~ "Consumer\nsentiment", 
                           series_name == "retail_sales" ~ "Retail sales", 
                           series_name == "mfg_trade_sales" ~ "Manufacturing\nand trade sales",
                           series_name == "ind_prod" ~ "Industrial\nproduction", 
                           series_name == "bldg_permits" ~ "Building\npermits", 
                           series_name == "capital_goods" ~ "Capital\ngoods")) %>% 
  filter(date == max(date)) %>% 
  ggplot(aes(scaled, scaled_change, fill = group, label = label)) +
  geom_label(nudge_x = .5, colour = "white") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Level", y = "Change") +
  theme(text = element_text(size = 10)) +
  xlim(-3,3) +
  scale_fill_manual(values = c(jobs = "#5c9f83", income = "#84a2b2", spending = "#7c5981", production = "#c3823b")) +
  annotate("text", x = 1.25, y = .8, label = "GOOD AND\nGETTING BETTER", colour = "grey75") +
  annotate("text", x = 1.25, y = -.8, label = "GOOD AND\nGETTING WORSE", colour = "grey75") +
  annotate("text", x = -1.25, y = -.8, label = "BAD AND\nGETTING WORSE", colour = "grey75") +
  annotate("text", x = -1.25, y = .8, label = "BAD AND\nGETTING BETTER", colour = "grey75") +
  labs(x = "Current level",
       y = "Recent change",
       title = "How is the economy doing right now?",
       subtitle = "How conditions are faring for <span style='color:#5c9f83;'>jobs</span>,
       <span style='color:#84a2b2;'>income</span>,
       <span style='color:#7c5981;'>consumers</span> and
       <span style='color:#c3823b;'>production</span>",
       caption = "Note: Data updated through 05/03/2023. Methodology at https://www.nytimes.com/interactive/2022/09/13/business/economy/us-economy.html\nSource: Federal Reserve Bank of St. Louis") +
  theme(
    plot.title = element_text(size = 26),
    plot.subtitle = element_markdown(size = 20),
    plot.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(colour = "grey50", size = 12),
    strip.text = element_text(size = 16),
    legend.position = "none",
    axis.title = element_text(size = 16)
  )

ggsave(
  "charts/in-recession-2023-05-03.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)

p <- w2 %>% 
  filter(date == max(date)) %>% 
  ggplot(aes(scaled, scaled_change, colour = group, label = series_name)) +
  geom_point(size = 3) +
  geom_text(nudge_x = .5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Level", y = "Change") +
  theme(text = element_text(size = 10))




w %>% 
  mutate(scaled_3mo = rollmean(scaled, 3, align = "right", na.pad = T),
         scaled_chg_3mo = rollmean(scaled_change, 3, align = "right", na.pad = T)) %>% 
  filter(date == max(date)) %>% 
  ggplot(aes(scaled_3mo, scaled_chg_3mo, colour = group, label = series_name)) +
  geom_point(size = 3) +
  geom_text(nudge_x = .5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Level", y = "Change") +
  theme(text = element_text(size = 10)) 
  
  



w %>% 
  filter(series_id == "CMRMTSPL",
         year(date) >= 2010) %>% 
  ggplot(aes(date)) +
  geom_line(aes(y = value), colour = "blue") +
  geom_line(aes(y = trend), linetype = "dashed")

w %>% 
  ggplot(aes(scaled)) +
  geom_histogram() +
  facet_wrap(~series_name)

w %>% 
  filter(series_id == "UNRATE") %>% 
  mutate(scaled = scale(measure)) %>%  
  # select(value, measure, scaled)
  ggplot(aes(scaled)) +
  geom_histogram()

w %>% 
  filter(series_id == "UNRATE") %>% 
  select(-group, -type, -series_name)

w %>% 
  filter(series_id == "UNRATE") %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  geom_line(aes(y = prepandemic))


rec_inds %>% 
  filter(year(date) >= 2000,
         series_id == "UMCSENT") %>% 
  ggplot(aes(date, value)) +
  geom_line()