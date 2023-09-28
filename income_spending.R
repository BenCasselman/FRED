# Personal income and spending
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(directlabels)
source("BEAR.R")
library(directlabels)

income <- bea_all("T20600", freq = "M")
spending <- bea_all("T20805", freq = "M")
real_spending <- bea_all("T20806", freq = "M")
pce <- bea_all("T20804", freq = "M")
spend_detail <- bea_all("U20405", freq = "M", dataset = "NIUnderlyingDetail")
real_spend_detail <- bea_all("U20406", freq = "M", dataset = "NIUnderlyingDetail")
# price_detail <- bea_all("U20404", freq = "M", dataset = "NIUnderlyingDetail")

pce_detail <- bea_all("U20404", freq = "M", dataset = "NIUnderlyingDetail")

income %>% 
  filter(date == max(date))

# Relative to prepandemic
prepandemic(income,32, "2020-02-01")

# Highest inflation since
pce %>% 
  filter(LineNumber == "1") %>%
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  filter(change >= change[date == max(date)]) %>% 
  arrange(desc(date))

prepandemic <- function(df = ., line = 1, startdate = "2020-01-01") {
  df %>% 
    filter(date >= ymd(startdate),
           LineNumber == line) %>% 
    mutate(change = DataValue/first(DataValue) - 1) %>%
    select(date, LineNumber, LineDescription, DataValue, change) %>% 
    arrange(desc(date))
}

income %>% 
  filter(LineNumber %in% c("1", "3", "21"),
         year(date) >= 2019) %>% 
  select(date, LineDescription, DataValue) %>% 
  spread(LineDescription, DataValue) %>% 
  mutate(`All other` = `Personal income` - `Unemployment insurance` - `Wages and salaries`) %>% 
  select(-`Personal income`) %>% 
  gather(series, value, -date) %>% 
  ggplot(aes(date, value, fill = series)) + geom_col()



# Income vs spending
# p <- income %>% 
#   filter(LineNumber %in% c("27", "29"),
#          year(date) >= 2020) %>% 
#   mutate(LineNumber = factor(LineNumber, levels = c("27", "29"),
#                              labels = c("After-tax income", "Consumer spending"))) %>% 
#   ggplot(aes(date, DataValue/1000000, colour = LineNumber)) +
#   geom_line(size = 1, show.legend = F)
# 
# p <- p +
#   ylim(0, 25) +
#   xlim(ymd("2020-01-01"), ymd("2021-10-01")) +
#   geom_hline(yintercept = 0) +
#   labs(x = NULL, y = NULL,
#        title = "Personal income and spending",
#        subtitle = "Billions of dollars at seasonally adjusted annual rates",
#        caption = "Source: Bureau of Economic Analysis") +
#   geom_dl(aes(label = LineNumber),
#           method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
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
#     legend.position = "none")
# 
# ggsave("charts/income_v_spending.png", p, device = "png", width = 14.2, height = 8)


p <- income %>% 
  filter(LineNumber %in% c("27", "29"),
         year(date) >= 2020) %>% 
  mutate(LineNumber = factor(LineNumber, levels = c("27", "29"),
                             labels = c("After-tax income", "Consumer spending"))) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineNumber)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  xlim(ymd("2020-01-01"), ymd("2023-05-01")) +
  scale_color_manual(values = c(`Consumer spending` = "#1f78b4", `After-tax income` = "#33a02c")) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Nominal personal income and spending",
       subtitle = "Change from January, 2020, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = LineNumber),
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

ggsave("charts/COVID charts/M_income_v_spending_indexed.png", p, device = "png", width = 14.2, height = 8)


p <- income %>% 
  filter(LineNumber %in% c("37"),
         year(date) >= 2020) 
p <- real_spending %>% 
  filter(LineNumber == "1",
         year(date) >= 2020) %>% 
  bind_rows(p)

p <- p %>% 
  mutate(LineNumber = factor(LineNumber, levels = c("37", "1"),
                             labels = c("After-tax income", "Consumer spending"))) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineNumber)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  xlim(ymd("2020-01-01"), ymd("2023-05-01")) +
  scale_color_manual(values = c(`Consumer spending` = "#1f78b4", `After-tax income` = "#33a02c")) +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Real personal income and spending",
       subtitle = "Change from January, 2020, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = LineNumber),
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

ggsave("charts/COVID charts/M_real_income_v_spending_indexed.png", p, device = "png", width = 14.2, height = 8)


# Saving
p <- income %>% 
  filter(LineNumber == "34",
         year(date) >= 2012) %>% 
  mutate(covid = case_when(date >= ymd("2020-03-01") ~ "COVID",
                           TRUE ~ "not")) %>% 
  ggplot(aes(date, DataValue/1000000, fill = covid)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(COVID = "#1f78b4", not = "#a6cee3"))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Monthly personal savings",
       subtitle = "Billions of dollars, seasonally adjusted annual rates",
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

ggsave("charts/COVID charts/saving.png", p, device = "png", width = 14.2, height = 8)

p <- income %>% 
  filter(LineNumber == "35",
         year(date) >= 2014) %>% 
  mutate(DataValue = DataValue/100,
         pre_covid = case_when(date <= ymd("2020-01-01") ~ DataValue),
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

ggsave("charts/COVID charts/saving_rate.png", p, device = "png", width = 14.2, height = 8)



income %>% 
  filter(LineNumber == "35") %>% 
  filter(DataValue <= 2.3) %>% 
  arrange(desc(date))


p <- income %>% 
  filter(LineNumber %in% c("3", "21", "23"),
         year(date) >= 2019) %>% 
  mutate(source = case_when(LineNumber == "3" ~ "Wages and salaries",
                            TRUE ~ "Unemployment insurance")) %>% 
  group_by(date, source) %>% 
  summarize(DataValue = sum(DataValue)) %>% 
  ggplot(aes(date, DataValue/1000000, fill = source)) + 
  geom_col()

p <- p +
  labs(x = NULL, y = NULL, 
       title = "Income from wages and unemployment benefits",
       subtitle = "Billions of dollars, seasonally adjusted annual rates",
       caption = "Note: Unemployment insurance includes Lost Wages Assistance and other uncategorized transfers. | Source: Bureau of Economic Analysis") +
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

ggsave("charts/ui_benefits.png", p, device = "png", width = 14.2, height = 8)


p <- income %>% 
  filter(LineNumber %in% c("3", "16"),
         year(date) >= 2019) %>% 
  ggplot(aes(date, DataValue/1000000, fill = LineDescription)) + 
  geom_col()

p <- p +
  scale_fill_manual(values = c(`Wages and salaries` = "#1f78b4",
                               `Personal current transfer receipts` = "#a6cee3")) +
  labs(x = NULL, y = NULL, 
       title = "Income from wages and government transfers",
       subtitle = "Billions of dollars, seasonally adjusted annual rates",
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

ggsave("charts/COVID charts/transfers.png", p, device = "png", width = 14.2, height = 8)


p <- income %>% 
  filter(LineNumber %in% c("3", "16")) %>% 
  group_by(LineNumber) %>% 
  mutate(chg = DataValue - lag(DataValue, 1, na.pad = T),
         chg = chg/1000000) %>% 
  filter(date >= ymd("2021-07-01")) %>% 
  ggplot(aes(date, chg, fill = LineDescription)) + 
  geom_col() +
  geom_hline(yintercept = 0)

p <- p +
  scale_fill_manual(values = c(`Wages and salaries` = "#1f78b4",
                               `Personal current transfer receipts` = "#a6cee3")) +
  labs(x = NULL, y = NULL, 
       title = "Monthly change in income from wages and government transfers",
       subtitle = "Billions of dollars, seasonally adjusted annual rates",
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

ggsave("charts/COVID charts/transfers.png", p, device = "png", width = 14.2, height = 8)




# Goods vs services
p <- spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2020) %>% 
  group_by(LineDescription) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  xlim(ymd("2020-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Spending on goods vs services",
       subtitle = "Change since January, 2020, seasonally adjusted",
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

ggsave("charts/COVID charts/M_goods_v_svces.png", p, device = "png", width = 14.2, height = 8)



p <- spending %>% 
  filter(LineNumber %in% c("3", "4"),
         year(date) >= 2020) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  xlim(ymd("2020-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Spending on durable vs nondurable goods",
       subtitle = "Change since January, 2020, seasonally adjusted",
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

ggsave("charts/durable_vs_non.png", p, device = "png", width = 14.2, height = 8)


# 
# p <- real_spending %>% 
#   filter(LineNumber == "1",
#          year(date) >= 2020) %>% 
#   mutate(actual = "actual") %>% 
#   bind_rows(tibble(date = ymd("2021-12-01"),
#                    DataValue = 13040653, actual = "est")) %>% 
#   arrange(date) %>% 
#   mutate(q = as.character(as.yearqtr(date))) %>% 
#   select(date, q, actual, DataValue) %>% 
#   group_by(q) %>% 
#   mutate(avg = mean(DataValue)) %>% 
#   ggplot(aes(date, DataValue/1000000, fill = q, alpha = actual)) +
#   geom_col(show.legend = F) +
#   geom_line(aes(y = avg/1000000), linetype = "dashed", size = 1.5, colour = "black", show.legend = F) +
#   scale_alpha_manual(values = c(actual = 1, est = 0.5)) +
#   scale_fill_manual(values = c("#6baed6", "#3182bd", "#08519c", "#e66101"))
# 
# p <- p +
#   geom_hline(yintercept = 0) +
#   labs(x = NULL, y = NULL,
#        title = "Monthly and quarterly real consumer spending",
#        subtitle = "In trillions of dollars. Dashed lines are quarterly averages. Assumes zero monthly growth in December.",
#        caption = "Source: Bureau of Economic Analysis") +
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
#     legend.position = "none",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 16))
# 
# 
# ggsave("charts/spending_momentum.png", p, device = "png", width = 14.2, height = 8)


real_spending %>% 
  filter(LineNumber == "1",
        date >= ymd("2019-10-01")) %>% 
  mutate(actual = "actual") %>% 
  bind_rows(tibble(date = c(ymd("2020-12-01")),
                   DataValue = 13040653*(1-.004), actual = "est")) %>%
  arrange(date) %>% 
  mutate(q = as.yearqtr(date)) %>% 
  group_by(q) %>% 
  summarize(avg = mean(DataValue)) %>% 
  mutate(chg = avg/lag(avg, 1, na.pad = T) -1,
         ann = (avg/lag(avg, 1, na.pad = T))^4 - 1,
         cum = avg/first(avg) - 1)


real_spending %>% 
  filter(LineNumber == "1",
         date >= ymd("2019-10-01")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  select(date, DataValue, change)



p <- real_spending %>% 
  filter(LineNumber %in% c("3", "4", "5"),
         date >= ymd("2020-02-01")) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  xlim(ymd("2020-02-01"), ymd("2023-05-15")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Inflation-adjusted spending on goods and services",
       subtitle = "Change since February, 2020, seasonally adjusted.",
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

ggsave("charts/real_monthly_spend.png", p, device = "png", width = 14.2, height = 8)



income %>% 
  filter(LineNumber== "1",
         year(date) >= 2020) %>% 
  mutate(LineNumber = factor(LineNumber, levels = c("27", "29"),
                             labels = c("After-tax income", "Consumer spending")),
         change = DataValue/first(DataValue) - 1)


income %>% 
  filter(LineNumber == "1") %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(desc(date))


p <- pce %>% 
  filter(LineNumber %in% c("1", "6"),
         year(date) >= 2010) %>%
  mutate(series = case_when(LineNumber == "1" ~ "Headline",
                            LineNumber == "6" ~ "Core"),
         LineNumber = factor(LineNumber, levels = c("1", "6"), labels = c("Headline", "Core"))) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  ggplot(aes(date, change, colour = series)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_colour_manual(values = c(Headline = "#a6cee3", Core = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2016-01-01"), ymd("2023-05-15")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, headline vs core",
       subtitle = "Percent change from a year ago, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
   geom_dl(aes(label = LineNumber),
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

ggsave("charts/COVID charts/M_prices.png", p, device = "png", width = 14.2, height = 8)


p <- pce %>% 
  filter(LineNumber %in% c("1", "6")) %>%
  mutate(series = case_when(LineNumber == "1" ~ "Headline",
                            LineNumber == "6" ~ "Core"),
         LineNumber = factor(LineNumber, levels = c("1", "6"), labels = c("Headline", "Core"))) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  ggplot(aes(date, change, colour = series)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_colour_manual(values = c(Headline = "#a6cee3", Core = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("1960-01-01"), ymd("2028-12-15")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, headline vs core",
       subtitle = "Percent change from a year ago, seasonally adjusted. Shaded areas denote recessions.",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = LineNumber),
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
    legend.position = "none") +
  recession_shade("1960-01-01")

ggsave("charts/COVID charts/M_prices_full.png", p, device = "png", width = 14.2, height = 8)



# 3-month avg
p <- pce %>% 
  filter(LineNumber %in% c("1", "6"),
         year(date) >= 2010) %>%
  mutate(series = case_when(LineNumber == "1" ~ "Headline",
                            LineNumber == "6" ~ "Core")) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = (DataValue/lag(DataValue, 3, na.pad = T))^4 - 1) %>% 
  ggplot(aes(date, change, colour = series)) +
  geom_line(size = 1, show.legend = F) 

p <- p +
  scale_colour_manual(values = c(Headline = "#a6cee3", Core = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2016-01-01"), ymd("2023-05-15")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, headline vs core",
       subtitle = "Annualized three-month change, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = series),
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

ggsave("charts/COVID charts/M_prices_3m.png", p, device = "png", width = 14.2, height = 8)


# 1-month avg
p <- pce %>% 
  filter(LineNumber %in% c("1", "6"),
         year(date) >= 2010) %>%
  mutate(series = case_when(LineNumber == "1" ~ "Headline",
                            LineNumber == "6" ~ "Core")) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = (DataValue/lag(DataValue, 1, na.pad = T))^12 - 1) %>% 
  ggplot(aes(date, change, colour = series)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_colour_manual(values = c(Headline = "#a6cee3", Core = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2016-01-01"), ymd("2023-05-15")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, headline vs core",
       subtitle = "One-month annualized change, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = series),
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

ggsave("charts/COVID charts/M_prices_1m.png", p, device = "png", width = 14.2, height = 8)

# Goods vs services inflation
p <- pce %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2010) %>%
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_colour_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2019-01-01"), ymd("2023-05-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, goods vs. services",
       subtitle = "Percent change from a year ago, seasonally adjusted",
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

ggsave("charts/COVID charts/M_goods_vs_svces_inflation.png", p, device = "png", width = 14.2, height = 8)

p <- pce %>% 
  filter(LineNumber %in% c("3", "4", "5"),
         year(date) >= 2010) %>%
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = (DataValue/lag(DataValue, 3, na.pad = T))^4 - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  scale_colour_manual(values = c(`Durable goods` = "#1f78b4", `Nondurable goods` = "#a6cee3", Services = "#33a02c")) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2019-01-01"), ymd("2023-09-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, durable and nondurable goods vs. services",
       subtitle = "Three-month annualized change, seasonally adjusted",
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

ggsave("charts/COVID charts/M_goods_vs_svces_inflation_3mo.png", p, device = "png", width = 14.2, height = 8)


p <- income %>% 
  filter(LineNumber == "1") %>% 
  arrange(date) %>% 
  mutate(covid = case_when(date >= ymd("2020-03-01") ~ "COVID",
                           TRUE ~ "not"),
         change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>%
  ggplot(aes(date, change, fill = covid)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(COVID = "red", not = "grey50"))

p <- p +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in personal income",
       subtitle = "Monthly change, seasonally adjusted",
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

ggsave("charts/income_long.png", p, device = "png", width = 14.2, height = 8)


p <- income %>% 
  filter(LineNumber == "34") %>% 
  arrange(date) %>% 
  mutate(covid = case_when(date >= ymd("2020-03-01") ~ "COVID",
                           TRUE ~ "not"),
         change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>%
  ggplot(aes(date, change, fill = covid)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(COVID = "red", not = "grey50"))

p <- p +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Monthly change in personal savings",
       subtitle = "Monthly change, seasonally adjusted",
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

ggsave("charts/saving_long.png", p, device = "png", width = 14.2, height = 8)


income %>% 
  filter(LineNumber %in% c("1", "34")) %>%
  write_csv("income_saving.csv")

income %>% 
  filter(LineNumber %in% c("1")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(desc(date))

spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2020) %>% 
  group_by(LineDescription) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(desc(date))




p <- income %>% 
  filter(LineNumber %in% c("1", "2", "16"),
         year(date) >= 2019) %>% 
  arrange(date) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  rename(total = `1`, comp = `2`, transfer = `16`) %>% 
  mutate(other = total - comp - transfer) %>% 
  select(-total) %>% 
  gather(series, value, -date) %>% 
  mutate(series = factor(series, levels = c("transfer", "other", "comp"),
                         labels = c("Transfers", "Other", "Compensation"))) %>% 
  ggplot(aes(date, value/1000000, fill = series, alpha = series)) +
  geom_col()

p <- p +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#00BFC4")) +
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
  "charts/COVID_income_components_m.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)





p <- pce %>% 
  filter(LineNumber %in% c("1", "6"),
         year(date) >= 2010) %>%
  mutate(LineNumber = factor(LineNumber, levels = c("1", "6"), labels = c("Headline", "Core"))) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/lag(DataValue, 3, na.pad = T) - 1,
         change = (change +1)^4 - 1) %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend = F)

p <- p +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = .02, linetype = "dashed") +
  xlim(ymd("2016-01-01"), ymd("2021-08-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "PCE Price Index, headline vs core",
       subtitle = "Percent change from a year ago, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = LineNumber),
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

ggsave("charts/prices.png", p, device = "png", width = 14.2, height = 8)



t <- read_tsv("Year	Jan	Feb	Mar	Apr	May	Jun	Jul	Aug	Sep	Oct	Nov	Dec
1993	194,150	194,689	190,185	198,296	198,835	203,175	208,760	212,238	214,446	214,778	227,235	235,648
1994	238,612	236,577	235,150	244,746	244,056	244,032	245,463	243,134	242,619	234,905	239,857	241,395
1995	241,184	234,257	228,013	225,488	222,062	218,972	224,113	226,045	228,688	227,589	231,913	235,572
1996	240,958	243,695	249,952	252,836	259,296	263,085	263,549	263,618	265,677	261,444	259,668	253,234
1997	255,365	258,595	264,566	257,490	264,581	262,299	261,593	263,951	270,511	272,980	272,786	269,192
1998	276,485	279,249	291,505	288,409	292,879	296,632	296,717	299,495	305,345	308,127	307,899	302,650
1999	305,968	307,914	316,822	316,111	320,326	322,905	320,854	326,779	334,947	342,423	348,035	346,989
2000	353,065	351,933	353,452	356,188	349,907	348,133	337,374	337,583	339,048	344,095	347,301	344,139
2001	348,279	353,629	350,029	361,991	360,534	365,905	366,500	369,000	368,809	370,957	375,803	376,279
2002	377,631	385,745	385,406	398,990	394,223	401,920	404,632	398,875	394,820	397,077	401,424	411,758
2003	417,788	417,500	413,303	420,510	421,143	430,929	448,917	453,031	458,676	469,855	485,245	503,356
2004	498,640	500,439	506,879	512,400	522,534	527,794	543,260	551,447	546,306	551,137	550,392	566,284
2005	575,650	588,435	591,580	598,335	603,707	616,958	628,750	633,710	647,374	658,590	665,376	670,522
2006	675,679	678,562	668,311	650,388	633,815	615,053	598,487	584,460	573,878	561,444	548,971	545,011
2007	537,571	531,641	527,865	523,666	514,131	505,936	495,976	480,974	465,709	447,819	431,878	416,486
2008	402,138	386,197	390,471	385,328	381,911	374,495	364,873	352,598	340,396	329,304	312,540	293,997
2009	283,870	267,938	254,645	243,958	237,188	237,343	244,141	250,768	254,135	254,942	255,941	255,801
2010	254,826	255,121	253,924	254,574	252,896	250,518	244,354	238,695	237,834	236,512	237,468	239,030
2011	240,548	240,062	241,586	244,598	245,449	247,520	249,770	251,877	250,152	252,821	247,279	247,983
2012	251,869	247,503	252,283	258,567	263,797	269,664	273,587	279,216	286,120	294,197	296,510	299,101
2013	303,792	309,078	312,083	316,537	321,706	324,770	328,388	335,690	339,977	346,273	353,317	360,754
2014	367,522	364,391	370,368	375,235	377,661	371,860	371,532	373,163	381,377	385,996	392,280	402,301
2015	405,483	411,534	412,105	415,354	423,855	432,084	437,549	438,637	446,379	448,718	447,590	454,428
2016	458,300	459,376	472,356	469,303	468,554	472,390	475,371	478,473	480,518	497,911	504,344	514,603
2017	513,586	536,522	530,177	534,992	534,302	533,206	539,722	539,160	541,370	539,543	560,127	568,337
2018	575,299	585,549	576,915	575,694	574,864	564,660	556,155	547,261	547,817	536,260	534,182	522,581
2019	528,328	523,569	525,424	530,800	532,148	538,372	543,830	552,508	555,879	556,693	566,916	572,387
2020	589,103	592,960	588,060	562,339	539,555	553,721	575,451	615,396	620,941	650,484	670,427	704,079
2021	712,427	713,056	725,245									
")

t %>% 
  gather(month, value, -Year) %>% 
  mutate(date = ymd(paste(Year, month, "01", sep = "-"))) %>% 
  ggplot(aes(date, value/1000)) + geom_line(size = 1) +
  labs(x = NULL, y = NULL, title = "New residential construction",
       subtitle = "Millions of dollars, seasonally adjusted")
# 

spend_detail %>% 
  filter(as.numeric(LineNumber) %in% 149:337) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date == ymd("2021-05-01")) %>% 
  arrange(desc(change)) %>% 
  View()

spend_detail %>% 
  filter(as.numeric(LineNumber) %in% 149:337,
         date >= ymd("2020-02-01")) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  filter(date == ymd("2021-05-01")) %>% 
  arrange(desc(change)) %>% 
  View("since pandemic")


p <- spend_detail %>% 
  filter(SeriesCode %in% c("DAITRC", "DHOTRC", "DPMBRC", "DLIGRC", "DBBBRC", "DMOVRC", "DSPERC"),
         date >= ymd("2020-02-01")) %>% 
  mutate(SeriesCode = factor(SeriesCode, 
                             levels = c("DAITRC", "DHOTRC", "DPMBRC", "DLIGRC", "DBBBRC", "DMOVRC", "DSPERC"),
                             labels = c("Air travel", "Hotels", "Restaurants", "Live entertainment",
                                        "Personal care", "Movies", "Sports"))) %>% 
  group_by(SeriesCode) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = SeriesCode)) + 
  geom_line(size = 1) +
  geom_hline(yintercept = 0)

p <- p +
  xlim(ymd("2020-02-01"), ymd("2022-12-01")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Services spending in the pandemic",
       subtitle = "Change in consumer spending since Feb. 2020, select categories",
       caption = "Note: 'Restaurants' includes all purchased meals. | Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = SeriesCode),
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

ggsave("charts/svc_detail.png", p, device = "png", width = 14.2, height = 8)


spend_detail %>% 
  filter(SeriesCode %in% c("DAITRC", "DHOTRC", "DPMBRC", "DLIGRC", "DBBBRC", "DMOVRC", "DSPERC"),
         date >= ymd("2020-02-01")) %>% 
  mutate(SeriesCode = factor(SeriesCode, 
                             levels = c("DAITRC", "DHOTRC", "DPMBRC", "DLIGRC", "DBBBRC", "DMOVRC", "DSPERC"),
                             labels = c("Air travel", "Hotels", "Restaurants", "Live entertainment",
                                        "Personal care", "Movies", "Sports"))) %>% 
  group_by(SeriesCode) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  select(date, SeriesCode, change) %>% 
  spread(SeriesCode, change) %>% 
  write_csv("spend_detail.csv")


spend_detail %>% 
  group_by(SeriesCode) %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date == max(date)) %>% 
  arrange(desc(chg))

real_pce_archive <- real_spending %>% 
  arrange(date) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1)
  
real_pce_archive %>% 
  arrange(desc(date))

real_spending %>% 
  arrange(date) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date <= ymd("2021-05-01")) %>% 
  arrange(desc(date))




p <- real_spending %>% 
  filter(LineNumber == "1",
         date >= ymd("2021-07-01")) %>% 
  mutate(actual = "actual") %>% 
  # bind_rows(tibble(date = ymd("2021-07-01"),
  #                  DataValue = 13675723, actual = "est")) %>% 
  arrange(date) %>% 
  mutate(q = as.character(as.yearqtr(date))) %>% 
  select(date, q, actual, DataValue) %>% 
  group_by(q) %>% 
  mutate(avg = mean(DataValue)) %>% 
  ggplot(aes(date, DataValue/1000000, fill = q, alpha = actual)) +
  geom_col(show.legend = F) +
  geom_line(aes(y = avg/1000000), linetype = "dashed", size = 1.5, colour = "black", show.legend = F) +
  scale_alpha_manual(values = c(actual = 1, est = 0.5)) +
  scale_fill_manual(values = c("#6baed6", "#3182bd", "#08519c", "#e66101"))

p <- p +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Monthly and quarterly real consumer spending",
       subtitle = "In trillions of dollars. Dashed lines are quarterly averages. Assumes zero monthly growth in December.",
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
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 16))


ggsave("charts/spending_momentum.png", p, device = "png", width = 14.2, height = 8)



homebase <- read_csv("~/coronavirus/coronavirus/Homebase/industry_v2_082721.csv")
homebase <- homebase %>% 
  mutate(Date = mdy(Date))

homebase %>% 
  filter(Date <= ymd("2021-08-20")) %>% 
  group_by(`Business Type`) %>% 
  arrange(Date) %>% 
  mutate(roll = zoo::rollmean(`Hours Worked`, 7, na.pad = T, align = "right")) %>% 
  ggplot(aes(Date, roll, colour = `Business Type`)) +
  geom_line(size = 1)


income %>% 
  filter(LineNumber %in% c("27", "37"),
         date >= ymd("2020-02-01")) %>% 
  mutate(LineNumber = factor(LineNumber, levels = c("27", "37"), labels = c("Nominal", "Real"))) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/first(DataValue) - 1) %>% 
  ggplot(aes(date, change, colour = LineNumber)) +
  geom_line()


dfa <- read_csv("~/Fed data/DFA/dfa-income-levels-detail.csv")

dfa %>% 
  mutate(date = ymd(paste(substr(Date, 1, 4), as.numeric(substr(Date, 7, 7)) * 3 - 2, "01", sep = "-"))) %>% 
  group_by(date, Category) %>% 
  summarize(cash = sum(`Checkable deposts and currency`)) %>% 
  filter(date >= ymd("2019-10-01")) %>% 
  group_by(Category) %>% 
  mutate(change = cash - first(cash),
         Category = factor(Category),
         Category = forcats::fct_rev(Category)) %>% 
  ggplot(aes(date, change, fill = Category)) +
  geom_col()



read_tsv("01/01/2010	2.0
02/01/2010	-92.0
03/01/2010	181.0
04/01/2010	231.0
05/01/2010	540.0
06/01/2010	-139.0
07/01/2010	-84.0
08/01/2010	-5.0
09/01/2010	-65.0
10/01/2010	268.0
11/01/2010	125.0
12/01/2010	72.0
01/01/2011	19.0
02/01/2011	212.0
03/01/2011	235.0
04/01/2011	314.0
05/01/2011	101.0
06/01/2011	236.0
07/01/2011	60.0
08/01/2011	126.0
09/01/2011	233.0
10/01/2011	204.0
11/01/2011	132.0
12/01/2011	202.0
01/01/2012	354.0
02/01/2012	262.0
03/01/2012	240.0
04/01/2012	82.0
05/01/2012	100.0
06/01/2012	73.0
07/01/2012	152.0
08/01/2012	172.0
09/01/2012	187.0
10/01/2012	159.0
11/01/2012	156.0
12/01/2012	239.0
", col_names = c("date", "value")) %>% 
  mutate(date = mdy(date),
         aug = case_when(date == ymd("2011-08-01") ~ "y", 
                         TRUE ~ "n")) %>% 
  ggplot(aes(date, value, fill = aug)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c(y = "#1f78b4", n = "grey75")) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Monthly job growth, 2010-12",
       subtitle = "In thousands, seasonally adjusted. Aug. 2011 highlighted.",
       caption = "Source: Bureau of Labor Statistics") +
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




# Excess saving
p <- income %>% 
  filter(LineNumber == "34",
         date >= ymd("2019-01-01")) %>% 
  mutate(cum_savings = cumsum(DataValue/12000000)) %>% 
  select(date, DataValue, cum_savings)

r <- p %>% 
  filter(date < ymd("2020-03-01")) %>% 
  lm(cum_savings ~ date, data = .)

p <- p %>% 
  mutate(trend = map_dbl(date, ~predict(r, newdata = tibble(date = .x))))

p %>% 
  mutate(excess = cum_savings - trend) %>% 
  arrange(desc(date)) %>% 
  head(20)

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
       caption = "Note: Monthly data, not annualized. | Source: Bureau of Economic Analysis") +
  annotate("text", x = ymd("2021-08-01"), y = 4.9,
           label = "Pandemic savings", colour = "#1f78b4", size = 5.5) +
  geom_curve(aes(x = ymd("2020-05-01"), y = 1.5,
                 xend = ymd("2020-04-01"), yend = 1.75),
             curvature = -.2, arrow = arrow(length = unit(.02, "npc"))) +
  annotate("text", x = ymd("2020-08-01"), y = 1.5,
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
  "charts/COVID charts/M_excess_savings.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


spending %>% 
  filter(LineNumber %in% c("3"),
         year(date) >= 2020) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(desc(date)) %>% 
  head(20)

spend_detail %>% 
  filter(LineNumber == "23") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(desc(date)) %>% 
  head(20)


spend_detail %>% 
  filter(as.numeric(LineNumber) %in% 3:69) %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date == ymd("2021-04-01")) %>% 
  arrange(desc(change)) %>% 
  head(20)


spend_detail %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date == ymd("2021-04-01")) %>% 
  arrange(desc(change)) %>% 
  head(20)



p <- spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2019) %>% 
  group_by(LineDescription) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/first(DataValue) - 1,
         df_2019 = case_when(year(date) == 2019 ~ DataValue,
                             TRUE ~ NA_real_),
         df_2019 = mean(df_2019, na.rm = T),
         index = 100*DataValue/df_2019)

p <- p %>% 
  ggplot(aes(date, index, colour = LineDescription)) +
  geom_line(size = 1, show.legend =  F) +
  # geom_hline(yintercept = 0) +
  stat_smooth(data = subset(p, date < ymd("2020-02-01")), 
              method = "glm", 
              method.args = list(family=gaussian(link="log")),
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) 

p <- p +
  scale_color_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  scale_x_date(limits = c(ymd("2019-01-01"), ymd("2023-07-01"))) +
  labs(x = NULL, y = NULL,
       title = "Nominal consumer spending on goods and services vs prepandemic trends",
       subtitle = "Index, 2019 = 100, seasonally adjusted",
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

ggsave("charts/COVID charts/M_goods_v_svces_vs_trend.png", p, device = "png", width = 14.2, height = 8)


p <- real_spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2019) %>% 
  group_by(LineDescription) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/first(DataValue) - 1,
         df_2019 = case_when(year(date) == 2019 ~ DataValue,
                             TRUE ~ NA_real_),
         df_2019 = mean(df_2019, na.rm = T),
         index = 100*DataValue/df_2019)

p <- p %>% 
  ggplot(aes(date, index, colour = LineDescription)) +
  geom_line(size = 1, show.legend =  F) +
  # geom_hline(yintercept = 0) +
  stat_smooth(data = subset(p, date < ymd("2020-02-01")), 
              method = "glm", 
              method.args = list(family=gaussian(link="log")),
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) 

p <- p +
  scale_color_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  scale_x_date(limits = c(ymd("2019-01-01"), ymd("2023-05-01"))) +
  labs(x = NULL, y = NULL,
       title = "Real consumer spending on goods and services vs prepandemic trends",
       subtitle = "Index, 2019 = 100, seasonally adjusted",
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

ggsave("charts/COVID charts/M_real_goods_v_svces_vs_trend.png", p, device = "png", width = 14.2, height = 8)



# Goods share
p <- spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 1992) %>% 
  select(date, LineDescription, DataValue) %>% 
  spread(LineDescription, DataValue) %>% 
  mutate(ratio = Goods/Services)

p <- p %>% 
  ggplot(aes(date, ratio)) +
  geom_line(size = 1, color = "#1f78b4") +
  scale_y_continuous(labels = percent, limits = c(.4, .62)) +
  labs(x = NULL, y = NULL,
       title = "Goods spending as a share of services spending",
       subtitle = "Seasonally adjusted. Shaded areas denote recessions.",
       caption = "Source: Bureau of Economic Analysis") +
  recession_shade("1992-01-01") +
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

ggsave("charts/COVID charts/M_goods_share.png", p, device = "png", width = 14.2, height = 8)

p <- real_spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 1992) %>% 
  select(date, LineDescription, DataValue) %>% 
  spread(LineDescription, DataValue) %>% 
  mutate(ratio = Goods/Services)

p <- p %>% 
  ggplot(aes(date, ratio)) +
  geom_line(size = 1, color = "#1f78b4") +
  scale_y_continuous(labels = percent, limits = c(.4, .72)) +
  labs(x = NULL, y = NULL,
       title = "Real goods spending as a share of services spending",
       subtitle = "Seasonally adjusted. Shaded areas denote recessions.",
       caption = "Source: Bureau of Economic Analysis") +
  recession_shade("1992-01-01") +
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

ggsave("charts/COVID charts/M_real_goods_share.png", p, device = "png", width = 14.2, height = 8)


p <- real_spending %>% 
  filter(LineNumber %in% c("3", "1"),
         year(date) >= 1992) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ratio = `3`/`1`)

p <- p %>% 
  filter(year(date) >= 2005) %>% 
  ggplot(aes(date, ratio)) +
  geom_line(size = 1, color = "#1f78b4") +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL,
       title = "Real durable goods spending as a share of total spending",
       subtitle = "Seasonally adjusted. Shaded areas denote recessions.",
       caption = "Source: Bureau of Economic Analysis") +
  recession_shade("2005-01-01") +
  scale_y_continuous(labels = percent) +
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

ggsave("charts/COVID charts/M_real_durables_share.png", p, device = "png", width = 14.2, height = 8)




p <- real_spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2017) %>% 
  group_by(LineDescription) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/first(DataValue) - 1)


p <- p %>% 
  ggplot(aes(date, change, colour = LineDescription)) +
  geom_line(size = 1, show.legend =  F) +
  geom_hline(yintercept = 0) +
  stat_smooth(data = subset(p, date < ymd("2020-02-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) 

p <- p +
  scale_color_manual(values = c(Goods = "#1f78b4", Services = "#33a02c")) +
  scale_x_date(limits = c(ymd("2017-01-01"), ymd("2022-09-01"))) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real consumer spending on goods and services vs prepandemic trends",
       subtitle = "Change since Jan. 2017, seasonally adjusted",
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

ggsave("charts/COVID charts/M_real_goods_v_svces_vs_trend.png", p, device = "png", width = 14.2, height = 8)



income %>% 
  filter(LineNumber == "35") %>% 
  filter(DataValue <= DataValue[date == max(date)]) %>% 
  arrange(desc(date))


income %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date == ymd("2013-01-01"))




pce %>% 
  filter(LineNumber %in% c("1", "6")) %>%
  mutate(series = case_when(LineNumber == "1" ~ "Headline",
                            LineNumber == "6" ~ "Core"),
         LineNumber = factor(LineNumber, levels = c("1", "6"), labels = c("Headline", "Core"))) %>% 
  arrange(date) %>% 
  group_by(LineDescription) %>% 
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  filter(series == "Headline",
         change >= .063) %>% 
  arrange(desc(date))

pce %>% 
  filter(LineNumber == "6") %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  select(date, LineDescription, DataValue, chg) %>% 
  filter(year(date) > 1984) %>% 
  arrange(desc(chg))

p <- income %>% 
  filter(LineNumber == "29",
         year(date) >= 2015) %>% 
  mutate(DataValue = DataValue/1000000)

p <- p %>% 
  ggplot(aes(date, DataValue)) +
  geom_line(size = 1, color = "#1f78b4") +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
            method = "lm", 
            fullrange = TRUE,
            linetype = "dashed",
            se = FALSE,
            color = "black") +
  labs(x = NULL, y = NULL,
       title = "Nominal consumer spending vs prepandemic trend",
       subtitle = "Trillions of dollars, seasonally adjusted annual rate",
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

ggsave("charts/COVID charts/M_spend_vs_trend.png", p, device = "png", width = 14.2, height = 8)


p <- real_spending %>% 
  filter(LineNumber == "1",
         year(date) >= 2015) %>% 
  mutate(DataValue = DataValue/1000000)

p <- p %>% 
  ggplot(aes(date, DataValue)) +
  geom_line(size = 1, color = "#1f78b4") +
  stat_smooth(data = subset(p, date < ymd("2020-01-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE,
              color = "black") +
  labs(x = NULL, y = NULL,
       title = "Real consumer spending vs prepandemic trend",
       subtitle = "Trillions of dollars, seasonally adjusted annual rate",
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

ggsave("charts/COVID charts/M_real_spend_vs_trend.png", p, device = "png", width = 14.2, height = 8)


p <- spending %>% 
  filter(LineNumber == "1") %>% 
  select(date, DataValue) %>% 
  mutate(series = "nominal")

p <- real_spending %>% 
  filter(LineNumber == "1") %>% 
  select(date, DataValue) %>% 
  mutate(series = "real") %>% 
  bind_rows(p)

p <- p %>% 
  mutate(series = factor(series, levels = c("nominal", "real"),
                         labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  group_by(series) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, color = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(Unadjusted = "#a6cee3", `Inflation-adjusted` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal consumer spending",
       subtitle = "Percent change from a year ago, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
  xlim(ymd("2019-01-01"), ymd("2022-10-01")) +
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

ggsave("charts/COVID charts/M_real_vs_nominal_spend.png", p, device = "png", width = 14.2, height = 8)

p <- income %>% 
  filter(LineNumber %in% c("27", "37")) %>% 
  select(date, LineNumber, DataValue) 

p <- p %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  ungroup() %>% 
  mutate(series = factor(LineNumber, levels = c("27", "37"),
                         labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, color = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_color_manual(values = c(Unadjusted = "#a6cee3", `Inflation-adjusted` = "#1f78b4")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal after-tax income",
       subtitle = "Percent change from a year ago, seasonally adjusted",
       caption = "Source: Bureau of Economic Analysis") +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
  xlim(ymd("2019-01-01"), ymd("2022-12-01")) +
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

ggsave("charts/COVID charts/M_real_vs_nominal_dpi.png", p, device = "png", width = 14.2, height = 8)


p %>% 
  group_by(LineNumber) %>% 
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  arrange(desc(date))

ws_industry <- bea_all("T20700B", freq = "M")

# Personal income
p <- income %>% 
  filter(LineNumber == "1") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  filter(date >= ymd("2021-07-01")) %>% 
  ggplot(aes(date, change)) +
  geom_col(fill = "#a6cee3") +
  geom_line(aes(y = roll), colour = "#1f78b4", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Personal income, not adjusted for inflation",
       subtitle = "Bars show change from prior month, seasonally adjusted. Line shows three-month trailing average.",
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

ggsave("charts/COVID charts/M_nominal_income.png", p, device = "png", width = 14.2, height = 8)



# Real income less transfers
p <- income %>% 
  filter(LineNumber == "36") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  filter(year(date) >= 2021) %>% 
  ggplot(aes(date, change)) +
  geom_col(fill = "#a6cee3") +
  geom_line(aes(y = roll), colour = "#1f78b4", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
     title = "Inflation-adjusted personal income, excluding government transfers",
     subtitle = "Bars show change from prior month, seasonally adjusted. Line shows three-month trailing average.",
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

ggsave("charts/COVID charts/M_income_less_transfers.png", p, device = "png", width = 14.2, height = 8)



income %>% 
  filter(LineNumber %in% c("1", "16")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  left_join(pce %>% 
              filter(LineNumber == "1") %>% 
              select(date, pce = DataValue),
            by = "date") %>% 
  mutate(less_transfer = `1` - `16`,
         chg = less_transfer - lag(less_transfer, 1, na.pad = T),
         real = less_transfer/pce,
         real_chg = real - lag(real, 1,  na.pad = T)) %>% 
  arrange(desc(date))


p <- real_spending %>% 
  filter(LineNumber == "1") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  filter(year(date) >= 2021) %>% 
  ggplot(aes(date, change)) +
  geom_col(fill = "#a6cee3") +
  geom_line(aes(y = roll), colour = "#1f78b4", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Inflation-adjusted consumer spending",
       subtitle = "Bars show change from prior month, seasonally adjusted. Line shows three-month trailing average.",
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

ggsave("charts/COVID charts/M_real_spend_monthly.png", p, device = "png", width = 14.2, height = 8)


p <- spending %>% 
  filter(LineNumber == "1") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  filter(year(date) >= 2021) %>% 
  ggplot(aes(date, change)) +
  geom_col(fill = "#a6cee3") +
  geom_line(aes(y = roll), colour = "#1f78b4", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Consumer spending, not adjusted for inflation",
       subtitle = "Bars show change from prior month, seasonally adjusted. Line shows three-month trailing average.",
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

ggsave("charts/COVID charts/M_nominal_spend_monthly.png", p, device = "png", width = 14.2, height = 8)


p <- spending %>% 
  filter(LineNumber == "2") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  filter(year(date) >= 2021) %>% 
  ggplot(aes(date, change)) +
  geom_col(fill = "#a6cee3") +
  geom_line(aes(y = roll), colour = "#1f78b4", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Consumer spending on goods, not adjusted for inflation",
       subtitle = "Bars show change from prior month, seasonally adjusted. Line shows three-month trailing average.",
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

ggsave("charts/COVID charts/M_nominal_spend_monthly_goods.png", p, device = "png", width = 14.2, height = 8)


p <- spending %>% 
  filter(LineNumber == "5") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  filter(year(date) >= 2021) %>% 
  ggplot(aes(date, change)) +
  geom_col(fill = "#a6cee3") +
  geom_line(aes(y = roll), colour = "#1f78b4", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Consumer spending on services, not adjusted for inflation",
       subtitle = "Bars show change from prior month, seasonally adjusted. Line shows three-month trailing average.",
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

ggsave("charts/COVID charts/M_nominal_spend_monthly_svces.png", p, device = "png", width = 14.2, height = 8)





p <- spending %>% 
  filter(LineNumber == "1") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T),
         stim = case_when(date %in% c(ymd("2020-04-01"), ymd("2020-05-01"), ymd("2021-01-01"), ymd("2021-03-01")) ~ "stim",
                          TRUE ~ "no_stim")) %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, change, fill = stim)) +
  geom_col() +
  scale_fill_manual(values = c(stim = "#1f78b4", no_stim = "#a6cee3")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Consumer spending, not adjusted for inflation",
       subtitle = "Change from prior month, seasonally adjusted. Darker bars are months with relief payments.",
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

ggsave("charts/COVID charts/stim_spend.png", p, device = "png", width = 14.2, height = 8)



spending %>% 
  filter(LineNumber == "3") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         roll = rollmean(change, 3, align = "right", na.pad = T)) %>% 
  select(date, LineDescription, DataValue, change, roll) %>% 
  arrange(desc(date))

real_spend_detail %>% 
  filter(LineNumber == "113") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  select(date, LineDescription, DataValue, change) %>% 
  arrange(desc(date))



real_spending %>% 
  filter(LineNumber == "1") %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  select(date, LineDescription, DataValue, change) %>% 
  arrange(desc(date))


income %>% 
  filter(LineNumber %in% c("37")) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  select(date, LineDescription, DataValue, change) %>% 
  arrange(desc(date))


real_inventories <- bea_all("U001B", freq = "M", dataset = "NIUnderlyingDetail")

p <- real_inventories %>% 
  filter(LineNumber %in% c("2", "28", "51"),
         year(date) >= 2015) %>% 
  mutate(LineNumber = factor(LineNumber, 
                             levels = c("51", "28", "2"),
                             labels = c("Retail", "Wholesale", "Manufacturing"))) %>% 
  ggplot(aes(date, DataValue/1000000, fill = LineNumber)) +
  geom_col()

p <- p +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(Retail = "#fb9a99", Wholesale = "#33a02c", Manufacturing = "#1f78b4")) +
  labs(x = NULL, y = NULL,
       title = "Real inventories by major industry",
       subtitle = "Trillions of 2012 dollars",
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

ggsave("charts/COVID charts/M_inventories.png", p, device = "png", width = 14.2, height = 8)


p <- real_inventories %>% 
  filter(LineNumber %in% c("51", "52"),
         year(date) >= 2015) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(ex_auto = `51` - `52`) %>% 
  select(date, auto = `52`, ex_auto) %>% 
  gather(series, value, -date) %>% 
  mutate(series = factor(series, levels = c("auto", "ex_auto"),
                         labels = c("Auto & parts dealers", "All other retailers"))) %>% 
  ggplot(aes(date, value/1000000, fill = series)) +
  geom_col()

p <- p +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c('Auto & parts dealers' = "#a6cee3", `All other retailers` = "#1f78b4")) +
  labs(x = NULL, y = NULL,
       title = "Real retail inventories, autos and non-autos",
       subtitle = "Trillions of 2012 dollars",
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

ggsave("charts/COVID charts/M_retail_inventories.png", p, device = "png", width = 14.2, height = 8)




vintage_gdp <- readxl::read_xlsx("C:/Users/208546/Downloads/routputMvQd.xlsx")

vintage_gdp <- vintage_gdp %>% 
  gather(vintage, value, -DATE) %>% 
  group_by(vintage) %>% 
  mutate(value = as.numeric(value),
         chg = (value/lag(value, 1, na.pad = T))^4 -1,
         neg = case_when(chg < 0 ~ 1,
                         TRUE ~ 0),
         neg_streak = neg,
         neg_streak = case_when(neg == 1 ~ lag(neg_streak, 1, na.pad = T) + 1,
                                TRUE ~ 0))

vintage_gdp <- vintage_gdp %>% 
  mutate(date2 = paste(substr(DATE, 1, 4),
                        as.numeric(substr(DATE, 7,7)) * 3 - 2,
                        "01", sep = "-"),
         date2 = ymd(date2))

vintage_gdp %>% 
  ungroup() %>% 
  mutate(rec = case_when(neg_streak >= 2 ~ "recession",
                         TRUE ~ "not")) %>% 
  count(date2, rec) %>% 
  group_by(date2) %>% 
  mutate(count = n()) %>% 
  filter(count > 1,
         year(date2) >= 1960) %>% head(20)
  arrange(desc(date2)) %>% 
  head(20)
  
vintage_gdp %>% 
  filter(!is.na(chg)) %>% 
  filter(DATE == "1974:Q2") %>% 
  head(20)

vintage_gdp %>% 
  filter(neg_streak >= 2,
         year(date2) %in% 2000:2001)

vintage_gdp %>% 
  filter(vintage == "ROUTPUT01M11",
         year(date2) == 2001)

vintage_gdp %>% 
  filter(!is.na(chg)) %>% 
  ungroup() %>% 
  group_by(date2) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(est = case_when(rownum == 1 ~ "first_est",
                         rownum == 2 ~ "second_est",
                         rownum == 3 ~ "third_est",
                         rownum == max(rownum) ~ "latest_est",
                         TRUE ~ "other_est")) %>% 
  filter(est %in% c("first_est", "second_est", "third_est", "latest_est")) %>% 
  ungroup() %>% 
  select(DATE, est, chg) %>% 
  spread(est, chg) %>% 
  filter((first_est < 0 | second_est < 0 | third_est < 0),
         latest_est > 0) %>% 
  select(DATE, first_est, second_est, third_est, latest_est) %>% 
  arrange(first_est)


vintage_gdp %>% 
  filter(!is.na(chg)) %>% 
  ungroup() %>% 
  group_by(date2) %>% 
  mutate(rownum = row_number()) %>% 
  mutate(est = case_when(rownum == 1 ~ "first_est",
                         rownum == 2 ~ "second_est",
                         rownum == 3 ~ "third_est",
                         rownum == max(rownum) ~ "latest_est",
                         TRUE ~ "other_est")) %>% 
  filter(est == "first_est",
         chg < 0, neg_streak >= 2) %>% 
  View()
  
  mutate(first_est= case_when(rownum == min(rownum) ~ 1,
                              TRUE ~ 0),
         final_est = case_when(rownum == max(rownum) ~ 1,
                               TRUE ~ 0)) %>% 
  filter(first_est == 1 |)
  
  
  mutate(init_neg = case_when(rownum == min(rownum) & neg == 1 ~ 1,
                              TRUE ~0),
         latest_neg = case_when(rownum == max(rownum) & neg == 1 ~ 1,
                                TRUE ~ 0)) %>% 
  filter(init_neg == 1,
         latest_neg != 1) %>% 
  ungroup() %>% 
  count(DATE) %>% 
  filter(n > 1)

nber_data <- fred_mult(series_id = c("W875RX1", "PCEC96", "PAYEMS", "INDPRO", "CMRMTSPL", "CE16OV"))

nber_data <- nber_data %>% 
  mutate(series_name = factor(series_id, levels = c("W875RX1", "PCEC96", "PAYEMS", "INDPRO", "CMRMTSPL", "CE16OV"),
                              labels = c("income_ex_transfers", "pce", "payrolls", "indust_prod", "sales", "employment")))

nber_data %>% 
  group_by(series_name) %>% 
  filter(date >= ymd("1947-01-01"),
         year(date) < 1955) %>% 
  mutate(change = value/first(value) - 1) %>% 
  ggplot(aes(date, change, color = series_name)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Change in NBER recession indicators since Feb. 2020")


nber_data %>% 
  filter(series_name %in% c("income_ex_transfers", "pce", "payrolls", "indust_prod")) %>% 
  group_by(series_name) %>% 
  filter(date >= ymd("2020-02-01")) %>% 
  mutate(change = 100*(value/first(value) - 1)) %>% 
  select(-value, -series_id) %>% 
  spread(series_name, change) %>% 
  write_csv("recession_inds.csv")


nber_data %>% 
  group_by(series_name) %>% 
  filter(date >= ymd("2020-04-01")) %>% 
  mutate(change = value/first(value) - 1) %>% 
  ggplot(aes(date, change, color = series_name)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  labs(title = "Change in NBER recession indicators since April 2020")


gdp_gdi <- fred_mult(series_id = c("GDPC1", "A261RX1Q020SBEA"))

gdp_gdi %>% 
  mutate(series_id = factor(series_id, levels = c("GDPC1", "A261RX1Q020SBEA"),
                            labels = c("GDP", "GDI"))) %>% 
  filter(date >= ymd("2012-01-01")) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value) - 1) %>% 
  ggplot(aes(date, value, colour = series_id)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0)


gdp_gdi %>% 
  mutate(series_id = factor(series_id, levels = c("GDPC1", "A261RX1Q020SBEA"),
                            labels = c("GDP", "GDI")),
         value = value/1000) %>% 
  filter(date >= ymd("2012-01-01")) %>% 
  spread(series_id, value) %>% 
  write_csv("gdp_gdi.csv")

indeed_data <- read_csv("aggregate_job_postings_US.csv")



p <- nber_data %>% 
  filter(series_name == "payrolls") %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1) %>% 
  filter(year(date) %in% 1945:1950) %>% 
  ggplot(aes(date, change)) +
  geom_line(colour = "#1f78b4") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0) +
  recession_shade("1945-01-01") +
  xlim(ymd("1945-01-01"), ymd("1949-12-01"))
    

price_measures <- fred_mult(series_id = c("PCEPI", "CPIAUCSL"))

price_measures %>% 
  filter(date >= ymd("2020-02-01")) %>% 
  mutate(series_id = factor(series_id, levels = c("PCEPI", "CPIAUCSL"), labels = c("PCE", "CPI"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/first(value)) %>% 
  ggplot(aes(date, change, colour = series_id)) +
  geom_line()

p <- price_measures %>% 
  mutate(series_id = factor(series_id, levels = c("PCEPI", "CPIAUCSL"), labels = c("PCE", "CPI"))) %>% 
  group_by(series_id) %>% 
  mutate(change = value/lag(value, 12, na.pad = T) - 1) %>% 
  filter(date >= ymd("2020-02-01")) %>% 
  select(-value) %>% 
  spread(series_id, change) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = CPI), colour = "#1f78b4", size = 1) +
  geom_line(aes(y = PCE), colour = "#33a02c", size = 1) +
  geom_ribbon(
    aes(ymin = PCE, ymax = CPI),
    fill = "#a6cee3", alpha = 0.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Two measures of inflation",
       subtitle = "CPI vs PCE inflation, change from a year earlier",
       caption = "Sources: Bureau of Labor Statistics (CPI), Bureau of Economic Analysis (PCE)") +
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

p <- p +
  annotate("text", x = ymd("2021-10-01"), y = .07, label = "bold(CPI)", parse = TRUE, colour = "#1f78b4", size = 6) +
  annotate("text", x = ymd("2021-12-01"), y = .052, label = "bold(PCE)", parse = TRUE, colour = "#33a02c", size = 6) 

ggsave("charts/COVID charts/M_cpi_vs_pce.png", p, device = "png", width = 14.2, height = 8)




p <- price_measures %>% 
  mutate(series_id = factor(series_id, levels = c("PCEPI", "CPIAUCSL"), labels = c("PCE", "CPI"))) %>% 
  group_by(series_id) %>% 
  mutate(change = (value/lag(value, 3, na.pad = T))^4 - 1) %>% 
  filter(date >= ymd("2020-02-01")) %>% 
  select(-value) %>% 
  spread(series_id, change) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = CPI), colour = "#1f78b4", size = 1) +
  geom_line(aes(y = PCE), colour = "#33a02c", size = 1) +
  geom_ribbon(
    aes(ymin = PCE, ymax = CPI),
    fill = "#a6cee3", alpha = 0.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent)

p <- p +
  labs(x = NULL, y = NULL,
       title = "Two measures of inflation (three-month change)",
       subtitle = "CPI vs PCE inflation, three-month annualized change",
       caption = "Sources: Bureau of Labor Statistics (CPI), Bureau of Economic Analysis (PCE)") +
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

p2 <- p +
  annotate("text", x = ymd("2021-10-01"), y = .090, label = "bold(CPI)", parse = TRUE, colour = "#1f78b4", size = 6) +
  annotate("text", x = ymd("2021-12-01"), y = .055, label = "bold(PCE)", parse = TRUE, colour = "#33a02c", size = 6) 

ggsave("charts/COVID charts/M_cpi_vs_pce_3mo.png", p2, device = "png", width = 14.2, height = 8)




# Saving as share of total income
p <- income %>% 
  filter(LineNumber == "1") %>% 
  select(date, DataValue) %>% 
  mutate(series = "Income")

p <- spending %>% 
  filter(LineNumber == "1") %>% 
  select(date, DataValue) %>% 
  mutate(series = "Spending") %>% 
  bind_rows(p)

p <- p %>% 
  spread(series, DataValue) %>% 
  mutate(saving = Income - Spending,
         saving_rate = saving/Income)

p <- p %>% 
  filter(year(date) >= 2014) %>% 
  mutate(pre_covid = case_when(date <= ymd("2020-01-01") ~ saving_rate),
         pre_avg = mean(pre_covid, na.rm = T)) %>% 
  ggplot(aes(date, saving_rate)) +
  geom_line(size = 1, color = "#1f78b4") +
  geom_line(aes(y = pre_avg), linetype = "dashed", colour = "black", size = 1) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Alternate saving rate based on total income",
       subtitle = "Total income, less spending, as a share of income. Seasonally adjusted. Dashed line is pre-Covid average.",
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

ggsave("charts/COVID charts/saving_rate_recalc.png", p, device = "png", width = 14.2, height = 8)



spending %>% 
  filter(LineNumber == "5") %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1,
         chg = 100*chg) %>%
  select(date, LineDescription, DataValue, chg) %>% 
  arrange(desc(date))


price_detail %>% 
  filter(year(date)  >= 2021,
         as.numeric(LineNumber) %in% 3:69) %>% 
  group_by(LineNumber) %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  filter(date == max(date)) %>% 
  arrange(chg) %>% 
  select(date, LineDescription, DataValue, chg) %>% 
  head(20)


income %>% 
  filter(LineNumber == "3") %>% 
  select(date, LineDescription, DataValue) %>% 
  mutate(chg = 100*(DataValue/lag(DataValue, 1, na.pad = T) - 1),
         chg_12 = 100*(DataValue/lag(DataValue, 12, na.pad = T) - 1)) %>% 
  arrange(desc(date))



spending %>% 
  filter(LineNumber %in% c("2", "5"),
         year(date) >= 2020) %>% 
  group_by(LineDescription) %>% 
  arrange(date) %>% 
  mutate(change = 100*(DataValue/lag(DataValue, 1, na.pad = T) - 1)) %>% 
  filter(LineDescription == "Services") %>% 
  arrange(desc(date))




# Real excess saving
p <- income %>% 
  filter(LineNumber == "34",
         date >= ymd("2019-01-01")) %>% 
  mutate(cum_savings = cumsum(DataValue/12000000)) %>% 
  select(date, DataValue, cum_savings)

p <- p %>% 
  left_join(pce %>% 
              filter(LineNumber == "1") %>% 
              select(date, pce = DataValue),
            by = "date") %>% 
  mutate(real = 100* cum_savings/pce)
  
r <- p %>% 
  filter(date < ymd("2020-03-01")) %>% 
  lm(real ~ date, data = .)

p <- p %>% 
  mutate(trend = map_dbl(date, ~predict(r, newdata = tibble(date = .x))))

p %>% 
  mutate(excess = real - trend) %>% 
  arrange(desc(date)) %>% 
  head(20)

p <- p %>% 
  ggplot(aes(date, real)) +
  geom_line(size = 1, color = "#1f78b4") +
  geom_line(aes(y = trend), linetype = "dashed", size = 1) +
  geom_ribbon(
    # data = subset(p, date >= ymd("2020-01-01")),
    aes(ymin = trend, ymax = real),
    fill = "#a6cee3", alpha = 0.5)

p <- p +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, 
       title = "Cumulative personal savings in the pandemic",
       subtitle = "Trillions of dollars, seasonally adjusted. Not adjusted for inflation.",
       caption = "Note: Monthly data, not annualized. | Source: Bureau of Economic Analysis") +
  annotate("text", x = ymd("2021-08-01"), y = 4.9,
           label = "Pandemic savings", colour = "#1f78b4", size = 5.5) +
  geom_curve(aes(x = ymd("2020-05-01"), y = 1.5,
                 xend = ymd("2020-04-01"), yend = 1.75),
             curvature = -.2, arrow = arrow(length = unit(.02, "npc"))) +
  annotate("text", x = ymd("2020-08-01"), y = 1.5,
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
  "charts/COVID charts/M_excess_savings.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


pce %>% 
  filter(LineNumber %in% c("1")) %>% 
  mutate(change = DataValue/lag(DataValue, 12, na.pad = T) - 1) %>% 
  select(date, LineDescription, DataValue, change) %>% 
  arrange(desc(date))


spend_detail %>% 
  filter(LineNumber %in% c("1", "368")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(share = `368`/`1`) %>% arrange(desc(date))

# Core services ex housing
spend_detail %>% 
  filter(LineNumber %in% c("374", "151")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(share = `151`/`374`) %>% arrange(desc(date))

pce_detail %>% 
  filter(LineNumber %in% c("374", "151")) %>% 
  group_by(LineNumber) %>% 
  mutate(chg = DataValue/lag(DataValue, 1, na.pad = T) - 1) %>% 
  arrange(desc(date)) %>% 
  select(date, LineDescription, chg)


ex_item <- function(ex, baseline = "1", startdate, enddate = "latest") {
  
  if (enddate == "latest") {enddate = max(spend_detail$date)}
  
  share <- spend_detail %>% 
    filter(LineNumber %in% c(ex, baseline)) %>% 
    mutate(LineNumber = factor(LineNumber, levels = c(ex, baseline),
                               labels = c("item", "total"))) %>% 
    select(date, LineNumber, DataValue) %>% 
    spread(LineNumber, DataValue) %>% 
    mutate(share = item/total) %>% 
    arrange(desc(date))
  
  share <- share$share[share$date == ymd(startdate)]

  prices <- pce_detail %>%
    filter(LineNumber %in% c(ex, baseline),
           date >= ymd(startdate),
           date <= ymd(enddate)) %>%
    mutate(LineNumber = factor(LineNumber, levels = c(ex, baseline),
                               labels = c("item", "total"))) %>%
    group_by(LineNumber) %>%
    mutate(chg = DataValue/first(DataValue) - 1) %>%
    arrange(desc(date)) 

  output <- (prices$chg[prices$LineNumber == "total" & prices$date == max(prices$date)] -
               share * prices$chg[prices$LineNumber == "item" & prices$date == max(prices$date)])/
    (1 - share)

  output

  
}

ex_item(ex = "151", baseline = "374", startdate = "2022-02-01", enddate = "2023-02-01")



ex_item <- function(ex, baseline = "1", startdate, lag = 1) {
  
  share <- spend_detail %>% 
    filter(LineNumber %in% c(ex, baseline)) %>% 
    mutate(LineNumber = factor(LineNumber, levels = c(ex, baseline),
                               labels = c("item", "total"))) %>% 
    select(date, LineNumber, DataValue) %>% 
    spread(LineNumber, DataValue) %>% 
    mutate(share = item/total) %>% 
    select(date, share)
  
  prices <- pce_detail %>%
    filter(LineNumber %in% c(ex, baseline),
           date >= ymd(startdate)) %>%
    mutate(LineNumber = factor(LineNumber, levels = c(ex, baseline),
                               labels = c("item", "total"))) %>%
    group_by(LineNumber) %>%
    mutate(chg = DataValue/lag(DataValue, lag, na.pad = T) - 1) %>%
    select(date, LineNumber, chg) %>% 
    spread(LineNumber, chg)

  output <- left_join(prices, share, by = "date") %>%
    mutate(ex_item = (total - item * lag(share, lag))/(1 - lag(share, lag)))

  output

}

ex_item(ex = "151", baseline = "374", startdate = "2022-02-01", lag = 1) %>% 
  arrange(desc(date))



cpi_item %>% 
  filter(display_level == 2) %>% 
  left_join(cpi_wts, by = "item_code") %>% 
  filter(date == max(date, na.rm = T)) %>% 
  summarize(sum(wt))

cpi_wts
