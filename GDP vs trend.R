# Real vs nominal trend
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

# p %>% 
#   select(date, real_nominal, series, change) %>% 
#   pivot_wider(names_from = c(series, real_nominal),
#               values_from = change) %>% 
#   filter(!is.na(value_Unadjusted)) %>% 
#   write_csv("real_vs_nominal.csv")

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
  xlim(ymd("2017-01-01"), ymd("2024-03-01")) +
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


# Real only
p <- gdp_tables %>% 
  filter(TableName %in% c("T10106"),
         LineNumber == "1",
         date >= ymd("2017-01-01")) %>% 
  mutate(series = "value",
         DataValue = DataValue/1000) %>%  
  select(date, series, value = DataValue) %>% 
  bind_rows(cbo_project_2020_01 %>% filter(real_nominal == "Inflation-adjusted")) %>% 
  group_by(series)

p <- p %>% 
  mutate(value = value/1000) %>% 
  select(-real_nominal) %>% 
  spread(series, value) %>% 
  filter(!is.na(value)) %>% 
  mutate(label = case_when(date == max(date) ~ percent((value/lag(value, 1))^4 - 1, accuracy = .1)),
         change = case_when(date == max(date) ~ value)) %>% 
  ggplot(aes(date, value)) +
  geom_line(size = 1, show.legend = F, colour = "#1f78b4") +
  geom_line(aes(y = trend), linetype = "dashed", size = 1, colour = "black") +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) 

p <- p +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") +
  scale_color_manual(values = c(`Inflation-adjusted` = "#1f78b4",
                                Unadjusted = "#33a02c")) +
  xlim(ymd("2017-01-01"), ymd("2023-10-01")) +
  labs(x = NULL, y = NULL,
       title = "Real GDP vs trend",
       subtitle = "U.S. gross domestic product, in trillions of 2012 dollars. Trend is based on Jan. 2020 CBO forecast.",
       caption = "Note: Seasonally adjusted annual rate. | Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/real_vs_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)



# Nominal only
p <- gdp_tables %>% 
  filter(TableName %in% c("T10105"),
         LineNumber == "1",
         date >= ymd("2017-01-01")) %>% 
  mutate(series = "value",
         DataValue = DataValue/1000) %>%  
  select(date, series, value = DataValue) %>% 
  bind_rows(cbo_project_2020_01 %>% filter(real_nominal == "Unadjusted")) %>% 
  group_by(series)

p <- p %>% 
  mutate(value = value/1000) %>% 
  select(-real_nominal) %>% 
  spread(series, value) %>% 
  filter(!is.na(value)) %>% 
  mutate(label = case_when(date == max(date) ~ percent(value/lag(value, 1) - 1, accuracy = .1)),
         change = case_when(date == max(date) ~ value)) %>% 
  ggplot(aes(date, value)) +
  geom_line(size = 1, show.legend = F, colour = "#1f78b4") +
  geom_line(aes(y = trend), linetype = "dashed", size = 1, colour = "black") +
  geom_point(aes(y = change), colour = "#1f78b4", size = 3) 

p <- p +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5),
          colour = "#1f78b4") +
  scale_color_manual(values = c(`Inflation-adjusted` = "#1f78b4",
                                Unadjusted = "#33a02c")) +
  xlim(ymd("2017-01-01"), ymd("2023-10-01")) +
  labs(x = NULL, y = NULL,
       title = "Nominal GDP vs trend",
       subtitle = "U.S. gross domestic product, in trillions of dollars, not adjusted for inflation. \nTrend is based on Jan. 2020 CBO forecast.",
       caption = "Note: Seasonally adjusted annual rate. | Source: Bureau of Economic Analysis") +
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







price_adjust_2017 <- gdp_tables %>% 
  filter(TableName == "T10109",
         LineNumber == "1",
         date == ymd("2017-01-01")) %>% 
  select(DataValue) %>% 
  as.numeric()

p <- gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10109"),
         LineNumber == "1",
         date >= ymd("2017-01-01")) %>% 
  mutate(series = case_when(TableName == "T10105" ~ "nominal",
                            TableName == "T10109" ~ "prices")) %>% 
  select(date, series, DataValue) %>% 
  spread(series, DataValue) %>% 
  mutate(real = (nominal/prices)*price_adjust_2017) %>% 
  select(date, nominal, real) %>% 
  gather(series, value, -date) %>% 
  mutate(value = value/1000000)

r1 <- p %>% 
  filter(date < ymd("2020-01-01"),
         series == "nominal") %>% 
  lm(value ~ date, data = .)

r2 <- p %>% 
  filter(date < ymd("2020-01-01"),
         series == "real") %>% 
  lm(value ~ date, data = .)

p <- p %>% 
  mutate(trend = case_when(series == "nominal" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           series == "real" ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x)))),
         series = factor(series, levels = c("nominal", "real"),
                         labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = series), linetype = "dashed", size = 1) +
  scale_color_manual(values = c(Unadjusted = "#33a02c", `Inflation-adjusted` = "#1f78b4"))


p <- p +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .1),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2017-01-01"), ymd("2023-10-01")) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal GDP vs trend",
       subtitle = "Seasonally adjusted in trillions of dollars. Real values adjusted to Q1 2017 dollars.",
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
  "charts/COVID charts/nominal_vs_real.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


pce_adjust_2017 <- gdp_tables %>% 
  filter(TableName == "T10109",
         LineNumber == "2",
         date == ymd("2017-01-01")) %>% 
  select(DataValue) %>% 
  as.numeric()

p <- gdp_tables %>% 
  filter(TableName %in% c("T10105", "T10109"),
         LineNumber == "2",
         date >= ymd("2017-01-01")) %>% 
  mutate(series = case_when(TableName == "T10105" ~ "nominal",
                            TableName == "T10109" ~ "prices")) %>% 
  select(date, series, DataValue) %>% 
  spread(series, DataValue) %>% 
  mutate(real = (nominal/prices)*pce_adjust_2017) %>% 
  select(date, nominal, real) %>% 
  gather(series, value, -date) %>% 
  mutate(value = value/1000000)

r1 <- p %>% 
  filter(date < ymd("2020-01-01"),
         series == "nominal") %>% 
  lm(value ~ date, data = .)

r2 <- p %>% 
  filter(date < ymd("2020-01-01"),
         series == "real") %>% 
  lm(value ~ date, data = .)

p <- p %>% 
  mutate(trend = case_when(series == "nominal" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           series == "real" ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x)))),
         series = factor(series, levels = c("nominal", "real"),
                         labels = c("Unadjusted", "Inflation-adjusted"))) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(aes(y = trend, colour = series), linetype = "dashed", size = 1) +
  scale_color_manual(values = c(Unadjusted = "#33a02c", `Inflation-adjusted` = "#1f78b4"))

p <- p +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .1),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2017-01-01"), ymd("2023-10-01")) +
  labs(x = NULL, y = NULL,
       title = "Real and nominal consumer spending vs trend",
       subtitle = "Seasonally adjusted in trillions of dollars. Real values adjusted to Q1 2017 dollars.",
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



new_cbo <- read_csv("cbo_projections.csv")
new_cbo %>% 
  mutate(date = mdy(date)) %>% 
  pivot_longer(cols = -date,
               names_to = c("series", "vintage"),
               names_sep = "_",
               values_to = "value") %>% 
  filter(series == "gdp",
         year(date) >= 2010,
         year(date) <= 2025) %>% 
  ggplot(aes(date, value, colour = vintage)) +
  geom_line(size = 1)