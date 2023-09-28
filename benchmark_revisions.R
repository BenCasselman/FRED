# GDI vs GDP, old vs new

# UNREVISED_gdi_vs_gdp <- bea_all("T11706")
# UNREVISED_gdi_vs_gdp <- gdi_vs_gdp
# UNREVISED_gdp_tables <- gdp_tables
# UNREVISED_gdi <- gdi

# Embargoed data:
# revised_gdi_vs_gdp <- read_csv("gdp_gdi_revisions.csv")
# revised_gdi_vs_gdp <- revised_gdi_vs_gdp %>% 
#   gather(label, value, -date) %>% 
#   mutate(date = mdy(date),
#          rev = "Revised")

gdi_vs_gdp <- bea_all("T11706")
gdp_tables <- map_dfr(table_list, ~bea_all(.x))
gdi <- bea_all("T11000")

# New data
revised_gdi_vs_gdp <- bea_all("T11706") %>% 
  filter(LineNumber %in% c("1", "2"),
         year(date) >= 2018) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI"),
         value = DataValue/1000,
         rev = "Revised") %>% 
  select(date, label, rev, value) 

gdp_gdi_rev <- UNREVISED_gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2"),
         year(date) >= 2018) %>% 
  mutate(label = case_when(LineNumber == "1" ~ "GDP",
                           TRUE ~ "GDI"),
         value = DataValue/1000,
         rev = "Unrevised") %>% 
  select(date, label, rev, value) %>% 
  bind_rows(revised_gdi_vs_gdp)

gdp_gdi_rev %>% 
  select(date, label, value, rev) %>% 
  write_csv("gdp_rev_data_for_ella_final.csv")

# How gap has changed:
gdp_gdi_rev %>% 
  spread(label, value) %>% 
  mutate(gap = GDI/GDP - 1) %>% 
  select(date, rev, gap) %>% 
  spread(rev, gap) %>% 
  arrange(desc(date))

# Revisions
gdp_gdi_rev %>% 
  spread(rev, value) %>% 
  mutate(rev = Revised/Unrevised - 1) %>% 
  select(date, rev, label) %>% 
  spread(label, rev) %>% 
  arrange(desc(date))

gdp_gdi_rev %>% 
  group_by(label, rev) %>% 
  mutate(chg = (value/lag(value, 1, na.pad = T))^4 - 1) %>% 
  select(date, label, rev, chg) %>% 
  pivot_wider(names_from = c("label", "rev"),
              values_from = "chg") %>% 
  arrange(desc(date)) %>% 
  select(date, GDP_Unrevised, GDP_Revised, GDI_Unrevised, GDI_Revised)

# p <- gdp_gdi_rev %>% 
#   filter(year(date) >= 2018) %>% 
#   ggplot(aes(date, value/1000, colour = label)) +
#   geom_line(show.legend = F, size = 1, aes(linetype = rev)) +
#   scale_colour_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
#   scale_linetype_manual(values = c(Revised = 1, Unrevised = 2))
# 
# p <- p +
#   labs(x = NULL, y = NULL,
#        title = "Real gross domestic products vs gross domestic income, revised and unrevised",
#        subtitle = "In trillions of chained 2012 dollars, annualized. Dashed lines are originally published figures.",
#        caption = "Source: Bureau of Economic Analysis") +
#   scale_color_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
#   geom_dl(aes(label = label),
#           method = list(dl.trans(x = x + .2),
#                         "last.points", cex = 1.5)) +
#   xlim(ymd("2018-01-01"), ymd("2022-06-01")) +
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
#     legend.position = "top",
#     legend.title = element_blank(),
#     legend.text = element_text(size = 16))
# 
# ggsave("~/FRED/charts/COVID charts/gdp_vs_gdi_rev.png", p, device = "png",  
#        width = 14.2,
#        height = 8)


p <- gdp_gdi_rev %>% 
  filter(year(date) >= 2018) %>% 
  group_by(label, rev) %>% 
  mutate(change = 100*value/first(value)) %>% 
  ggplot(aes(date, change, colour = label)) +
  geom_line(show.legend = F, size = 1, aes(linetype = rev)) +
  scale_colour_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
  scale_linetype_manual(values = c(Revised = 1, Unrevised = 2))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic products vs gross domestic income, revised and unrevised",
       subtitle = "Q1 2018 = 100. Dashed lines are originally published figures.",
       caption = "Source: Bureau of Economic Analysis") +
  scale_color_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2018-01-01"), ymd("2024-06-01")) +
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

ggsave("~/FRED/charts/COVID charts/bench_gdp_vs_gdi.png", p, device = "png",  
       width = 14.2,
       height = 8)




# Quarterly change in GDP
p <- gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("2005-01-01")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         label = "Revised") %>% 
  filter(year(date) >= 2020)

p <- UNREVISED_gdp_tables %>% 
  filter(TableName == "T10106",
         LineNumber == "1",
         date >= ymd("2005-01-01")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         label = "Unrevised") %>% 
  filter(year(date) >= 2020) %>% 
  bind_rows(p)

p <- p %>% 
  ggplot(aes(date, change, fill = label)) + 
  geom_col(show.legend = F, position = "dodge") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(Revised = "#1f78b4", Unrevised = "#a6cee3"))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic product, before and after benchmark revisions",
       subtitle = "Non-annualized, seasonally adjusted.",
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
  "charts/COVID charts/bench_quarterly_change.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


# Quarterly change in GDI
p <- gdi_vs_gdp %>% 
  filter(LineNumber == "2",
         date >= ymd("2005-01-01")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         label = "Revised") %>% 
  filter(year(date) >= 2020)

p <- UNREVISED_gdi_vs_gdp %>% 
  filter(LineNumber == "2",
         date >= ymd("2005-01-01")) %>% 
  arrange(date) %>% 
  mutate(change = DataValue/lag(DataValue, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up"),
         label = "Unrevised") %>% 
  filter(year(date) >= 2020) %>% 
  bind_rows(p)

p <- p %>% 
  ggplot(aes(date, change, fill = label)) + 
  geom_col(show.legend = F, position = "dodge") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(Revised = "#33a02c", Unrevised = "#b2df8a"))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic income, before and after benchmark revisions",
       subtitle = "Non-annualized, seasonally adjusted.",
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
  "charts/COVID charts/bench_quarterly_GDI.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)



# Real GDP vs trend
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
  mutate(series = "Revised")

p <- UNREVISED_gdp_tables %>% 
  filter(TableName %in% c("T10106"),
         LineNumber == "1",
         date >= ymd("2017-01-01")) %>% 
  mutate(series = "Unrevised",
         DataValue = DataValue/1000000) %>%  
  select(date, series, value = DataValue) %>% 
  bind_rows(p)

p <- p %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  geom_line(data = p %>% filter(series == "Revised"), aes(x = date, y = trend), linetype = "dashed", size = 1, colour = "black") +
  scale_colour_manual(values = c(Revised = "#1f78b4", Unrevised = "#a6cee3"))

p <- p +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2017-01-01"), ymd("2023-01-01")) +
  labs(x = NULL, y = NULL,
       title = "Real GDP vs trend, before and after benchmark revisions",
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
  "charts/COVID charts/bench_real_vs_trend.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


p <- gdi %>% 
  filter(LineNumber == "3",
         year(date) >= 2019) %>% 
  select(date, DataValue) %>% 
  mutate(series = "Revised")

p <- UNREVISED_gdi %>% 
  filter(LineNumber == "3",
         year(date) >= 2019) %>% 
  select(date, DataValue) %>% 
  mutate(series = "Unrevised") %>% 
  bind_rows(p)

p <- p %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, DataValue/1000000, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_colour_manual(values = c(Revised = "#1f78b4", Unrevised = "#a6cee3"))

p <- p +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2019-01-01"), ymd("2022-09-01")) +
  labs(x = NULL, y = NULL,
       title = "Wage and salary income, before and after benchmark revisions",
       subtitle = "Trillions of dollars, not adjusted for inflation",
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
  "charts/COVID charts/bench_income.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)



p <- gdi %>% 
  filter(LineNumber == "15",
         year(date) >= 2019) %>% 
  select(date, DataValue) %>% 
  mutate(series = "Revised")

p <- UNREVISED_gdi %>% 
  filter(LineNumber == "15",
         year(date) >= 2019) %>% 
  select(date, DataValue) %>% 
  mutate(series = "Unrevised") %>% 
  bind_rows(p)

p <- p %>% 
  filter(year(date) >= 2019) %>% 
  ggplot(aes(date, DataValue/1000000, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_colour_manual(values = c(Revised = "#1f78b4", Unrevised = "#a6cee3"))

p <- p +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2019-01-01"), ymd("2022-09-01")) +
  labs(x = NULL, y = NULL,
       title = "Corporate profits, before and after benchmark revisions",
       subtitle = "Trillions of dollars, not adjusted for inflation",
       caption = "Note: Seasonally adjusted annual rate. Profits are before tax and after inventory and capital adjustments. | Source: Bureau of Economic Analysis") +
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
  "charts/COVID charts/bench_income.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


wage_recon <- bea_all("T71800")


# Excess savings, before and after
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
       title = "REVISED: Cumulative personal savings in the pandemic",
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


# Unrevised
p <- UNREVISED_gdp_tables %>% 
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
       title = "PRE-REVISION: Cumulative personal savings in the pandemic",
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
  "charts/COVID charts/bench_excess_savings.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)




saving_rev <- read_tsv(unz("PMSAVE_2_txt.zip", "PMSAVE_2_Vintages_Starting_2022_08_26.txt"),
                       skip = 1,
                       col_names = c("date", "pre_revision", "post_revision"))

p <- saving_rev %>% 
  gather(series, value, -date) %>% 
  filter(date >= ymd("2019-01-01")) %>% 
  group_by(series) %>% 
  mutate(cum_savings = cumsum(value/12000))

r1 <- p %>% 
  filter(date < ymd("2020-01-01"),
         series == "pre_revision") %>% 
  lm(cum_savings ~ date, data = .)

r2 <- p %>% 
  filter(date < ymd("2020-01-01"),
         series == "post_revision") %>% 
  lm(cum_savings ~ date, data = .)

p %>% 
  ggplot(aes(date, cum_savings, colour = series)) +
  geom_line() +
  stat_smooth(data = subset(p, date < ymd("2020-02-01")), 
              method = "lm", 
              fullrange = TRUE,
              linetype = "dashed",
              se = FALSE) 


p %>% 
  mutate(trend = case_when(series == "pre_revision" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           TRUE ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x)))),
         excess = cum_savings - trend) %>% 
  arrange(desc(date))

p1 <- p %>% 
  mutate(trend = case_when(series == "pre_revision" ~ map_dbl(date, ~predict(r1, newdata = tibble(date = .x))),
                           TRUE ~ map_dbl(date, ~predict(r2, newdata = tibble(date = .x))))) %>% 
  ggplot(aes(date, cum_savings, colour = series)) +
  geom_line(aes(y = trend), linetype = "dashed", size = 1,
            show.legend = F) +
  geom_ribbon(
    # data = subset(p, date >= ymd("2020-01-01")),
    aes(ymin = trend, ymax = cum_savings, fill = series),
    alpha = 0.5, show.legend = F) +
  scale_color_manual(values = c(pre_revision = "#a6cee3",
                                post_revision = "#1f78b4")) +
  scale_fill_manual(values = c(pre_revision = "#a6cee3",
                                post_revision = "#1f78b4"))

p1 <- p1 +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, 
       title = "Cumulative personal savings in the pandemic, before and after benchmark revisions",
       subtitle = "Trillions of dollars, seasonally adjusted. Darker shade is latest data; ligher shade is pre-revision.",
       caption = "Note: Monthly data, not annualized. Not adjusted for inflation. | Source: Bureau of Economic Analysis") +
  annotate("text", x = ymd("2021-08-01"), y = 4.9,
           label = "Pandemic savings", colour = "#1f78b4", size = 5.5) +
  geom_curve(aes(x = ymd("2020-05-01"), y = 1.4,
                 xend = ymd("2020-04-01"), yend = 1.6),
             curvature = -.2, arrow = arrow(length = unit(.02, "npc")), 
             colour = "#1f78b4") +
  annotate("text", x = ymd("2020-08-01"), y = 1.4,
           label = "Pre-pandemic trend", colour = "#1f78b4", size = 4.75) +
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
  "charts/COVID charts/bench_excess_savings.png",
  p1,
  device = "png",
  width = 14.2,
  height = 8
)

savrate_rev <- read_tsv(unz("PSAVERT_2_txt.zip", "PSAVERT_2_Vintages_Starting_2022_08_26.txt"),
                       skip = 1,
                       col_names = c("date", "pre_revision", "post_revision"))

p <- savrate_rev %>% 
  rename(`Post-revision` = "post_revision",
         `Pre-revision` = "pre_revision") %>% 
  gather(series, value, -date) %>% 
  filter(date >= ymd("2018-01-01"),
         date < max(date)) %>% 
  group_by(series) %>% 
  mutate(value = value/100,
         pre_covid = case_when(date <= ymd("2020-01-01") ~ value),
         pre_avg = mean(pre_covid, na.rm = T)) %>% 
  ggplot(aes(date, value, colour = series)) +
  geom_line(size = 1, show.legend = F) +
  scale_colour_manual(values = c(`Pre-revision` = "#a6cee3",
                                 `Post-revision` = "#1f78b4"))

p <- p + 
  geom_line(aes(y = pre_avg), linetype = "dashed", size = 1, show.legend = F) +
  geom_hline(yintercept = 0) +
  geom_dl(aes(label = series),
          method = list(dl.trans(x = x + .1), "last.qp", cex = 1.5))  +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL,
       title = "Personal saving rate, before and after benchmark revisions",
       subtitle = "Savings as a share of after-tax income, seasonally adjusted. Dashed line is pre-Covid average.",
       caption = "Note: August 2022 data not shown. | Source: Bureau of Economic Analysis") +
  xlim(ymd("2018-01-01"), ymd("2022-12-01")) +
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

ggsave("charts/COVID charts/bench_saving_rate.png", p, device = "png", width = 14.2, height = 8)


w <- alfred_mult(series_id = c("PI", "W055RC1", "PCE", "DSPI"), vintage_dates = c("2022-08-26", "2022-09-30"))

w %>% 
  filter(date >= ymd("2018-01-01")) %>% 
  mutate(vintage = factor(vintage, levels = c("2022-08-26", "2022-09-30"),
                          labels = c("pre_revision", "post_revision"))) %>% 
  ggplot(aes(date, value, colour = vintage)) +
  geom_line(size = 1) +
  facet_wrap(~series_id, scales = "free_y")

p <- w %>% 
  filter(date >= ymd("2018-01-01"),
         series_id %in% c("PCE", "DSPI")) %>% 
  mutate(vintage = factor(vintage, levels = c("2022-08-26", "2022-09-30"),
                          labels = c("Pre-revision", "Post-revision")),
         series_id = factor(series_id, levels = c("PCE", "DSPI"),
                            labels = c("Spending", "After-tax income"))) %>% 
  ggplot(aes(date, value/1000, alpha = vintage, colour = series_id)) +
  geom_line(size = 1, show.legend = F) +
  facet_wrap(~series_id, scales = "free_y") +
  scale_colour_manual(values = c(`After-tax income` = "#33a02c",
                                 `Spending` = "#1f78b4")) +
  scale_alpha_manual(values = c(`Pre-revision` = 0.5,
                                 `Post-revision` = 1))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Income and spending, before and after benchmark revisions",
       subtitle = "Trillions of dollars, seasonally adjusted annual rate. Darker shade is latest data; ligher shade is pre-revision. Y-axis scales differ.",
       caption = "Note: Not adjusted for inflation. | Source: Bureau of Economic Analysis") +
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

ggsave("charts/COVID charts/bench_income_spend.png", p, device = "png", width = 14.2, height = 8)




unrev_gdi <- fred_mult(c("GDI", "A261RX1Q020SBEA", "GDP", "GDPC1"))

p <- gdp_gdi_rev %>% 
  filter(rev == "Revised")

p <- unrev_gdi %>% 
  filter(series_id %in% c("A261RX1Q020SBEA", "GDPC1")) %>% 
  mutate(label = case_when(series_id == "A261RX1Q020SBEA" ~ "GDI",
                           series_id == "GDPC1" ~ "GDP"),
         rev = "Unrevised") %>% 
  select(date, label, rev, value) %>% 
  bind_rows(p)

p <- p %>% 
  filter(year(date) >= 2018) %>% 
  group_by(label, rev) %>% 
  mutate(change = 100*value/first(value)) %>% 
  ggplot(aes(date, change, colour = label)) +
  geom_line(show.legend = F, size = 1, aes(linetype = rev)) +
  scale_colour_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
  scale_linetype_manual(values = c(Revised = 1, Unrevised = 2))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic products vs gross domestic income, revised and unrevised",
       subtitle = "Q1 2018 = 100. Dashed lines are originally published figures.",
       caption = "Source: Bureau of Economic Analysis") +
  scale_color_manual(values = c(GDP = "#1f78b4", GDI = "#33a02c")) +
  geom_dl(aes(label = label),
          method = list(dl.trans(x = x + .2),
                        "last.points", cex = 1.5)) +
  xlim(ymd("2018-01-01"), ymd("2023-09-01")) +
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

ggsave("~/FRED/charts/COVID charts/bench_gdp_vs_gdi.png", p, device = "png",  
       width = 14.2,
       height = 8)



p <- unrev_gdi %>% 
  filter(series_id == "A261RX1Q020SBEA") %>% 
  mutate(label = "Unrevised")

p <- gdi_vs_gdp %>% 
  filter(LineNumber == "2") %>% 
  select(date, value = DataValue) %>% 
  mutate(label = "Revised") %>% 
  bind_rows(p) %>% 
  arrange(date) %>% 
  group_by(label) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up")) %>% 
  filter(year(date) >= 2021)

p <- p %>% 
  ggplot(aes(date, change, fill = label)) + 
  geom_col(show.legend = F, position = "dodge") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(Revised = "#33a02c", Unrevised = "#b2df8a"))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic income, before and after benchmark revisions",
       subtitle = "Non-annualized, seasonally adjusted.",
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
  "charts/COVID charts/bench_quarterly_GDI.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)






# Quarterly change in GDP
p <- unrev_gdi %>% 
  filter(series_id == "GDPC1") %>% 
  mutate(label = "Unrevised")

p <- gdi_vs_gdp %>% 
  filter(LineNumber == "1") %>% 
  select(date, value = DataValue) %>% 
  mutate(label = "Revised") %>% 
  bind_rows(p) %>% 
  arrange(date) %>% 
  group_by(label) %>% 
  mutate(change = value/lag(value, 1, na.pad = T) -1,
         updown = case_when(change < 0 ~ "down",
                            TRUE ~ "up")) %>% 
  filter(year(date) >= 2021)

p <- p %>% 
  ggplot(aes(date, change, fill = label)) + 
  geom_col(show.legend = F, position = "dodge") +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0)  +
  scale_fill_manual(values = c(Revised = "#1f78b4", Unrevised = "#a6cee3"))

p <- p +
  labs(x = NULL, y = NULL,
       title = "Real gross domestic product, before and after benchmark revisions",
       subtitle = "Non-annualized, seasonally adjusted.",
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
  "charts/COVID charts/bench_quarterly_change.png",
  p,
  device = "png",
  width = 14.2,
  height = 8
)


p <- unrev_gdi %>% 
  filter(series_id %in% c("GDP", "GDI")) %>% 
  spread(series_id, value) %>% 
  mutate(discrep = GDP/GDI,
         label = "Unrevised")

p <- gdi_vs_gdp %>% 
  filter(LineNumber %in% c("1", "2")) %>% 
  select(date, LineNumber, DataValue) %>% 
  spread(LineNumber, DataValue) %>% 
  mutate(discrep = `1`/`2`,
         label = "Revised") %>% 
  bind_rows(p)

p <- p %>% 
  filter(year(date) >= 2010) %>% 
  ggplot(aes(date, discrep, colour = label)) +
  geom_line(size = 1, show.legend = F) +
  scale_colour_manual(values = c(Revised = "#1f78b4", Unrevised = "#a6cee3"))
