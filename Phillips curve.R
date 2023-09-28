# Phillips curve
library(tidyverse)
library(httr)
library(lubridate)

# This entry is REQUIRED. Can included multiple series
series_id <- c("UNRATE", "LNS12300060", "ECIWAG")  # Only required entry

# These are all optional. These must be consistent for all series IDs.
startdate <- "1994-01-01"
enddate <- "" # Leave blank for latest data
units <- "lin" # Options are lin = levels, chg = change, ch1 = change from year ago, pch = pct change, pc1 = pct change from year ago, pca = compounded annual rate of change, cch = continuously compounded rate of change, cca = continuously compounded annual rate of change, log = natural log
freq <- "q" # options are d = daily, w = weekly, bw = biweekly, m = monthly, q = quarterly, sa = semiannual, a = annual
agg <- "" # Aggregation method. Options are avg = average, sum = sum, eop = end of period
output <- 1 # Output type. Defaults to 1. Other options are:
# 1 = Observations by Real-Time Period
# 2 = Observations by Vintage Date, All Observations
# 3 = Observations by Vintage Date, New and Revised Observations Only
# 4 = Observations, Initial Release Only
vintages <- "" # comma-separated string of vintage dates
realtime_start <- "" # Probably won't use these much. See explanation here: https://research.stlouisfed.org/docs/api/fred/realtime_period.html
realtime_end <- ""

# Assuming you just want the tidy data, run this:
phil <- fred_mult(series_id = series_id, 
                observation_start = startdate, 
                observation_end = enddate, 
                units = units, 
                frequency = freq, 
                aggregation_method = agg,
                output_type = output,
                vntage_dates = vintages,
                realtime_start = realtime_start,
                realtime_end = realtime_end)

# Example:
p <- ggplot(df, aes(date, value, fill = series_id)) + geom_bar(stat = "identity", position = "dodge")
p + recession_shade(startdate)


phil %>% filter(year(date) >= 1995) %>% 
  spread(series_id, value)


old_eci <- read_tsv("Year	January	April	July	October
1980	37	37.9	38.7	39.5
                    1981	40.5	41.4	42.1	43
                    1982	43.8	44.3	45.1	45.6
                    1983	46.1	46.7	47.3	47.9
                    1984	48.4	48.9	49.2	49.8
                    1985	50.4	51	51.6	51.9
                    1986	52.5	52.8	53.1	53.5
                    1987	54	54.3	54.9	55.3
                    1988	55.8	56.4	56.9	57.5
                    1989	58.2	58.7	59.3	60
                    1990	60.6	61.3	61.9	62.3
                    1991	63	63.6	64.1	64.6
                    1992	65.1	65.5	65.8	66.3
                    1993	66.8	67.3	67.8	68.4
                    1994	68.7	69.3	69.8	70.2
                    1995	70.8	71.3	71.8	72.3
                    1996	73	73.7	74.2	74.8
                    1997	75.4	76.1	76.8	77.8
                    1998	78.5	79.1	80.1	80.8
                    1999	81	82	82.7	83.5
                    2000	84.4	85.3	86.1	86.8
                    2001	87.8	88.6	89.2	90.1
                    2002	90.9	91.7	92.1	92.6
                    2003	93.5	94.2	94.9	95.3
                    2004	96	96.5	97.3	97.7
                    2005	98.2	98.8	99.4	100.1")

old_eci <- old_eci %>% 
  gather(month, value, -Year) %>% 
  mutate(date = ymd(paste(Year, month, 1)))

old_eci %>% 
  filter(date < ymd("2001-01-01"),
         date >= ymd("1994-01-01")) %>% 
  select(date, value) %>% 
  mutate(series_id = "ECIWAG") %>% 
  bind_rows(phil %>% filter(!is.na(value))) %>% 
  spread(series_id, value) %>% 
  rename(epop = LNS12300060) %>% 
  mutate(ECIWAG = ECIWAG/lag(ECIWAG, 4) -1,
         epop = 100 - epop)  %>% 
  gather(unemp, value, -date, -ECIWAG) %>% 
  filter(!is.na(ECIWAG)) %>% 
  mutate(new = case_when(year(date) >= 2016 ~ "recent")) %>% 
  ggplot(., aes(value, ECIWAG)) + 
  geom_point(aes(colour = new)) +
  geom_smooth(method = "lm") +
  facet_grid(~unemp, scales = "free_x")


old_eci %>% 
  filter(date < ymd("2001-01-01"),
         date >= ymd("1994-01-01")) %>% 
  select(date, value) %>% 
  mutate(series_id = "ECIWAG") %>% 
  bind_rows(phil %>% filter(!is.na(value))) %>% 
  spread(series_id, value) %>% 
  rename(epop = LNS12300060) %>% 
  mutate(ECIWAG = 100*(ECIWAG/lag(ECIWAG, 4) -1),
         epop = 100 - epop,
         new = case_when(date == max(date) ~ "latest",
                         year(date) >= 2016 ~ "recent")) %>% 
  filter(!is.na(ECIWAG)) %>% 
  rename(`Wage growth` = ECIWAG,
         `Nonemployment rate` = epop,
         `Unemployment rate` = UNRATE) %>% 
  write.csv("eci_chart.csv")
                                           
                                           