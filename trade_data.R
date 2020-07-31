# Trade data from BEA

library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
source("BEAR.R")

# Trade variables
load("~/FRED/bea_key.RData")
trade_vars <- beaParamVals(bea_key, "ITA", "Indicator") %>% 
  as_tibble

trade_countries <- beaParamVals(bea_key, "ITA", "AreaOrCountry") %>% 
  as_tibble

# Get data
trade_list <- c("BalGds", "BalGdsServ", "BalServ", "ExpGds", "ImpGds", 
                "ImpGdsCrudePet", "ImpGdsDurCons", "ImpGdsEnergyProds")

trade_tables <- map_dfr(trade_list, ~bea_trade(indicator = .x, dataset = "ITA"))

trade_tables %>% 
  filter(Frequency == "QSA", Indicator == "BalGdsServ") %>% 
  ggplot(., aes(date, DataValue)) + geom_bar(stat = "identity")


# China
china_list <- c("BalGds", "ExpGds", "ExpGdsAgFoodsFeedsAndBevs", "ExpGdsSoybeans",
                "ImpGds")

china_trade <- map_dfr(trade_list, ~bea_trade(indicator = .x, dataset = "ITA", country = "China"))

china_trade %>% 
  filter(Frequency == "QSA", Indicator %in% c("ExpGds", "ImpGds")) %>% 
  group_by(Indicator) %>% 
  mutate(change_saar = saar(DataValue)) %>% 
  ggplot(., aes(date, change_saar)) + geom_bar(stat = "identity") +
  facet_wrap(~Indicator)

# Trade balance, imports and exports by country/region
countrylist <- c("AllCountries", "Africa", "EU", "Europe", "UnitedKingdom", "Canada", "Mexico",
                 "China", "Japan", "KoreaRepOf", "MiddleEast", "Russia", "SouthAndCenAm", "HongKong", 
                 "India", "AsiaAndPac")
tradelist <- c("BalGds", "ImpGds", "ExpGds")

country_data <- cross_df(list(countries = countrylist, vars = tradelist)) %>% 
  pmap_dfr(~bea_trade(country = .x, indicator = .y))

country_data %>% 
  filter(Frequency == "QNSA",
         year(date) >= 2010) %>% 
  select(date, Indicator, country, DataValue) %>% 
  spread(country, DataValue) %>% 
  mutate(Europe = Europe - EU - Russia,
         EU = EU - UnitedKingdom,
         AsiaAndPac = AsiaAndPac - China - HongKong - Japan - KoreaRepOf - India,
         SouthAndCenAm = SouthAndCenAm - Mexico) %>% 
    mutate(other = AllCountries - rowSums(select(., 3, 5:18), na.rm = T)) %>% 
  select(-AllCountries) %>% 
  gather(country, value, -date, -Indicator) %>% 
  mutate(china = case_when(country == "China" ~ "china"),
         country = factor(country),
         country = factor(country, levels = c("China", levels(country)[-which(levels(country) == "China")])),
         country = fct_rev(country)) %>% 
  ggplot(., aes(date, value, group = country, fill = china)) + geom_bar(stat = "identity") +
  facet_wrap(~Indicator) +
  geom_hline(yintercept = 0)

country_data %>% 
  filter(Frequency == "QSA",
         year(date) >= 2000,
         country %in% c("China", "AllCountries")) %>% 
  select(date, Indicator, country, DataValue) %>% 
  spread(country, DataValue) %>% 
  mutate(AllCountries = AllCountries - China) %>% 
  gather(country, value, -date, -Indicator) %>% 
  ggplot(., aes(date, value, fill = country)) + geom_bar(stat = "identity") +
  facet_wrap(~Indicator) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(China = "#F8766D", AllCountries = "grey70"))

  
# Soybeans
soybeans <- bea_trade("ExpGdsSoybeans")

soybeans %>% 
  filter(Frequency == "QSA") %>% 
  ggplot(., aes(date, DataValue)) + geom_line()



