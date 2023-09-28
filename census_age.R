# Census single year of age files
census_age <- read_csv("nc-est2020-agesex-res.csv")
census_age <- read_csv("nc-est2021-agesex-res.csv") %>% 
  left_join(census_age %>% select(-POPESTIMATE2020), by = c("SEX", "AGE"))

# census_age_older <- read_csv("us-est00int-alldata.csv") 
# 
# census_age_older <- census_age_older %>% 
#   filter(AGE != 999,
#          MONTH == 7) %>% 
#   select(YEAR, AGE, TOT_POP, TOT_MALE, TOT_FEMALE) %>% 
#   gather(sex, pop, -YEAR, -AGE) %>% 
#   mutate(SEX = case_when(sex == "TOT_POP" ~ 0,
#                          sex == "TOT_MALE" ~ 1,
#                          sex == "TOT_FEMALE" ~ 2)) %>% 
#   select(-sex) %>% 
#   pivot_wider(
#     names_from = "YEAR", 
#     names_prefix = "POPESTIMATE", 
#     values_from = "pop"
#   )
# 
# census_age <- census_age %>% 
#   left_join(census_age_older, by = c("AGE", "SEX"))

census_age %>% 
  filter(SEX == 0) %>% 
  select(AGE, POPESTIMATE2010, POPESTIMATE2021) %>% 
  gather(year, pop, -AGE) %>% 
  filter(AGE < 999) %>% 
  mutate(grp = case_when(AGE %in% 55:64 ~ "60s",
                         TRUE ~ "all_other")) %>% 
  ggplot(aes(AGE, pop, fill = grp)) +
  geom_col() +
  facet_wrap(~year) +
  scale_fill_manual(values = c(`60s` = "#1f78b4",
                               all_other = "grey75"))


census_age %>% 
  filter(SEX == 0) %>% 
  select(AGE, POPESTIMATE2010, POPESTIMATE2021) %>% 
  gather(year, pop, -AGE) %>% 
  filter(AGE < 999) %>% 
  mutate(birthyear = case_when(year == "POPESTIMATE2010" ~ 2010 - AGE,
                               year == "POPESTIMATE2021" ~ 2021 - AGE),
         grp = case_when(birthyear %in% 1946:1964 ~ "Baby boom",
                         TRUE ~ "all_other"),
         year = factor(year, levels = c("POPESTIMATE2010", "POPESTIMATE2021"),
                       labels = c("2010", "2021"))) %>% 
  ggplot(aes(AGE, pop/1000000, fill = grp)) +
  geom_col(show.legend = F) +
  facet_wrap(~year) +
  scale_fill_manual(values = c(`Baby boom` = "#1f78b4",
                               all_other = "grey75")) +
  geom_vline(xintercept = 65, linetype = "dashed") +
  labs(x = "Age", y = "Pop. in millions",
       title = "U.S. population by age, 2010 vs 2021",
       subtitle = "Highlighted area represents baby boom generation. Dashed line marks traditional retirement age (65).")


census_age %>% 
  filter(SEX == 0) %>% 
  select(AGE, POPESTIMATE2010, POPESTIMATE2021) %>% 
  gather(year, pop, -AGE) %>% 
  filter(AGE < 999) %>% 
  mutate(birthyear = case_when(year == "POPESTIMATE2010" ~ 2010 - AGE,
                               year == "POPESTIMATE2021" ~ 2021 - AGE),
         grp = case_when(birthyear %in% 1946:1964 ~ "Baby boom",
                         TRUE ~ "all_other"),
         year = factor(year, levels = c("POPESTIMATE2010", "POPESTIMATE2021"),
                       labels = c("2010", "2021"))) %>% 
  ggplot(aes(AGE, pop/1000000, fill = grp)) +
  geom_col(show.legend = F) +
  facet_wrap(~year) +
  scale_fill_manual(values = c(`Baby boom` = "#1f78b4",
                               all_other = "grey75")) +
  geom_rect(data = tibble(start = 18, end = 64),
            inherit.aes = F, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), alpha = 0.2) +
  labs(x = "Age", y = "Pop. in millions",
       title = "U.S. population by age, 2010 vs 2021",
       subtitle = "Highlighted area represents baby boom generation. Shaded area denotes traditional working age (18-64).")




census_age %>% 
  filter(SEX == 0) %>% 
  select(AGE, POPESTIMATE2021) %>% 
  mutate(age_adj = AGE + 1,
         retirement = case_when(age_adj >= 65 ~ "retirement age",
                                TRUE ~ "not")) %>% 
  filter(age_adj %in% 58:76) %>% 
  group_by(retirement) %>% 
  summarize(n = sum(POPESTIMATE2021)) %>% 
  mutate(share = n/sum(n))

library(tidycensus)
load("~/mapping/census_key.RData")
census_api_key(census_key)

test <- get_estimates("us", breakdown = "AGEGROUP", year = 2009)

url <- paste0("https://api.census.gov/data/2000/pep/int_charage?get=GEONAME,POP,DATE_,DATE_DESC,AGE&for=us:1&key=", census_key)
test <- GET(url)

test2 <- map_dfr(1:(content(test, "parsed") %>% length - 1),
                 ~content(test, "parsed")[[.x + 1]] %>% as.data.frame(col.names = content(test, "parsed")[[1]])
) %>% 
  as_tibble() %>% 
  mutate(POP = as.numeric(POP),
         AGE = as.numeric(AGE))


test2 %>% 
  filter(DATE_DESC != "4/1/2000 population estimates base") %>% 
  mutate(YEAR = substr(DATE_DESC, 5,8)) %>% 
  filter(AGE < 999) %>% 
  ggplot(aes(AGE, POP)) +
  geom_col() +
  facet_wrap(~YEAR)

p <- census_age %>% 
  filter(SEX == 0, AGE < 999) %>% 
  select(AGE, pop = POPESTIMATE2021) %>% 
  mutate(YEAR = 2021)

p <- test2 %>% 
  filter(DATE_DESC != "4/1/2000 population estimates base") %>% 
  mutate(YEAR = as.numeric(substr(DATE_DESC, 5,8))) %>% 
  filter(AGE < 999, YEAR == 2009) %>% 
  select(AGE, YEAR, pop = POP) %>% 
  bind_rows(p)

p %>% 
  mutate(birthyear = case_when(YEAR == 2009 ~ 2009 - AGE,
                               YEAR == 2021 ~ 2021 - AGE),
         grp = case_when(birthyear %in% 1947:1965 ~ "Baby boom",
                         TRUE ~ "all_other")) %>% 
  ggplot(aes(AGE, pop/1000000, fill = grp)) +
  geom_col(show.legend = F) +
  facet_wrap(~YEAR) +
  scale_fill_manual(values = c(`Baby boom` = "#1f78b4",
                               all_other = "grey75")) +
  geom_vline(xintercept = 65, linetype = "dashed") +
  labs(x = "Age", y = "Pop. in millions",
       title = "U.S. population by age, 2009 vs 2021",
       subtitle = "Highlighted area represents baby boom generation. Dashed line marks traditional retirement age (65).")


p %>% 
  mutate(birthyear = case_when(YEAR == 2009 ~ 2009 - AGE,
                               YEAR == 2021 ~ 2021 - AGE),
         grp = case_when(birthyear %in% 1947:1965 ~ "Baby boom",
                         TRUE ~ "all_other")) %>% 
  filter(YEAR == 2009) %>% 
  select(AGE, grp, pop) %>%
  mutate(pop = pop/1000000) %>% 
  write.table("clipboard", sep = "\t", row.names = F)


p %>% 
  mutate(birthyear = case_when(YEAR == 2009 ~ 2009 - AGE,
                               YEAR == 2021 ~ 2021 - AGE),
         grp = case_when(birthyear %in% 1947:1965 ~ "Baby boom",
                         TRUE ~ "all_other")) %>% 
  filter(YEAR == 2021) %>% 
  select(AGE, pop) %>%
  mutate(pop = pop/1000000) %>% 
  write.table("clipboard", sep = "\t", row.names = F)


census_age %>% 
  filter(SEX == 0, AGE < 999) %>% 
  select(AGE, pop = POPESTIMATE2012) %>% 
  filter(AGE %in% 50:70) %>% 
  arrange(desc(pop))