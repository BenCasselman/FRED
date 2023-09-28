# CBO projections
library(rvest)

https://www.cbo.gov/system/files/2018-06/51135-2011-08-economicprojections.xlsx

c("Jul 2023", "Feb 2023", "May 2022", "Jul 2021", "Feb 2021", "Jul 2020", "Jan 2020", "Aug 2019", "Jan 2019", "Aug 2018", "Apr 2018", "Jun 2017", "Jan 2017", "Aug 2016", "Jan 2016", "Aug 2015", "Jan 2015", "Aug 2014", "Feb 2014", "Feb 2013", "Aug 2012", "Jan 2012", "Aug 2011")

cbo_page <- rvest::read_html("https://www.cbo.gov/data/budget-economic-data")

cbo_links <- cbo_page %>% 
  html_nodes(".field-content a") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  filter(grepl("51135", value))
  
walk(cbo_links$value, function(x) {
  url <- paste0("https://www.cbo.gov", x)
  filename <- paste0("cbo_projections/", substr(x, gregexpr("51135", x)[[1]][1], nchar(x)))
  download.file(url = url, destfile = filename, method = "libcurl", mode = "wb")
  
})

w <- readxl::read_xlsx("cbo_projections/51135-2014-08-economicprojections.xlsx", sheet = "1. Quarterly", skip = 5)

w <- w %>% 
  select(-...1, -...3) %>% 
  slice(-1) %>% 
  mutate(series = case_when(is.na(...2) ~ lag(...2, 1),
                            TRUE ~ ...2)) %>% 
  select(-...2) %>% 
  gather(date, value, -series, -Units) %>% 
  mutate(date = ymd(paste(substr(date, 1, 4),
                          as.numeric(substr(date, 6, 6)) * 3 - 2,
                          "01", sep = "-")))

w %>% 
  filter(series == "Real GDP",
         Units == "Billions of 2009 dollars") %>% 
  ggplot(aes(date, value)) +
  geom_line()

cbo_reader <- function(filename) {
  w <- readxl::read_xlsx(filename, sheet = "1. Quarterly", skip = 5)
  colnames(w)[4] <- "Units"
  if ("OBS" %in% names(w)) w <- w %>% select(-OBS)
  vintage <- substr(filename, 23, 29)
  
  w <- w %>% 
    select(-...1, -...3) %>% 
    slice(-1) %>% 
    mutate(series = case_when(is.na(...2) ~ lag(...2, 1),
                              TRUE ~ ...2)) %>% 
    select(-...2) %>% 
    gather(date, value, -series, -Units) %>% 
    mutate(date = ymd(paste(substr(date, 1, 4),
                            as.numeric(substr(date, 6, 6)) * 3 - 2,
                            "01", sep = "-")),
           vintage = vintage)
  
  w
}

paste0("cbo_projections/", 
       list.files("cbo_projections/")[4]) %>% 
  cbo_reader()

cbo_projects <- map_dfr(list.files("cbo_projections/"), function(x) {
  print(x)
  filename <- paste0("cbo_projections/", x)
  cbo_reader(filename) 
  })

cbo_projects %>% 
  filter(series == "Real GDP",
         Units != "Percentage change, annual rate") %>% 
  ggplot(aes(date, value, colour = vintage)) +
  geom_line()

cbo_projects %>% 
  filter(series == "Real GDP") %>% 
  count(Units)