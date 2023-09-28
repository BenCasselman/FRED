library(tidyverse)
library(scales)
library(lubridate)

balance <- read_csv("balance_sheet.csv")

p <- balance %>% 
  gather(class, value, -Date) %>% 
  mutate(Date = as.Date(mdy(Date)),
         class = factor(class, levels = c("mortgage", "treasuries", "other"), labels = c("Mortgage-backed securities", "Treasury bonds", "Other assets"))) %>% 
  ggplot(., aes(Date, value, fill = class)) + geom_area()

p +
  labs(x = NULL, y = NULL,
       title = "Fed Balance Sheet",
       subtitle = "The Fed stopped buying assets under Ms. Yellen, but it has only just begun to reduce its holdings.",
       caption = "Source: The Federal Reserve") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        plot.background = element_rect(fill = "grey92"),
        panel.grid.major = element_line(colour = "grey", size = 0.15),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "grey50"),
        axis.ticks = element_line(colour = "grey", size = 0.15),
        plot.caption = element_text(colour = "grey50")) +
  geom_hline(yintercept = 0, colour = "black") +
  scale_y_continuous(label = function(x) paste0("$", x, "trillion")) +
