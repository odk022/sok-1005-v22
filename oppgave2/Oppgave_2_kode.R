
# Mappeinnlevering 2:

# Oppgave 1

# LLoading necessary packages:
library(tidyverse)
library(jsonlite)
library(scales)

# The is the main file from New York Times:
covid_data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

covid_data <- covid_data %>% 
  as_tibble

# Getting a file with the abbreviations of the US states:
library(readr)
stater_fk <- read_csv("https://scottontechnology.com/wp-content/uploads/2014/08/50_us_states_all_data.csv", 
                                   col_names = FALSE) %>% select(X2, X4)

head(stater_fk)

# Putting the Covid data and abbrevitions together:
covid_data <- left_join(covid_data, stater_fk, by = c("name" = "X2"))

# Replaces the "na" for Washington DC:
covid_data[is.na(covid_data)] <-"D.C."

# Picks variables, makes a percentage variable and renames X4:
covid_data <- covid_data %>% 
  select(name, X4, deaths_per_100k, fully_vaccinated_pct_of_pop) %>% 
  mutate(fully_vaccinated_pct=fully_vaccinated_pct_of_pop*100) %>% 
  rename(stat_fk = X4) 

# Making a plot:
pl_death_rate <- ggplot(covid_data, aes(x = fully_vaccinated_pct, y = deaths_per_100k)) +
  geom_point(size = 2, color = "#99CC99") + 
  geom_text(aes(label = stat_fk), size=3, color = "#999999", nudge_y = 0.5) +
  scale_x_continuous("Share of total population fully vaccinated in %", limits = c(45, 80),
                                             breaks = seq(45, 80, 5)) +
  xlab("Share of total population fully vaccinated") +
  ylab("avg. monthly deaths per 100.000") +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates") + 
  theme_bw() +
  theme(axis.title = element_text(size = 9, colour = "#999999")) +
  geom_segment(x = 57 , y = 15,
             xend = 56, yend = 15.7,
             color = "black",
             arrow = arrow(length = unit(0.2, "cm"), type =  "closed")) +
  annotate("text",
           x = 60, y = 15,
           label = "Lower vaccination rate,\n higher death rate",
           vjust = 1, size = 3, color = "black", fontface=2) +
  geom_segment(x = 73 , y = 9,
             xend = 74, yend = 8.3,
             color = "black",
             arrow = arrow(length = unit(0.2, "cm"), type =  "closed")) +
  annotate("text",
           x = 71, y = 10,
           label = "Higher vaccination rate,\n lower death rate",
           vjust = 1, size = 3, color = "black", fontface=2) 



 # Oppgave 2:

library(mosaic)

# Making a regression line that shows the connection between vaccination rate and death rate. 
# and call it "reg_line",

reg_line <- lm(deaths_per_100k ~ fully_vaccinated_pct , data = covid_data)
reg_line

# The values gives interception with y-axis and the slope of the regression line.
# This can be interpreted as follow: 1 % increase in vaccination rate would cause a 0.366 %
# decrease in death rate.

# Making a plot with a regression line:
pl_regression <- ggplot(covid_data, aes(x = fully_vaccinated_pct, y = deaths_per_100k)) +
  geom_point(size = 2, color = "#99CC99") + 
  geom_text(aes(label = stat_fk), size=3, color = "#999999", nudge_y = 0.5) +
  scale_x_continuous("Share of total population fully vaccinated in %", limits = c(45, 80),
                     breaks = seq(45, 80, 5)) +
  xlab("Share of total population fully vaccinated") +
  ylab("avg. monthly deaths per 100.000") +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with
vaccination rates") + 
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  theme(axis.title = element_text(size = 9, colour = "#999999")) +
  annotate("curve", x = 65, y = 13,
           xend = 60, yend = 9.5,
           arrow = arrow(length = unit(0.2, "cm"), type =  "closed"),
           color = "grey40") +
  annotate("text",
           x = 71, y = 13.5,
           label = "The regression line deathrate Covid-19 \n and vaccination rate",
           vjust = 1, size = 3, color = "grey40", fontface=2) 

  




