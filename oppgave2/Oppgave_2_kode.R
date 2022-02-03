library(tidyverse)
library(jsonlite)

#url <- "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json"

# download.file(url, "covid_data")

covid_data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

covid_data  
#as_tibble(covid_data)

covid_data %>% 
  as_tibble %>% 
  mutate(fully_vaccinated_pct_of_pop=fully_vaccinated_pct_of_pop*100)


ggplot(covid_data, aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k)) +
  geom_point() +
  ggtitle("Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates") +
  xlab("Share of total population fully vaccinated") +
  ylab("avg. monthly deaths per 100.000")
