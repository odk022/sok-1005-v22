# Oppgave 1

library(tidyverse)
library(jsonlite)
library(readxl)


covid_data <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")


covid_data <- covid_data %>% 
  as_tibble %>% 
  mutate(fully_vaccinated_pct=fully_vaccinated_pct_of_pop*100) 

covid_data

ggplot(covid_data, aes(x = fully_vaccinated_pct, y = deaths_per_100k)) +
  geom_point(size = 2, color = "#99CC99") + 
  geom_text(aes(label = name), size=3, color = "#999999", nudge_y = 0.5) +
  xlab("Share of total population fully vaccinated") +
  ylab("avg. monthly deaths per 100.000") +
  labs("Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates") + +
  theme_bw() +
  theme(axis.title = element_text(size = 9)) 






# stater_fk <- read_excel("oppgave2/stater_forkortelse.xlsx")  

#covid_data_fk <- 
 # covid_data %>% 
  #bind_cols(covid_data, stater_fk$s_name) 





# Ny start

# covid_data_liten <- covid_data %>% 
#   as_tibble %>% 
#   mutate(fully_vaccinated_pct=fully_vaccinated_pct_of_pop*100) %>% 
#   select(name, deaths_per_100k, fully_vaccinated_pct)
# 
# stater <- stater_forkortelse <- read_excel("oppgave2/stater_forkortelse.xlsx")
# 
# covid_data_fk <- bind_cols(stater$s_name, covid_data_liten)
# 
# # covid_data_fk <- covid_data_fk[, c("name", "s_name", "deaths_per_100k","fully_vaccinated_pct")]
# 
# ggplot(covid_data_fk, aes(x = fully_vaccinated_pct, y = deaths_per_100k)) +
#   geom_point(size = 2, color = "#99CC99") +
#   geom_text(aes(label = s_name), size=3, color = "#999999", nudge_y = 0.5) +
#   labs(
#     title = "Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates", 
#     x = "Share of total population fully vaccinated",
#     y = "avg. monthly deaths per 100.000"
#   ) +
#   theme_bw() +
#   theme(axis.title = element_text(size = 9)) 
# 
# names(covid_data_fk)

 # Oppgave 2:

library(mosaic)
# lager regresjonslinje for sammenhengen mellom vaksinasjonsrate og dødsrate, 
# kaller den "reg_linje", og lager plott av dette.

reg_linje <- lm(deaths_per_100k ~ fully_vaccinated_pct , data = covid_data)
reg_linje
# Verdiene angir henholdvis skjæring med y-aksen og stigningstallet til regresjonslinjen. 
# Dette kan tolkes som at 1 % økning i andelen fullvaksinerte vil gi 0.36 % endring i dødsraten.

#plotModel(reg1) +
 # labs(title="Sammenheng mellom dødsrate og vaksinasjonsrate")  


ggplot(covid_data, aes(x = fully_vaccinated_pct, y = deaths_per_100k)) +
  geom_point(size = 2, color = "#99CC99") +
  geom_text(aes(label = name), size=3, color = "#999999", nudge_y = 0.5) +
  labs(
    title = "Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates", 
    x = "Share of total population fully vaccinated",
    y = "avg. monthly deaths per 100.000"
  ) +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() +
  theme(axis.title = element_text(size = 9)) 

library(readr)

# url: "https://www.50states.com/abbreviations.htm"
# test <- fromJSON("https://www.50states.com/wp-json/")
# names(test)
# test$name


