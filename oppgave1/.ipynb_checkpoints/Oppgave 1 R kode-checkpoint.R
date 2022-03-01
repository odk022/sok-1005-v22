# Laster ned nødvendige pakker
library(tidyverse)
library(lubridate)
library(zoo) # for å lage glidende gjennomsnitt
library(janitor) # endrer til variabelnavn uten store bokstaver

# Kilde:  https://www.drroyspencer.com/latest-global-temperatures/


# Oppgave 1:
# Laster ned data:
lower_tropos <- read_table("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

# sørger for at det kun er relevante rader som er med
lower_tropos <-lower_tropos[1:which(lower_tropos$Year %in% "Year")-1, ]

# vil bare ha små bokstaver i variablene, bruker "janitor" til dette
lower_tropos <- lower_tropos %>% 
            clean_names()

# velger variablene
lower_tropos <- lower_tropos %>% 
            select(year, mo, globe)

# ordner dataformatet og setter alle "chr" til "num"
lower_tropos <-lower_tropos %>% 
            mutate(dato = ymd(paste(year, mo, 1, sep = "-"))) %>% 
            mutate_if(is.character, ~as.numeric(.))

# lager glidende gjennomsnitt
lower_tropos <-
  lower_tropos %>% 
  select(dato, globe) %>% 
  mutate(glidende_snitt = rollmean(globe, 13, fill = NA, align = "center"))

tail(lower_tropos)

# lager plott til datasettet
lower_tropos %>% 
  ggplot(aes(x=dato, y=globe)) + geom_line(color="lightblue") + theme_bw() +
  geom_point(shape=1, color="blue") +
  geom_line(aes(y=glidende_snitt), color="red", lwd=1.2) +
  labs(x = "Tidsperiode", y = "Temperaturvariasjoner i grader Celsius", 
       title = "Gjennomsnittstemperaturer i lavere troposfære", 
       subtitle = "Endringer i perioden 1978 - 2021 månedlige målinger") +
  theme(axis.title = element_text(size = 8))


# Oppgave 2:
# lager en funksjon som henter data og ordner dem
# koden er hentet fra forelesning høst 2021 og modifisert 

# lager en fellesfunksjon for alle operasjoner som utføres:
# - henting av data
# - rydding

felles_funksjon <- function(url, location) {
  return(read_table(url) %>%
           .[1:which(.$Year %in% "Year")-1, ] %>%
           clean_names() %>%
           .[ , c("year", "mo", "no_pol")] %>%  
           mutate(dato = ymd(paste(.$year, .$mo, 1, sep = "-"))) %>%
           mutate_if(is.character, ~as.numeric(.)) %>%
           mutate(nivå = paste0(location)))
}

# henteliste for data:

url_list <- list("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt",
                 "http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt",
                 "http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt",
                 "http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")


# navneliste:
location_list <- list("lower_tropos","mid_tropos", "tropo_pause", "strato")

# samler data, navneliste og fellesfunksjonen:

alle_data <- map2(url_list, location_list, felles_funksjon)

library(plyr)

alle_data <- ldply(alle_data, data.frame)

# lager fellesplott for alle nivåene i atmossfæren samt gjennomsnitt 
alle_data %>% 
  select(dato, no_pol, nivå) %>%
  pivot_wider(names_from = nivå, values_from = no_pol) %>% 
  mutate(snitt=rowMeans(.[ , -1])) %>% 
  pivot_longer(-dato,
               names_to = "nivå",
               values_to = "no_pol") %>%
  dplyr::rename(temperatur = no_pol) %>% 
  ggplot(aes(x=dato, y=temperatur, color=nivå)) + 
  geom_line() + 
  theme_bw() +
  scale_size_manual(values = c(lower_tropos = 0.5, mid_tropos = 0.5, 
                               snitt = 2, strato = 0.5, tropo_pause = 0.5)) +
  labs(x = "Tidsperiode", y = "Temperatur i grader Celsius", 
  title = "Temperaturer i ulike deler av atmossfæren fra 60 til 90 grader nord ", 
  subtitle = "Endringer i perioden 1978 - 2021 månedlige målinger") +
  theme(axis.title = element_text(size = 8))


