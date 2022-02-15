
# Mappeinnlevering 3:

library(tidyverse)
library(rvest)
library(httr)
library(readr)

# Scarping the actual table and setting the first row as header:
url <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")
rekkevidde <- html_table(html_nodes(url, "table")[[1]], header = TRUE)
rekkevidde


# Removes the car model that have "x" in column "STOPP" and "Avvik":
rekkevidde_2 <- subset(rekkevidde, !STOPP == "x")
rekkevidde_2

# Changes the names of the columns:
rekkevidde_2 <-
  rekkevidde_2 %>% 
  rename(wltp = `WLTP-tall`, stopp = STOPP) 
  

# Removes "km" from the "stopp" column:
rekkevidde_2$stopp <- gsub("km", "",as.character(rekkevidde_2$stopp))
rekkevidde_2$Avvik <- gsub("%", "",as.character(rekkevidde_2$Avvik))

rekkevidde_2

# Removes text from the "wltp" column
rekkevidde_2$wltp <-substr(rekkevidde_2$wltp,1,3)

rekkevidde_2

sapply(rekkevidde_2, class)


rekkevidde_2$wltp <- as.numeric(as.character(rekkevidde_2$wltp))
rekkevidde_2$stopp <- as.numeric(as.character(rekkevidde_2$stopp))
#rekkevidde_2$Avvik <- as.numeric(as.character(rekkevidde_2$Avvik))
print(rekkevidde_2)
sapply(rekkevidde_2, class)

# lager en kolonnevektor:
#kolonner <- c(2,3,4)

#rekkevidde_2[ , kolonner]<- apply(rekkevidde_2[ , kolonner], 3,
                                  #function(x) as.numeric(as.character(x)))



ggplot(rekkevidde_2, aes(x = wltp, y = stopp)) +
  geom_point() +
  theme_classic()

