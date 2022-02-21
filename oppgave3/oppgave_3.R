
# Mappeinnlevering 3:

library(tidyverse)
library(rvest)

# Oppgave 1: 

# Scarping the actual table and setting the first row as header:
url <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")
rekkevidde <- html_table(html_nodes(url, "table")[[1]], header = TRUE)
rekkevidde


#Tidying the data set
rekkevidde_2 <-
  subset(rekkevidde, !STOPP == "x") %>% # replaces"x" in column "STOPP" 
  rename(wltp = `WLTP-tall`, stopp = STOPP) %>% # renaming the columns
  mutate(Avvik = str_replace(Avvik, ",","."), # replaces comma with point
         Avvik = gsub("%", "",as.character(Avvik)), # removes text
         stopp = gsub("km", "",as.character(stopp)), # removes text
         wltp = substr(wltp,1,3), # removes text
         wltp = as.numeric(as.character(wltp)), # from chr to num
         stopp = as.numeric(as.character(stopp)), # from chr to num
         Avvik = as.numeric(as.character(Avvik))) # from chr to num

rekkevidde_2

# Plotting "rekkevidde_2:
plot_1 <- ggplot(rekkevidde_2, aes(x = wltp, y = stopp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.65, color = "red") +
  labs(title="Faktisk rekkevidde i km i forhold til oppgitt rekkevidde for elbiler(wltp)") +
  scale_x_continuous(name= "wltp", limits = c(200,600)) +
  scale_y_continuous(name= "stopp", limits = c(200,600)) +
  theme_classic() +
  geom_segment(x = 420 , y = 500,
             xend = 450, yend = 460,
             color = "black",
             arrow = arrow(length = unit(0.2, "cm"), type =  "closed")) +
  annotate("text",
           x = 400, y = 530,
           label = "The red line show the case 
           if wltp were equal to actual driving lenght",
           vjust = 1, size = 3, color = "black", fontface=2) +
  geom_segment(x = 500 , y = 280,
               xend = 480, yend = 310,
               color = "black",
               arrow = arrow(length = unit(0.2, "cm"), type =  "closed")) +
  annotate("text",
           x = 500, y = 270,
           label = "The dots represent actual driving
           lenght for car models with different wltp",
           vjust = 1, size = 3, color = "black", fontface=2) 
plot_1



# Oppgave 2:

# making a regreesion:
Reg <- lm(stopp ~ wltp, data = rekkevidde_2)
Reg
# The slope of the regression line is 0.8671 and it crosses the y-axis at -26.6450. This can be interpreted like this:
# A 1 % increase in wltp,increases the stop value with 0.8671 % . This implies that the higher the stated mileage (wltp), 
# the lower the relative mileage before stopping will be.  

plot_2 <- plot_1 +
  geom_smooth(method = lm, se = FALSE)

plot_2
