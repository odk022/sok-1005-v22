# Loading necessary packages:
library(tidyverse)
library(lubridate)
library(kableExtra)

# downloading data
file_location <- paste0(getwd(), '/')

AppWichStoreAttributes <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/26afd5e7-90b1-4301-ac5e-5905b38c4ec2/file_downloaded"
dest <- paste0(file_location, "AppWichStoreAttributes.csv")
download.file(AppWichStoreAttributes, dest)

county_crime <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/3691994e-2874-4ee6-b215-12e130c96175/file_downloaded"
dest <- paste0(file_location, "county_crime.csv")
download.file(county_crime, dest)

county_demographic <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/527e7486-7233-460a-99e0-3529b7cd7d49/file_downloaded"
dest <- paste0(file_location, "county_demographic.csv")
download.file(county_demographic, dest)

county_employment <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/846ac757-721e-4fd9-a414-5871f60de093/file_downloaded"
dest <- paste0(file_location, "county_employment.csv")
download.file(county_employment, dest)

WEEKLY_WEATHER <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/b8708b3c-c5e8-456e-83f4-9f23f472a840/file_downloaded"
dest <- paste0(file_location, "WEEKLY_WEATHER.csv")
download.file(WEEKLY_WEATHER, dest)

WEEKLY_SALES_10STORES <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/b963fdd1-0df9-4257-bd62-65256ec9d57c/file_downloaded"
dest <- paste0(file_location, "WEEKLY_SALES_10STORES.csv")
download.file(WEEKLY_SALES_10STORES, dest)



# Removes all noise since we have finished downloading
rm(list=ls()) 


# I change the names of the datatables:
# AppWichStoreAttributes = df1_attr
# county_crime = df2_crime
# county_demographic = df3_demo
# county_employment = df4_em
# weekly_sales_10stores = df5_sales
# weekly_weather = df6_weather

# I go through the dfs and make necessary changes:

df1_attr <- AppWichStoreAttributes <- read_csv("AppWichStoreAttributes.csv")

# I change the names of Store_County and Store_Weather_Station.
df1_attr_2 <-df1_attr %>%
  rename(County_Name = Store_County, Weather_Station = Store_Weather_Station)
head(df1_attr_2)

#  
df2_crime <- county_crime <- read_csv("county_crime.csv")
head(df2_crime)

#  
df3_demo <- county_demographic <- read_csv("county_demographic.csv")
head(df3_demo)

#  
df4_em <- county_employment <- read_csv("county_employment.csv")
head(df4_em)

#  
df5_weather <- WEEKLY_WEATHER <- read_csv("WEEKLY_WEATHER.csv")
head(df5_weather)

# Formatting the weather date
df5_weather$Weather_Date <- 
  as.Date(df5_weather$Weather_Date, format = "%d/%m/%Y") 
df5_weather

#  
df6_sales <- WEEKLY_SALES_10STORES <- read_csv("WEEKLY_SALES_10STORES.csv")
head(df6_sales)

# I rename the store number column.
# Some dates are missing in the Date column, I use the other date columns

df6_sales_2 <- df6_sales %>% 
  rename(Store_Num = Store_num) %>% 
  mutate(Date = as.Date(with(df6_sales, paste(Year, Month, Day,sep="-")), "%Y-%m-%d"))

# Removing unnecessary columns:
df6_sales_3 <- 
  subset (df6_sales_2, select = -c(Year, Month, Day))

# Changing the letters in Description:
df6_sales_3$Description = str_to_title(df6_sales_3$Description)

df6_sales_3



# OPPGAVE 1:
# Den første oppgaven er å skrive R kode som slår sammen de 6 datasettene til et stort datasett.
# Du må benytte de variablene som de ulike datasettene har til felles for å gjøre dette. 
# Denne prosessen skal kort dokumenteres og kommenteres.

# I make one df out of alle df:

df1_df2 <- left_join(df1_attr_2, df5_weather, by = "Weather_Station")
df1_df3 <- left_join(df1_df2, df2_crime, by = "County_Name")
df1_df4 <- left_join(df1_df3, df3_demo, by = "County_Name")
df1_df5 <- left_join(df1_df4, df4_em, by = "County_Name")
All_df <- left_join(df1_df5, df6_sales_3, by = c("Store_Num", "Weather_Date" = "Date"))

All_df <- All_df %>% 
  rename("Date" = "Weather_Date") 
 

 
# OPPGAVE 2:
# Dataene skal benyttes til en ukentlig salgsrapport til et enkelt utsalg. Gi noen eksempler på hva innholdet
# i en slik kortsiktig individuell rapport bør inneholde. Begrunn dine valg og tankegangen bak figurer og 
# eventuelle tabeller.

# Velger utsalg nr 14
# velger uke nr 20
# lokasjon - stat, fylke, beliggenhet(skole, senter)
# Omsetning totalt (Sales)
# Omsetning pr varegruppe (INV_NUMBER). Grafikk
## hvorfor selger ulike varegrupper godt i ulike peroder, feks is om vinteren
# Hvor mye utgjør hver varegruppe av salget. Grafikk
# Sammenlikning med samme uke året før og uken før. Grafikk
# Fortjenste pr varegruppe (Profit)
# kan demo/andre variabler bidra til forklaring
# avgrensing
# går det bra eller dårlig, og hvorfor?

# I choose store no 14 and week 20:
Store_No_Nm <- All_df %>% 
  filter(Store_Num == 14)  
Store_No_Nm[1,2] 
Store_No_Nm[1,1]

Week <- All_df %>% 
  filter(Weather_Week == 20) 
Week[1,22]

# I choose week 20
sales_14_20 <-All_df %>% 
  filter(Weather_Week == 20, Store_Num == 14) %>% 
  select(Store_Name, Store_Num, Store_City, County_Name, Date, INV_NUMBER,Description, Price, Sold, 
         Sales, Tot_Sls, Unit_Cost, Cost,Cost_Percent, Margin, Profit) %>% 
  group_by(INV_NUMBER, Description, Price, Sold, Cost, Profit, Margin) %>% 
  summarise(Sales) %>% 
  ungroup()

# choose also week 19 to compare weeks:
sales_14_19 <-All_df %>% 
  filter(Weather_Week == 19, Store_Num == 14) %>% 
  select(Store_Name, Store_Num, Store_City, County_Name, Date, INV_NUMBER,Description, Price, Sold, 
         Sales, Tot_Sls, Unit_Cost, Cost,Cost_Percent, Margin, Profit) %>% 
  group_by(INV_NUMBER, Description, Price, Sold, Cost, Profit, Margin) %>% 
  summarise(Sales) %>% 
  ungroup()


# sum of variables:
sum(sales_14_19$Sales)
sum(sales_14_20$Sales)

sum(sales_14_19$Profit)
sum(sales_14_20$Profit)

# Change from week 19 to week 20:
sales_change = round(100 * ((sum(sales_14_20$Sales)/sum(sales_14_19$Sales)-1)),1)
sales_change

profit_change = round(100 * ((sum(sales_14_20$Profit)/sum(sales_14_19$Profit)-1)),1)
profit_change


max(sales_14_19$Sales)
max(sales_14_20$Sales)

# Sorting the goods after price groups:

sales_price_gr <- sales_14_20 %>% 
  group_by(price_group = ifelse(Price <= 1.0, "price_$1", 
                         ifelse(Price > 1 & Price <= 2, "price_$2", 
                         ifelse(Price > 2 & Price <= 3, "price_$3", 
                         ifelse(Price > 3 & Price <= 4, "price_$4", 
                         ifelse(Price > 4 & Price <= 5, "price_$5", 
                         ifelse(Price > 5 & Price <= 6, "price_$6", 
                         ifelse(Price > 6 & Price <= 7, "price_$7", 
                         ifelse(Price > 7 & Price <= 8.0, "price_$8", "price_over_$8"))))))))) %>% 
  summarise(Sold, Price, Sales, Profit)



sales_price_gr <-
  sales_price_gr %>%
  select(price_group, Sold, Price, Sales, Profit) %>%
  filter(price_group >= 0) %>%
  group_by(price_group) %>%
  summarise(Tot_sales = sum(Sales), Tot_sold = sum(Sold), Tot_profit = sum(Profit)) 
sales_price_gr
  
# Total sales and profit:
sum(sales_price_gr$Tot_sales)
max(sales_price_gr$Tot_sales)  

sum(sales_price_gr$Tot_profit)
max(sales_price_gr$Tot_profit)

sales_most <- 
  subset(sales_price_gr, Tot_sales == max(sales_price_gr$Tot_sales))
sales_most
sales_most[1,1]
sales_most[1,2]

profit_most <- 
  subset(sales_price_gr, Tot_profit == max(sales_price_gr$Tot_profit))
profit_most
profit_most[1,1]
profit_most[1,4]

# share of sales and profit per pricegroup:
sales_share <-
  sales_price_gr %>% 
  mutate(Share_sales = round(100 *(sales_price_gr$Tot_sales/sum(sales_price_gr$Tot_sales)), 1),
         Share_profit = round(100 *(sales_price_gr$Tot_profit/sum(sales_price_gr$Tot_profit)),1)) %>% 
  select(price_group, Tot_sold, Tot_sales, Share_sales, Tot_profit, Share_profit)
sales_share

Shares_most <- 
  subset(sales_share, Share_sales == max(sales_share$Share_sales)) 
Shares_most  

#test <- subset(sales_share, Tot_share_sales == max(sales_share$Share_sales))

# profit_min <- 
#   subset(sales_price_gr, Tot_profit == min(sales_price_gr$Tot_profit))
# profit_min
# profit_min[1,1]
# profit_min[1,4]

# Making a plot of sales per pricegroup:  
figure_1 <-
sales_price_gr %>% 
ggplot(aes(x=price_group, y = Tot_sales))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Tot_sales), vjust= -0.3, size=3.5)+
  labs(title = "Figure 1: Sales per pricegroup", x = "Pricegroups in $", y = "Total Sales per pricegoup",
       caption = "Figure 1 shows how Sales are distributed across pricegroups. For example, price_$5 mean goods that cost from $4 to $5.") +
  theme_classic()
figure_1

#Vi ser av figur 1 at den største omsetningen er av varer i prisgruppen `r sales_most[1,1]`. Totalomsetningen her er $`r sales_most[1,2]`. 
# Den første prisgruppen er negativ. Årsaken til dette er at den også inneholder refusjoner mm. 

# Making a plot of shares of sales per pricegroup:  
figure_1a <-
  sales_share %>% 
  ggplot(aes(x=price_group, y = Share_sales))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Share_sales), vjust= -0.3, size=3.5)+
  labs(title = "Figure 1a: Shares of Sales per pricegroup", x = "Pricegroups in $", y = "Share of Sales per pricegoup",
       caption = "Figure 1 shows how shares of Sales are distributed across pricegroups.") +
  theme_classic()
figure_1a

# Making a plot of profit per pricegroup:
figure_2 <-
  sales_price_gr %>% 
  ggplot(aes(x=price_group, y = Tot_profit))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Tot_profit), vjust= -0.3, size=3.5)+
  labs(title = "Figure 2: Profit per pricegroup", x = "Pricegroups in $", y = "Total Profit per pricegoup",
       caption = "Figure 2 shows how Profit are distributed across pricegroups. For example price_$5 mean goods that cost from $4 to $5.") +
  theme_classic()
figure_2

# Vi ser av figur 2 at den største fortjenesten oppnås av omsetningen i prisgruppen `r profit_most[1,1]`. 
# Den totale fortjenesten her er $`r profit_most[1,4]`. Den første prisgruppen er negativ. Årsaken til dette er at den også inneholder refusjoner mm. som vil 
# påvirke fortjenesten negativt.

# Grouping the sales:
# Extract matching rows with str_detect - not finished
# sales_gr <- sales_14[str_detect(sales_14$Description, c("REGULAR", "MINI")), ]  
# head(sales_gr)

# Dette er en ukerapport for utsalg nr 14(lenke til nr og navn) for perioden uke nr 20(lenke) som gir en 
# oversikt og vurdering av resultatene. Den totale omsetningen i perioden var $ `r sum(sales_14$Sales)` som er en 
# endring på `r sales_change` prosent i forhold til uken før. Det var størst omsetning av (lenke). 
# Dette utgjorde (lenke) prosent av totalomsetningen.  

# Vareutvalget er veldig bredt, og det er derfor gjort en gruppering etter priser. Denne viser at (lenke) prosent av 
# omsetningen omfatter varer som koster mellom $(lenke)og(lenke). Det er størst antall salg av varer i prisgruppen fra 
# $(lenke) til $(lenke). Vi finner (lenke til varer) i denne gruppen.
# Figur 1 viser hvordan omsetningen fordeler seg på prisgruppene.

# Den totale fortjeneste (Profit) i uke 20(lenke) var $ `r sum(sales_14_20$Profit`. Størst fortjeneste var det på (lenke). 
# Dette utgjorde (lenke) prosent av den totale fortjenesten.  
# Figur 2 viser hvordan fortjenesten fordeler seg på prisgruppene.
# Denne viser at (lenke) prosent av fortjenesten omfatter varer som koster mellom $(lenke)og(lenke). Den største fortjenesten 
# var det innen prisgruppen fra $(lenke) til $(lenke). Vi finner (lenke til varer) i denne gruppen.

# Hvilke varer i gruppen (lenke til størst gruppe)

Goods_most<- sales_14_20 %>% 
  select(Description, Price, Sold, Profit, Sales) %>% 
  filter(Price > 4 & Price <= 5, Sold > 10) %>% 
  arrange(desc(Sales))

# Her er salget filtrert etter prisgruppe og salg over 10 enheter.
# Vi får da følgende tabell:
Goods_most[1,2]




#mutate(test, "Other" == sum("other"))

#sum(test$Sales) %in% test$price_class = "Other"



# library(data.table)
# library(janitor)
# 
# test_t <- transpose(test)
# 
# rownames(test_t) <- colnames(test)
# colnames(test_t) <- rownames(test)
# 
# 
# 
# test_t %>% 
#   as_tibble 
# 
# sapply(test_t, class)
# 
# 
# as.numeric(test_t$"1")
# 
# glimpse(test_t)
# 
# row_to_names(test_t, 1, remove_rows_above = TRUE)
# test_t %>% 
#   mutate_if(is.character, as.factor)



  # OPPGAVE 3:
# Dataene skal benyttes til en månedlig salgsrapport på aggregert nivå til konsernledelsen. 
# Gi noen eksempler på hva innholdet i en slik langsiktig konsernrapport bør inneholde. 
# Begrunn dine valg og tankegangen bak figurer og eventuelle tabeller.
# Gjelder alle utsalg
# Månedsdata - lage data for alle mnd slik at man kan se utvikling, totalt og hvert utsalg. Grafikk
# Omsetning totalt utvikling(Sales) Grafikk
# Hvilke varegrupper gir mest/minst profitt
# Omsetning pr varegruppe (INV_NUMBER). Grafikk
## slå sammen varegruppe, evt andel av omsetning
# Hvor mye utgjør hver varegruppe av salget. Grafikk
# Hvem er kunder og hva spiser de (koble demogrupper og varegrupper) - slå sammen 
# Belyse om det er sammenheng mellom ledighet i området og omsetning
# Belyse om det er sammenheng mellom demo i området og omsetning
# Belyse om det er sammenheng mellom crime i området og omsetning

# I choose 4 weeks from 20-23:
sales_mnd <-All_df %>%
  filter(Weather_Week >= 20, Weather_Week <= 23) %>%
  #select(Store_Name, Store_Num, Store_City, County_Name, Date, INV_NUMBER,Description, Price, Sold,
         #Sales, Tot_Sls, Unit_Cost, Cost,Cost_Percent, Margin, Profit) #%>%
  group_by(INV_NUMBER, Description, Price, Sold, Cost, Profit, Margin) %>% 
  summarise(Sales) %>% 
  ungroup()

sales_mnd

# sum of variables:
sum(sales_mnd$Sales)

sum(sales_mnd$Profit)

# Change from week 19 to week 20:
# sales_change = round(100 * ((sum(sales_14_20$Sales)/sum(sales_14_19$Sales)-1)),1)
# sales_change
# 
# profit_change = round(100 * ((sum(sales_14_20$Profit)/sum(sales_14_19$Profit)-1)),1)
# profit_change


max(sales_mnd$Sales)

# Sorting the goods after price groups:

sales_price_gr_mnd <- sales_mnd %>% 
  group_by(price_group = ifelse(Price <= 1.0, "price_$1", 
                         ifelse(Price > 1 & Price <= 2, "price_$2", 
                         ifelse(Price > 2 & Price <= 3, "price_$3", 
                         ifelse(Price > 3 & Price <= 4, "price_$4", 
                         ifelse(Price > 4 & Price <= 5, "price_$5", 
                         ifelse(Price > 5 & Price <= 6, "price_$6", 
                         ifelse(Price > 6 & Price <= 7, "price_$7", 
                         ifelse(Price > 7 & Price <= 8.0, "price_$8", "price_over_$8"))))))))) %>% 
  summarise(Sold, Price, Sales, Profit)



sales_price_gr_mnd <-
  sales_price_gr_mnd %>%
  select(price_group, Sold, Price, Sales, Profit) %>%
  filter(price_group >= 0) %>%
  group_by(price_group) %>%
  summarise(Tot_sales = sum(Sales), Tot_sold = sum(Sold), Tot_profit = sum(Profit)) 
sales_price_gr_mnd


# Total sales and profit:
sum(sales_price_gr_mnd$Tot_sales)
max(sales_price_gr_mnd$Tot_sales)  

sum(sales_price_gr_mnd$Tot_profit)
max(sales_price_gr_mnd$Tot_profit)

sales_most_mnd <- 
  subset(sales_price_gr_mnd, Tot_sales == max(sales_price_gr_mnd$Tot_sales))
sales_most_mnd
sales_most_mnd[1,1]
sales_most_mnd[1,2]

profit_most_mnd <- 
  subset(sales_price_gr_mnd, Tot_profit == max(sales_price_gr_mnd$Tot_profit))
profit_most_mnd
profit_most_mnd[1,1]
profit_most_mnd[1,4]

# share of sales and profit per pricegroup:
sales_share_mnd <-
  sales_price_gr_mnd %>% 
  mutate(Share_sales_mnd = round(100 *(sales_price_gr_mnd$Tot_sales/sum(sales_price_gr_mnd$Tot_sales)), 1),
         Share_profit_mnd = round(100 *(sales_price_gr_mnd$Tot_profit/sum(sales_price_gr_mnd$Tot_profit)),1)) %>% 
  select(price_group, Tot_sold, Tot_sales, Share_sales_mnd, Tot_profit, Share_profit_mnd)
sales_share_mnd

Shares_most_mnd <- 
  subset(sales_share_mnd, Share_sales_mnd == max(sales_share_mnd$Share_sales_mnd)) 
Shares_most_mnd  

#test <- subset(sales_share, Tot_share_sales == max(sales_share$Share_sales))

# profit_min <- 
#   subset(sales_price_gr, Tot_profit == min(sales_price_gr$Tot_profit))
# profit_min
# profit_min[1,1]
# profit_min[1,4]

# Making a plot of sales per pricegroup:  
figure_3 <-
  sales_price_gr_mnd %>% 
  ggplot(aes(x=price_group, y = Tot_sales))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Tot_sales), vjust= -0.3, size=3.5)+
  labs(title = "Figure 3: Sales per pricegroup in one month", x = "Pricegroups in $", y = "Total Sales per pricegoup",
       caption = "Figure 3 shows how Sales are distributed across pricegroups. For example, price_$5 mean goods that cost from $4 to $5.") +
  theme_classic()
figure_3

#Vi ser av figur 3 at den største omsetningen er av varer i prisgruppen `r sales_most_mnd[1,1]`. Totalomsetningen her er $`r sales_most_mnd[1,2]`. 
# Den første prisgruppen er negativ. Årsaken til dette er at den også inneholder refusjoner mm. 

# Making a plot of shares of sales per pricegroup:  
figure_3a <-
  sales_share_mnd %>% 
  ggplot(aes(x=price_group, y = Share_sales_mnd))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Share_sales_mnd), vjust= -0.3, size=3.5)+
  labs(title = "Figure 3a: Shares of Sales per pricegroup in one month", x = "Pricegroups in $", y = "Share of Sales per pricegoup",
       caption = "Figure 3a shows how shares of Sales are distributed across pricegroups.") +
  theme_classic()
figure_3a

# Making a plot of profit per pricegroup:
figure_4 <-
  sales_price_gr_mnd %>% 
  ggplot(aes(x=price_group, y = Tot_profit))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Tot_profit), vjust= -0.3, size=3.5)+
  labs(title = "Figure 4: Profit per pricegroup", x = "Pricegroups in $", y = "Total Profit per pricegoup",
       caption = "Figure 4 shows how Profit are distributed across pricegroups in one month. For example price_$5 mean goods that cost from $4 to $5.") +
  theme_classic()
figure_4

# Vi ser av figur 4 at den største fortjenesten oppnås av omsetningen i prisgruppen `r profit_most[1,1]`. 
# Den totale fortjenesten her er $`r profit_most[1,4]`. Den første prisgruppen er negativ. Årsaken til dette er at den også inneholder refusjoner mm. som vil 
# påvirke fortjenesten negativt.











# OPPGAVE 4:
# Kan dataene benyttes til å planlegge nye utsalg? Dersom konsernledelsen ønsker å 
# etablere et nytt utsalg, hvordan kan de benytte disse dataene til å finne den beste lokasjonen?
# se analysen under pkt 3
# reproduserbar analyse
# oppdatering av data







####
# For further analysis  
# I select the crime rates from this df  
#   select(County_Name, County_Total_Crime_Rate, County_Violent_Rate, 
#          County_Property_Rate, County_Society_Rate, County_Other_Rate) 
# df2_crime_2





####
# For further analysis
# I use the four largest groups and sum the small groups in to one category "All_Other_Groups". 

# df3_demo_2 <- df3_demo %>% 
#   mutate("All_Other_Groups"= rowSums(df3_demo[ , c("County_Non-Hispanic_Black","County_Non-Hispanic_Asian",
#                               "County_Non-Hispanic_Pacific_Islander", "County_Non-Hispanic_Two_or_more",
#                               "County_Hispanic_Black", "County_Hispanic_Asian", 
#                               "County_Hispanic_Pacific_Islander", "County_Hispanic_Two_or_more")])) %>% 
#   select("County_Name", "County_Total_Census_Pop", "County_Non-Hispanic_White", "County_Non-Hispanic_Native_American", 
#         "County_Hispanic_White", "County_Hispanic_Native_American","All_Other_Groups") 
# 
# 
# test <- df3_demo_2 %>% 
#     mutate(All_Other_Groups_pct = round(100 * (All_Other_Groups/County_Total_Census_Pop),1),
#            County_Non-Hispanic_White_pct = round(100 * (County_Non-Hispanic_White/County_Total_Census_Pop),1),
#            County_Non-Hispanic_Native_American_pct = round(100 * (County_Non-Hispanic_Native_American/County_Total_Census_Pop),1),
#            County_Hispanic_White_pct = round(100 * (County_Hispanic_White/County_Total_Census_Pop),1),
#            County_Hispanic_Native_American_pct = round(100 * (County_Hispanic_Native_American/County_Total_Census_Pop),1),
#            )



#


####
# For further analysis:
# # Selecting the variables that seem to be interesting:
# df6_weather_2 <- df6_weather %>% 
#   select(Weather_Station, Weather_Date, Weather_Week, Weather_Bad_Weather_Week)
# df6_weather_2

