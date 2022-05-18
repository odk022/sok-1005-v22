# Loading necessary packages:
library(tidyverse)
library(lubridate)

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

WEEKLY_SALES_10STORES <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/b963fdd1-0df9-4257-bd62-65256ec9d57c/file_downloaded"
dest <- paste0(file_location, "WEEKLY_SALES_10STORES.csv")
download.file(WEEKLY_SALES_10STORES, dest)

WEEKLY_WEATHER <-
  "https://data.mendeley.com/public-files/datasets/6htjnfs78b/files/b8708b3c-c5e8-456e-83f4-9f23f472a840/file_downloaded"
dest <- paste0(file_location, "WEEKLY_WEATHER.csv")
download.file(WEEKLY_WEATHER, dest)

# Removes all noise since we have finished downloading
rm(list=ls()) 


# I change the names of the datatables:
# AppWichStoreAttributes = df1_attr
# county_crime = df2_crime
# county_demographic = df3_demo
# county_employment = df4_em
# weekly_sales_10stores = df5_sales
# weekly_weather = df6_weather

library(readr)
df1_attr <- AppWichStoreAttributes <- read_csv("AppWichStoreAttributes.csv")
head(df1_attr)

df2_crime <- county_crime <- read_csv("county_crime.csv")
head(df2_crime)

df3_demo <- county_demographic <- read_csv("county_demographic.csv")
head(df3_demo)

df4_em <- county_employment <- read_csv("county_employment.csv")
head(df4_em)

df5_sales <- WEEKLY_SALES_10STORES <- read_csv("WEEKLY_SALES_10STORES.csv")
head(df5_sales)

df6_weather <- WEEKLY_WEATHER <- read_csv("WEEKLY_WEATHER.csv")
head(df6_weather)

# I go through the dfs and make necessary changes:

# df1_attr:
# I change the names of Store_County and Store_Weather_Station.

df1_attr_2 <-df1_attr %>%
  rename(County_Name = Store_County, Weather_Station = Store_Weather_Station)
df1_attr_2


# df2_crime:
# No changes

####
# For further analysis  
# I select the crime rates from this df  
#   select(County_Name, County_Total_Crime_Rate, County_Violent_Rate, 
#          County_Property_Rate, County_Society_Rate, County_Other_Rate) 
# df2_crime_2


# df3_demo:
# No changes 


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



# df4_em:
# No changes
 

# df5_sales:
# I rename the store number column.
# Some dates are missing in the Date column, I use the other date columns

df5_sales_2 <- df5_sales %>% 
  rename(Store_Num = Store_num) %>% 
  mutate(Date = as.Date(with(df5_sales, paste(Year, Month, Day,sep="-")), "%Y-%m-%d"))

# Removing unnecessary columns:
df5_sales_3 <- 
  subset (df5_sales_2, select = -c(Year, Month, Day)) 
df5_sales_3



# df6_weather:

# Formatting the weather date
df6_weather$Weather_Date <- 
  as.Date(df6_weather$Weather_Date, format = "%d/%m/%Y") 
df6_weather

####
# For further analysis:
# # Selecting the variables that seem to be interesting:
# df6_weather_2 <- df6_weather %>% 
#   select(Weather_Station, Weather_Date, Weather_Week, Weather_Bad_Weather_Week)
# df6_weather_2




# OPPGAVE 1:
# Den første oppgaven er å skrive R kode som slår sammen de 6 datasettene til et stort datasett.
# Du må benytte de variablene som de ulike datasettene har til felles for å gjøre dette. 
# Denne prosessen skal kort dokumenteres og kommenteres.

# I make one df out of alle df:

df1_df2 <- left_join(df1_attr_2, df6_weather, by = "Weather_Station")
df1_df3 <- left_join(df1_df2, df2_crime, by = "County_Name")
df1_df4 <- left_join(df1_df3, df3_demo, by = "County_Name")
df1_df5 <- left_join(df1_df4, df4_em, by = "County_Name")
All_df <- left_join(df1_df5, df5_sales_3, by = c("Store_Num", "Weather_Date" = "Date"))

All_df <- All_df %>% 
  rename("Date" = "Weather_Date")
All_df

 
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

# I choose week 20
sales_14 <-All_df %>% 
  filter(Weather_Week == 20, Store_Num == 14) %>% 
  select(Store_Name, Store_Num, Store_City, County_Name, Date, INV_NUMBER,Description, Price, Sold, 
         Sales, Tot_Sls, Unit_Cost, Cost,Cost_Percent, Margin, Profit) %>% 
  group_by(INV_NUMBER, Description, Price, Sold, Cost, Profit, Margin) %>% 
  summarise(Sales) %>% 
  ungroup()



# sum of variables:
sum(sales_14$Sales)
sum(sales_14$Profit)
max(sales_14$Sales)


# Sorting the goods after price groups:

sales_price_gr <- sales_14 %>% 
  group_by(price_group = ifelse(Price <= 0, "price_0", 
                         ifelse(Price > 0 & Price <= 1.0, "price_1", 
                         ifelse(Price > 1 & Price <= 2, "price_2", 
                         ifelse(Price > 2 & Price <= 3, "price_3", 
                         ifelse(Price > 3 & Price <= 4, "price_4", 
                         ifelse(Price > 4 & Price <= 5, "price_5", 
                         ifelse(Price > 5 & Price <= 6, "price_6", 
                         ifelse(Price > 6 & Price <= 7, "price_7", 
                         ifelse(Price > 7 & Price <= 8.0, "price_8", "over_8")))))))))) %>% 
  summarise(Sold, Price, Sales, Profit)



sales_price_gr <-
  sales_price_gr %>%
  select(price_group, Sold, Price, Sales, Profit) %>%
  filter(price_group >= 0) %>%
  group_by(price_group) %>%
  summarise(Tot_sales = sum(Sales), Tot_sold = sum(Sold), Tot_profit = sum(Profit)) 

  sum(sales_price_gr$Tot_profit)
  sum(sales_price_gr$Tot_sales)

figure_1 <-
sales_price_gr %>% 
ggplot(aes(x=price_group, y = Tot_sales))+
geom_bar(stat= "identity", fill = "steelblue") +
theme_classic()
figure_1






# Grouping the sales:
# Extract matching rows with str_detect - not finished
sales_gr <- sales_14[str_detect(sales_14$Description, c("REGULAR", "MINI")), ]  
head(sales_gr)


















#test_14 <- sales_14 %>% 
  #mutate(sales_14, price_1 = sales_14, Price > 0 & Price <= 1.0)











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




#test_p <- pivot_wider(test, names_from = price_class, values_from = c(Price, Sales, Sold))

# test_t %>% 
#   mutate_if(is.character, as.numeric)
# test_t
# sum(test_t[1:6])











glimpse(test_t)

  
  
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

# sales_mnd <-All_df %>%
#   filter(Weather_Week >= 20, Weather_Week <= 23) %>%
#   select(Store_Name, Store_Num, Store_City, County_Name, Date, INV_NUMBER,Description, Price, Sold,
#          Sales, Tot_Sls, Unit_Cost, Cost,Cost_Percent, Margin, Profit) %>%
#   group_by(Store_Num) %>%
#   summarise(Sales)
# 
# sales_mnd



# OPPGAVE 4:
# Kan dataene benyttes til å planlegge nye utsalg? Dersom konsernledelsen ønsker å 
# etablere et nytt utsalg, hvordan kan de benytte disse dataene til å finne den beste lokasjonen?
# se analysen under pkt 3
# reproduserbar analyse
# oppdatering av data




