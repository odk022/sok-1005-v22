library(tidyverse)
library(lubridate)

# downloading data

# I change the names of the datatables:
# AppWichStoreAttributes = df1_attr
# county_crime = df2_crime
# county_demographic = df3_demo
# county_employment = df4_em
# weekly_sales_10stores = df5_sales
# weekly_weather = df6_weather

# Store Attributes:
# I load the file AppWichStoreAttributes and call it df1_attr. I change the names
# of Store_County and Store_Weather_Station.

df1_attr <- read_csv("AppWichStoreAttributes.csv")
df1_attr_2 <-df1_attr %>%
  as_tibble() %>%
  rename(County_Name = Store_County, Weather_Station = Store_Weather_Station)
df1_attr_2


# Crime rate:
# I load the file county_crime and call it df2_crime:
df2_crime <- read_csv("county_crime.csv")
df2_crime %>% 
  as_tibble() 
  
# I select the crime rates from this df  
#   select(County_Name, County_Total_Crime_Rate, County_Violent_Rate, 
#          County_Property_Rate, County_Society_Rate, County_Other_Rate) 
# df2_crime_2


# demografic groups:
# I load the file county_demographic and call it df3_demo:

df3_demo <- read_csv("county_demographic.csv")
df3_demo %>% 
  as_tibble() 

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



# Employment:
# I load the file county_employment and call it df4_em:

df4_em <- read_csv("county_employment.csv")
df4_em %>% 
  as_tibble()
 


# Sales:
# I load the file weekly_sales_10stores and call it df5_sales:
# I rename the store number column

# Formatting the date
# Some dates are missing in the Date column, I use the other date columns

df5_sales <- read_csv("weekly_sales_10stores.csv")
df5_sales_2 <- df5_sales %>% 
  as_tibble %>%
  rename(Store_Num = Store_num) %>% 
  mutate(Date = as.Date(with(df5_sales_2, paste(Year, Month, Day,sep="-")), "%Y-%m-%d"))

  
# Removing unnecessary columns:
df5_sales_3 <- 
  subset (df5_sales_2, select = -c(Year, Month, Day)) 

df5_sales_3

# df5_sales_2$Date <-
#   as.Date(df5_sales_2$Date, format = "%d/%m/%Y")


# Weather:
# I load the file weekly_weather and call it df6_weather 
df6_weather <- read_csv("weekly_weather.csv")
df6_weather %>%
  as_tibble

# Formatting the weather date
df6_weather$Weather_Date <- 
  as.Date(df6_weather$Weather_Date, format = "%d/%m/%Y") 

# df6_weather_2 <-  df6_weather %>% 
#   rename(Date_Week = Weather_Date)

df6_weather

# 
# # Selecting the variables that seem to be interesting:
# df6_weather_2 <- df6_weather %>% 
#   select(Weather_Station, Weather_Date, Weather_Week, Weather_Bad_Weather_Week)
# df6_weather_2

# OPPGAVE 1:
# Den første oppgaven er å skrive R kode som slår sammen de 6 datasettene til et stort datasett.
# Du må benytte de variablene som de ulike datasettene har til felles for å gjøre dette. 
# Denne prosessen skal kort dokumenteres og kommenteres.

# I make one df out of alle df:

######
test_1 <- left_join(df1_attr_2, df6_weather, by = "Weather_Station")
test_2 <- left_join(test_1, df2_crime, by = "County_Name")
test_3 <- left_join(test_2, df3_demo, by = "County_Name")
test_4 <- left_join(test_3, df4_em, by = "County_Name")
test_5 <- left_join(test_4, df5_sales_3, by = c("Store_Num", "Weather_Date" = "Date"))

All_df <- test_5 %>% 
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
# Hvor mye utgjør hver varegruppe av salget. Grafikk
# Sammenlikning med samme uke året før og uken før. Grafikk
# Fortjenste pr varegruppe (Profit)



# I choose store no 14:
# name of Store:

#week 14
# sales_14_1 <- df5_sales_3 %>% 
#   filter(Date == '2012-04-01', Store_Num == 14)
# 
# all_df_1 <-All_df %>% 
#   filter(Date == '2012-04-01', Store_Num == 14)

# I choose week 20
sales_14 <-All_df %>% 
  filter(Weather_Week == 20, Store_Num == 14) %>% 
  select(Store_Name, Store_Num, Store_City, County_Name, Date, INV_NUMBER,Description, Price, Sold, 
         Sales, Tot_Sls, Unit_Cost, Cost,Cost_Percent, Margin, Profit) %>% 
  group_by(INV_NUMBER, Description, Price, Sold, Cost, Profit, Margin) %>% 
  summarise(Sales)


# OPPGAVE 3:
# Dataene skal benyttes til en månedlig salgsrapport på aggregert nivå til konsernledelsen. 
# Gi noen eksempler på hva innholdet i en slik langsiktig konsernrapport bør inneholde. 
# Begrunn dine valg og tankegangen bak figurer og eventuelle tabeller.

# Gjelder alle utsalg
# Månedsdata - lage data for alle mnd slik at man kan se utvikling, totalt og hvert utsalg. Grafikk
# Omsetning totalt utvikling(Sales) Grafikk
# Omsetning pr varegruppe (INV_NUMBER). Grafikk
# Hvor mye utgjør hver varegruppe av salget. Grafikk
# Hvem er kunder og hva spiser de (koble demogrupper og varegrupper) - slå sammen 
# Belyse om det er sammenheng mellom ledighet i området og omsetning
# Belyse om det er sammenheng mellom demo i området og omsetning
# Belyse om det er sammenheng mellom crime i området og omsetning




# OPPGAVE 4:
# Kan dataene benyttes til å planlegge nye utsalg? Dersom konsernledelsen ønsker å 
# etablere et nytt utsalg, hvordan kan de benytte disse dataene til å finne den beste lokasjonen?
  


## Til presentasjonen:
# Innovative analytiske grep
# - sammenheng salg og rammebetingelser(f.eks, lokasjon,demo,)
# - hva selger hvor?

