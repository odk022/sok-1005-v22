s# Loading necessary packages:
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
  rename("Date" = "Weather_Date") %>% 
  filter(Cost > 0)
  
 

 
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


sales_most_item <-
  subset(sales_14_20, Sales = max(sales_14_20$Sales)) %>% 
  arrange(desc(Sales))
sales_most_item
sales_most_item[1,2]
sales_most_item[1,8]

Profit_most_item <-
subset(sales_14_20, Profit = max(sales_14_20$Profit)) %>% 
  arrange(desc(Profit))
Profit_most_item
Profit_most_item[1,2]
Profit_most_item[1,6]


# sales_most_item <-
#   subset(sales_14_20, Sold = max(sales_14_20$Sold)) %>% 
#   arrange(desc(Sold))
# sales_most_item





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


profit_change = round(100 * ((sum(sales_14_20$Profit)/sum(sales_14_19$Profit)-1)),1)
profit_change


max(sales_14_19$Sales)
max(sales_14_20$Sales)

# Sorting the goods after price groups:

sales_price_gr <- sales_14_20 %>% 
  group_by(price_group = ifelse(Price <= 1.0, "price_below_$1", 
                         ifelse(Price > 1 & Price <= 2, "price_$1", 
                         ifelse(Price > 2 & Price <= 3, "price_$2", 
                         ifelse(Price > 3 & Price <= 4, "price_$3", 
                         ifelse(Price > 4 & Price <= 5, "price_$4", 
                         ifelse(Price > 5 & Price <= 6, "price_$5", 
                         ifelse(Price > 6 & Price <= 7, "price_$6", 
                         ifelse(Price > 7 & Price <= 8.0, "price_$7", "price_over_$8"))))))))) %>% 
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

#

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

# Highest number of items sold
Goods_most<- sales_14_20 %>% 
  select(Description, Price, Sold, Profit, Sales) %>% 
  filter(Sold > 10) %>% 
  arrange(desc(Sold))
Goods_most

# Higest revenue from Sales
Goods_most<- sales_14_20 %>% 
  select(Description, Price, Sold, Profit, Sales) %>% 
  filter(Sold > 10) %>% 
  arrange(desc(Sold))
Goods_most

cost_0 <- sales_14_20 %>% 
  filter(Cost== 0)
cost_0



# Her er salget filtrert etter prisgruppe og salg over 10 enheter.
# Vi får da følgende tabell:
Goods_most[1,2]



 # OPPGAVE 3:
# Gjenta analysen for aggregert nivå

# Hvem er kunder og hva spiser de (koble demogrupper og varegrupper) - slå sammen 
# Belyse om det er sammenheng mellom ledighet i området og omsetning
# Belyse om det er sammenheng mellom demo i området og omsetning
# Belyse om det er sammenheng mellom crime i området og omsetning

# Cleaning the data set for unnecessary variable and :

All_df_rev <- All_df %>% 
  
  # We merge small population groups into one variable:
  mutate("All_Other_Groups"= rowSums(All_df[ , c("County_Non-Hispanic_Black","County_Non-Hispanic_Asian",
                                                 "County_Non-Hispanic_Pacific_Islander", "County_Non-Hispanic_Two_or_more",
                                                 "County_Hispanic_Black", "County_Hispanic_Asian",
                                                 "County_Hispanic_Pacific_Islander", "County_Hispanic_Two_or_more")])) %>%  
  
  # and select the relevant variables:    
  select(Store_Name, Store_Num, Store_City, County_Name, Weather_Station, Date, Weather_Week,    
         Description, Price, Sold,Sales, Tot_Sls, Cost, Margin, Profit, Annual_Rent_Estimate,Store_Location, Store_Drive_Through, 
         Store_Competition_Fastfood, Weather_Bad_Weather_days, County_Total_Crime_Rate, County_Violent_Rate, 
         County_Property_Rate, County_Society_Rate, County_Unemployment_Rate, `County_Non-Hispanic_White`,
         `County_Non-Hispanic_Native_American`, County_Hispanic_White,  County_Hispanic_Native_American, All_Other_Groups, 
         County_Total_Census_Pop, ) %>% 
  
  
  # making percentages of population groups
  mutate(All_Other_Groups_pct = round(100 * (All_Other_Groups/County_Total_Census_Pop),1),
         `County_Non-Hispanic_White_pct` = round(100 * (`County_Non-Hispanic_White`/County_Total_Census_Pop),1),
         `County_Non-Hispanic_Native_American_pct` = round(100 * (`County_Non-Hispanic_Native_American`/`County_Total_Census_Pop`),1),
         `County_Hispanic_White_pct` = round(100 * (`County_Hispanic_White`/County_Total_Census_Pop),1),
         `County_Hispanic_Native_American_pct` = round(100 * (`County_Hispanic_Native_American`/County_Total_Census_Pop),1)
  ) %>% 
  
  # removing the variables we dont use
  select(- c(`County_Non-Hispanic_White`,
             `County_Non-Hispanic_Native_American`, County_Hispanic_White,  County_Hispanic_Native_American, All_Other_Groups, 
             County_Total_Census_Pop)) %>% 
  rename(Week_No = Weather_Week) 


All_df_rev$County_Violent_Rate <-
  round(All_df_rev$County_Violent_Rate,1)




# I choose 4 weeks from 40 to 43:

sales_mnd <-All_df_rev %>% 
  filter(Week_No >= 40, Week_No <= 43) %>% 
  select(Store_Name, Store_Num, Store_City, County_Name, Date,Description, Price, Sold, 
         Sales, Profit) %>% 
  group_by(Description, Price, Sold, Sales,Profit) %>% 
  summarise(Sales, Sold) %>% 
  ungroup()
sales_mnd

# sum of variables:
round(sum(sales_mnd$Sales),0)

round(sum(sales_mnd$Profit),0)


# De totale salgsinntektene for alle utsalgene i denne måneden var $ `r sum(sales_mnd$Sales)`.
# Samlet fortjeneste i perioden var $ `r sum(sales_mnd$Profit)`.


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
  summarise(Description, Sold, Price, Sales, Profit)
sales_price_gr_mnd

# sum pricegroups
price_gr_Sales_mnd <-
  sales_price_gr_mnd %>%
  select(price_group,Description, Sold, Price, Sales, Profit) %>%
  filter(price_group >= 0) %>%
  group_by(price_group) %>%
  summarise(Tot_sales = sum(Sales), Tot_sold = sum(Sold), Tot_profit = sum(Profit)) 
price_gr_Sales_mnd

# sum items
sales_Description_mnd <-
  sales_price_gr_mnd %>%
  select(Description, price_group, Sold, Price, Sales, Profit) %>%
  filter(price_group >= 0) %>%
  group_by(Description, price_group) %>%
  summarise(Tot_sales = sum(Sales), Tot_sold = sum(Sold), Tot_profit = sum(Profit)) 
sales_Description_mnd


# Total sales and profit:
round(sum(price_gr_Sales_mnd$Tot_sales),0)
round(max(price_gr_Sales_mnd$Tot_sales),0) 

round(sum(price_gr_Sales_mnd$Tot_profit),0)
round(max(price_gr_Sales_mnd$Tot_profit),0) 


#Comment  
 # The biggest revenue from Sales this month was from `r sales_most_mnd[1,1]`. 
#This amounted to $`r sales_most_mnd[1,3]` in revenue. The largest profit was from `r profit_most_mnd[1,1]`. Here was the sum $ `r profit_most_mnd[1,1]`.

# amount and shares of Sales and Profit per price group
amount_share_pricegroup_mnd <-
  price_gr_Sales_mnd %>% 
  mutate(Share_sales_mnd = round(100 *(Tot_sales/sum(price_gr_Sales_mnd$Tot_sales)), 1),
         Share_profit_mnd = round(100 *(Tot_profit/sum(price_gr_Sales_mnd$Tot_profit)),1)) %>% 
  select(price_group, Tot_sold, Tot_sales, Share_sales_mnd, Tot_profit, Share_profit_mnd) %>% 
  arrange(desc(Tot_sales))
amount_share_pricegroup_mnd

# Amount sold:
sold_most_pricegroup_mnd <- 
  subset(amount_share_pricegroup_mnd, Tot_sold == max(amount_share_pricegroup_mnd$Tot_sold))
sold_most_pricegroup_mnd

# Amount of Sales
sales_most_pricegroup_mnd <- 
  subset(amount_share_pricegroup_mnd, Tot_sales == max(amount_share_pricegroup_mnd$Tot_sales))
sales_most_pricegroup_mnd

# Share of total Sales
Shares_most_pricegroup_mnd <- 
  subset(amount_share_pricegroup_mnd, Share_sales_mnd == max(amount_share_pricegroup_mnd$Share_sales_mnd)) 
Shares_most_pricegroup_mnd

# Amount of Profit
profit_most_pricegroup_mnd <- 
  subset(amount_share_pricegroup_mnd, Tot_profit == max(amount_share_pricegroup_mnd$Tot_profit))
profit_most_pricegroup_mnd

# Share of total Profit
profit_share_most_pricegroup_mnd <-
  subset(amount_share_pricegroup_mnd, Share_profit_mnd == max(amount_share_pricegroup_mnd$Share_profit_mnd))  
profit_share_most_pricegroup_mnd

#We make a table of amount and shares
amount_shares_pricegroup_mnd <- amount_share_pricegroup_mnd[1:9, ]
#kable(amount_shares_mnd, caption = "Table 4: Amount and shares for Sale and Profit")

# bestselling items in pricegroup
bestselling_pricegroup_item <- 
  sales_Description_mnd %>% 
  filter(price_group%in%sales_most_pricegroup_mnd) %>% 
  arrange(desc(Tot_sold))
bestselling_pricegroup_item

# We make a table of the 10 bestselling items in pricegroup $5
bestselling_pricegroup_item <- bestselling_pricegroup_item[1:10, ]
#kable(bestselling_pricegroup_item, caption = "Table 5: Most sold items in price group $5 ")

# amount and shares of Sales and Profit per item
amount_share_item_mnd <-
  sales_Description_mnd %>% 
  mutate(Share_sales_mnd = round(100 *(Tot_sales/sum(sales_Description_mnd$Tot_sales)), 1),
         Share_profit_mnd = round(100 *(Tot_profit/sum(sales_Description_mnd$Tot_profit)),1)) %>% 
  select(Description, price_group, Tot_sold, Tot_sales, Share_sales_mnd, Tot_profit, Share_profit_mnd) %>% 
  arrange(desc(Tot_sales))
amount_share_item_mnd





# Amount of Sales
sales_most_item_mnd <- 
  subset(amount_share_item_mnd, Tot_sales == max(amount_share_item_mnd$Tot_sales))
sales_most_item_mnd

# Share of total Sales
Shares_most_item_mnd <- 
  subset(amount_share_item_mnd, Share_sales_mnd == max(amount_share_item_mnd$Share_sales_mnd)) 
Shares_most_item_mnd 

# Amount of Profit
profit_most_item_mnd <- 
  subset(amount_share_item_mnd, Tot_profit == max(amount_share_item_mnd$Tot_profit))
profit_most_item_mnd

# Share of total Profit
profit_share_most_item_mnd <-
  subset(amount_share_item_mnd, Share_profit_mnd == max(amount_share_item_mnd$Share_profit_mnd))  
profit_share_most_item_mnd

#We make a table of amount and shares
amount_shares_item_mnd <- amount_share_item_mnd[1:10, ]
#kable(amount_shares_mnd, caption = "Table 4: Amount and shares for Sale and Profit")

# Sales revenue
figure_3 <-
  amount_shares_item_mnd %>% 
  ggplot(aes(x= Description, y = Tot_sales))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Tot_sales/1000), vjust= -0.3, size=3.5)+
  labs(title = "Figure 3: Sales revenue of mostselling items in one month", 
       x = "Item Description", y = "Sales revenue in $1000",
       caption = "Figure 3 shows total revenues from the 10 mostselling items.") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
figure_3

figure_3a <-
  amount_shares_item_mnd %>% 
  ggplot(aes(x= Description, y = Share_sales_mnd))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=Share_sales_mnd), vjust= -0.3, size=3.5)+
  labs(title = "Figure 3a: Sales of mostselling items as shares of total Sales in one month", 
       x = "Item Description", y = "Sales shares per item",
       caption = "Figure 3 shows the 10 mostselling item as shares of total Sale .") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
figure_3a



# Profit as share of total Profit
figure_4 <-
  amount_shares_item_mnd %>% 
  ggplot(aes(x= Description, y = Share_profit_mnd))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label= Share_profit_mnd), vjust= -0.3, size=3.5)+
  labs(title = "Figure 4: Profits of mostselling items as shares of total Profit in one month", 
       x = "Item Description", y = "Profit shares per item",
       caption = "Figure 3 shows the 10 mostselling item as shares of total Profit .") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
figure_4



# making aggregate sum per Store
Store_mnd <- All_df_rev %>% 
  filter(Week_No >= 40, Week_No <= 43) %>% 
   select(Store_Name, Store_Num,Description, Sold, Sales, Profit, Store_Location, Store_Drive_Through, Store_Competition_Fastfood,
          County_Total_Crime_Rate, County_Unemployment_Rate, `County_Non-Hispanic_White_pct`,County_Hispanic_Native_American_pct, 
         County_Hispanic_White_pct,`County_Non-Hispanic_Native_American_pct`, All_Other_Groups_pct) %>% 
  group_by(Store_Num,Store_Name, Store_Location, Store_Drive_Through, Store_Competition_Fastfood,
           County_Total_Crime_Rate, County_Unemployment_Rate, `County_Non-Hispanic_White_pct`,County_Hispanic_Native_American_pct, 
           County_Hispanic_White_pct,`County_Non-Hispanic_Native_American_pct`, All_Other_Groups_pct) %>% 

  summarise(Store_Name_Sold = sum(Sold), Store_Name_Sale = sum(Sales)/1000, Store_Name_Profit = sum(Profit)/1000)

  # making shares
  Store_mnd <- Store_mnd %>% 
  mutate(sold_share = round(100 *(Store_Name_Sold/sum(Store_mnd$Store_Name_Sold)),1),
         sales_share = round(100 *(Store_Name_Sale/sum(Store_mnd$Store_Name_Sale)),1),
         profit_share = round(100 *(Store_Name_Profit/sum(Store_mnd$Store_Name_Profit)),1)) %>% 
  
  # creates the desired order of the variables
  select(Store_Num,Store_Name,Store_Name_Sold, sold_share, Store_Name_Sale,sales_share,
         Store_Name_Profit, profit_share,Store_Location, Store_Drive_Through, Store_Competition_Fastfood,
         County_Total_Crime_Rate, County_Unemployment_Rate, `County_Non-Hispanic_White_pct`,County_Hispanic_Native_American_pct, 
         County_Hispanic_White_pct,`County_Non-Hispanic_Native_American_pct`, All_Other_Groups_pct) %>% 
  rename(other_demo_groups_pct = All_Other_Groups_pct,
         sold = Store_Name_Sold,
         sales = Store_Name_Sale,
         profit = Store_Name_Profit) %>%  
  ungroup()
Store_mnd

sum(Store_mnd$sold)

sum(Store_mnd$sales)
sum(Store_mnd$profit)


amount_shares_store_mnd <- Store_mnd[,1:8 ]

# table

figure_5 <-
  amount_shares_store_mnd %>% 
  ggplot(aes(x= Store_Name, y = sales_share))+
  geom_bar(stat= "identity", fill = "steelblue") +
  geom_text(aes(label=sales_share), vjust= -0.3, size=3.5)+
  labs(title = "Figure 5: Sales shares per Store in one month ", 
       x = "Store name", y = "Sales shares in percent",
       caption = "Figure 5 shows sales share of  total Sale per Store .") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
figure_5

# max of the economic variables
Store_most_sold <- Store_mnd %>% 
  arrange(desc(sold))
Store_most_sold

Store_most_sales <- Store_mnd %>% 
  arrange(desc(sales))
Store_most_sales

Store_most_profit <- Store_mnd %>% 
  arrange(desc(profit))
Store_most_profit


library(mosaic)
reg_line_1 <- lm(County_Total_Crime_Rate ~ sales, data = Store_mnd)
reg_line_1

reg_line_2 <- lm(County_Hispanic_White_pct ~ sales, data = Store_mnd)
reg_line_2

# Looking at the non-economic variables
non_ec_var <- Store_mnd %>% 
  select(Store_Num,Store_Name, sold,sales,profit, Store_Location, Store_Drive_Through) %>% 
  arrange(desc(sales))
non_ec_var

#Comment
#We see that only the store with the highest sales is located as Free Standing. This store also has Store_Drive_Through 
#as one of two in the dataset. These factors can be important framework conditions for localization because they clearly affect sales.

# # Is there any correlation between Sales and other variables

# Preparing the datasett for regression
res_store_mnd <- Store_mnd %>% 
  select(sales,County_Total_Crime_Rate, 
         County_Unemployment_Rate, `County_Non-Hispanic_White_pct`,County_Hispanic_Native_American_pct, 
         County_Hispanic_White_pct,`County_Non-Hispanic_Native_American_pct`, other_demo_groups_pct)
  
res_store <- round(cor(res_store_mnd),2)
res_store

# Table over regression
#Comment

#Summary

# OPPGAVE 4:
# Kan dataene benyttes til å planlegge nye utsalg? Dersom konsernledelsen ønsker å 
# etablere et nytt utsalg, hvordan kan de benytte disse dataene til å finne den beste lokasjonen?
# se analysen under pkt 3
# reproduserbar analyse
# oppdatering av data

















