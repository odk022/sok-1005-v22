library(tidyverse)

# downloading data

# I change the names of the datatables:
# AppWichStoreAttributes = df1_attr
# county_crime = df2_crime
# county_demographic = df3_demo
# county_employment = df4_em
# weekly_sales_10stores = df5_sales
# weekly_weather = df6_weather

df1_attr <- read_csv("AppWichStoreAttributes.csv")
df1_attr_2 <-df1_attr %>%
  as_tibble() %>%
  rename(County_Name = Store_County)
df1_attr_2

# columns for use?



# I select the crime rates from this df
df2_crime <- read_csv("county_crime.csv")
df2_crime_2 <- df2_crime%>% 
  as_tibble() %>% 
  select(County_Name, County_Total_Crime_Rate, County_Violent_Rate, 
         County_Property_Rate, County_Society_Rate, County_Other_Rate) 
df2_crime_2

# This is the df which combines df1 and df2
df1_df2 <- inner_join(df1_attr_2, df2_crime_2, by = "County_Name")
df1_df2


# demografic groups:
df3_demo <- read_csv("county_demographic.csv")
df3_demo %>% 
  as_tibble() %>% 
  head()

# I use the four largest groups and sum the small groups in to one category "All_Other_Groups". 

df3_demo_2 <- df3_demo %>% 
  mutate("All_Other_Groups"= rowSums(df3_demo[ , c("County_Non-Hispanic_Black","County_Non-Hispanic_Asian",
                              "County_Non-Hispanic_Pacific_Islander", "County_Non-Hispanic_Two_or_more",
                              "County_Hispanic_Black", "County_Hispanic_Asian", 
                              "County_Hispanic_Pacific_Islander", "County_Hispanic_Two_or_more")])) %>% 
  select("County_Name", "County_Total_Census_Pop", "County_Non-Hispanic_White", "County_Non-Hispanic_Native_American", 
        "County_Hispanic_White", "County_Hispanic_Native_American","All_Other_Groups") 


df3_demo_3 <- df3_demo_2 %>% 
    mutate(All_Other_Groups_pct = round(100 * (All_Other_Groups/County_Total_Census_Pop),1))
df3_demo_3 <- df3_demo_2 %>%
    mutate(County_Non-Hispanic_White_pct = round(100 * (County_Non-Hispanic_White/County_Total_Census_Pop),1))

df3_demo_3

#mutate("County_Non-Hispanic_White_pct" = 100 *



# Employment:
df4_em <- read_csv("county_employment.csv")
df4_em %>% 
  as_tibble() %>% 
  head()


# Sales:
# rename the store number column
df5_sales <- read_csv("weekly_sales_10stores.csv")
df5_sales <- df5_sales %>% 
  as_tibble %>% 
  rename(Store_Num = Store_num)
head(df5_sales)

# Some of the rows are lacking Date, so I use the columns "Year", "Month" and "Day" to make a date

df5_sales$Date_test<- 
  as.Date(with(df5_sales,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

# Removes the former date columns and renames the Date_test column:
col_remove <- c("Date", "Year", "Month", "Day")

df5_sales_2 <- df5_sales %>% 
  select(- one_of(col_remove)) %>% 
  rename(Date = Date_test)
df5_sales_2


# Weather:
df6_weather <- read_csv("weekly_weather.csv")
df6_weather %>%
  as_tibble() 
  
# Changing the format of Weather _Date
df6_weather$Weather_Date <- format(as.Date(df6_weather$Weather_Date, format = "%d/%m/%Y"))
  head(df6_weather)

# Selecting the variables that seem to be interesting:
df6_weather_2 <- df6_weather %>% 
  select(Weather_Station, Weather_Date, Weather_Week, Weather_Bad_Weather_Week)
df6_weather_2











