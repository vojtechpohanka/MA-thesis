library(tidyverse)
library(ggplot2)

setwd("C:/Users/voyta/Desktop/Party competition") # set working directory everytime upon opening a new R session 
df2019 <- read.csv("CHES2019.csv")
dftrend <- read.csv("trend.csv")
df2023 <- read.csv("CHESUkraine.csv")

## East west comparison 2010, 2023

view(dftrend)
dftrendwest2010<- filter(dftrend, year == 2010 & eastwest == 1)

x = dftrendwest2010$lrecon
y = dftrendwest2010$galtan

ggplot(dftrendwest2010,aes(x, y, label = party)) +
  geom_point() +
  geom_smooth(method='lm', mapping = aes(weight = vote), se=FALSE, color='turquoise4') +
  geom_text(hjust=0, vjust=0) +
  theme_minimal() +
  labs(x='lrecon', y='galtan', title='Party positions 2010 West') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

dftrendwest2023 <- filter(df2023, party_id < 1700 | party_id >= 3500)

x = dftrendwest2023$lrecon
y = dftrendwest2023$galtan

ggplot(dftrendwest2023,aes(x, y, label = party)) +
  geom_point() +
  geom_smooth(method='lm', mapping = aes(weight = vote), se=FALSE, color='turquoise4') +
  geom_text(hjust=0, vjust=0) +
  theme_minimal() +
  labs(x='lrecon', y='galtan', title='Party positions 2023 West') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 


view(dftrend)
dftrendeast2010<- filter(dftrend, year == 2010 & eastwest == 0)

x = dftrendeast2010$lrecon
y = dftrendeast2010$galtan

ggplot(dftrendeast2010,aes(x, y, label = party)) +
  geom_point() +
  geom_smooth(method='lm', mapping = aes(weight = vote), se=FALSE, color='turquoise4') +
  geom_text(hjust=0, vjust=0) +
  theme_minimal() +
  labs(x='lrecon', y='galtan', title='Party positions 2010 East') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 


dftrendeast2023 <- filter(df2023, party_id > 1700 & party_id < 3200)

x = dftrendeast2023$lrecon
y = dftrendeast2023$galtan

ggplot(dftrendeast2023,aes(x, y, label = party)) +
  geom_point() +
  geom_smooth(method='lm', mapping = aes(weight = vote), se=FALSE, color='turquoise4') +
  geom_text(hjust=0, vjust=0) +
  theme_minimal() +
  labs(x='lrecon', y='galtan', title='Party positions 2023 East') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 


## Czech trends


dfcz <- filter(df2019, country==21)
head(df2019)

x = dfcz$lrecon
y = dfcz$galtan


ggplot(dfcz,aes(x, y, label = party)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  geom_text(hjust=0, vjust=0) +
  theme_minimal() +
  labs(x='lrecon', y='galtan', title='Party positions 2019') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

df2023 <- read.csv("CHESUkraine.csv")
view(df2023)

dfcz2 <- df2023 %>% 
  filter(country=="Czech Republic")

x = dfcz2$lrecon
y = dfcz2$galtan

ggplot(dfcz2,aes(x, y, label = party)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  geom_text(hjust=0, vjust=0) +
  theme_minimal() +
  labs(x='lrecon', y='galtan', title='Party positions2023') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) 

# changes in competition slope 
#install.packages("broom")
library(broom)

view(dftrend)

## OLS galtan by lrecon, weighed by vote share

result <- dftrend %>% 
  group_by(country, year)  %>% 
  do(tidy(lm(galtan ~ lrecon, data = ., weights = vote)))
?lm


result1 <- result %>%
  filter(term == "lrecon")

view(result1)

print(df2023$country)


## recode countries to match the CHES trend file 

df2023 <- df2023 %>%
  mutate(country_recoded = case_when(
    country == "Belgium" ~ 1,
    country == "Denmark" ~ 2,
    country == "Germany" ~ 3,
    country == "Greece"  ~ 4, 
    country == "Spain"   ~ 5, 
    country == "France"  ~ 6,
    country =="Ireland"  ~ 7, 
    country == "Italy"   ~ 8, 
    country == "Netherlands" ~ 10, 
    country == "United Kingdom" ~ 11, 
    country == "Portugal" ~ 12,
    country == "Austria" ~ 13,
    country == "Finland" ~ 14,
    country == "Sweden"  ~ 16, 
    country == "Bulgaria"   ~ 20, 
    country == "Czech Republic"  ~ 21,
    country =="Estonia"  ~ 22, 
    country == "Hungary"   ~ 23, 
    country == "Latvia" ~ 24, 
    country == "Lithuania" ~ 25, 
    country == "Poland" ~ 26, 
    country == "Romania" ~ 27, 
    country == "Slovakia" ~ 28,
    country == "Slovenia" ~ 29, 
    country == "Croatia" ~ 31, 
    country == "Turkey" ~ 35, 
    country == "Norway"  ~ 35,
    country == "Switzerland" ~ 36, 
    country == "Malta" ~ 37, 
    country == "Luxembourg" ~ 38,
    country == "Cyprus" ~ 40,
    country == "Iceland" ~ 45
  ))

## OLS for 2023
result2023 <- df2023 %>% 
  group_by(country_recoded)  %>% 
  do(tidy(lm(galtan ~ lrecon, data = ., weights = vote)))


result2023c <- result2023 %>%
  filter(term == "lrecon")

result2023c <- result2023c %>%
  rename(country = country_recoded)


class(dftrend$year)

## add year variable

result2023c$year <- as.integer(2023)
result2023c %>% relocate(year, .before = term)


resultstotal <- rbind(result1, result2023c)
resultstotal1 <- resultstotal %>% 
  select(country, year, estimate)

result


resultstable <- resultstotal1 %>%
  pivot_wider(names_from = year, values_from = estimate)

filtered_results <- resultstotal %>%
  filter(year %in% c(2010, 2014, 2019, 2023))

ggplot(filtered_results, aes(x = country, y = abs(estimate), fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge")


resultstable1 <- resultstable %>%
  rename(year_1 = '1999', year_2 = '2002', year_3 = '2006', year_4 = '2010', year_5 = '2014', year_6 = '2019', year_7 = '2023')



resultstable1$substraction <- resultstable1$year_7- resultstable1$year_4
mean(resultstable1$substraction, na.rm = TRUE)

?mean



view(resultstable1)






