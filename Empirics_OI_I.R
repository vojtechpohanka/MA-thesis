library(tidyverse)
library(plm)
library(gplots)
library(tseries)
library(huxtable)

df <- read.csv("data/CEE_data.csv") # party level data


# OIA - econ vs cultural issues  ------------------------------------------


df_elect <- df %>%
  group_by(countryname, year) %>% 
  reframe(uncod_a = mean(uncod, na.rm=T),
          econ_a = mean(econ, na.rm = T), 
          welf_a = mean(welf, na.rm = T), 
          foreign_a = mean(foreign, na.rm=T), 
          EU_a = mean(EU , na.rm=T), 
          fd_a = mean(fd, na.rm = T), 
          pol_a = mean(pol, na.rm = T),
          cor_a= mean(cor, na.rm = T), 
          envi_a = mean(envi, na.rm = T),
          cult_a = mean(cult, na.rm = T), 
          edu_a = mean(edu, na.rm = T), 
          nat_multi_a = mean(nat_multi, na.rm = T),
          mor_a = mean(mor, na.rm = T),
          law_a = mean(law, na.rm = T))


# add election cycle variable 

df_elect <- df_elect 
  group_by(countryname) %>%
  mutate(election_number = row_number()) %>%
  ungroup()
  



# OIA - econ vs cultural issues  ------------------------------------------


# OLS with dummy estimator for countries

d <- glm(nat_multi + mor ~ year + countryname,
         family = "gaussian",
         data = df)

huxreg(d)


# elections level 

m1_ya <- lm(nat_multi_a ~ year + countryname,
          data = df_elect)
m2_ya <- lm(mor_a ~ year + countryname,
              data = df_elect)
 
m3_ya <- lm(envi_a ~ year + countryname,
                data = df_elect)

m4_ya <- lm(EU_a ~ year + countryname,
                 data = df_elect)

export_summs(m1_ya, m2_ya, m3_ya, m4_ya, robust = T,
                model.names = c("Nationalism, multiculturalism, immigration, minorities","Morality",
                                                                              "Environment",
                                                                              "EU"), 
                coefs = "year")


# election number

m1_ea <- lm(nat_multi_a ~ election_number + countryname,
            data = df_elect)
m2_ea <- lm(mor_a ~ election_number + countryname,
            data = df_elect)

m3_ea <- lm(envi_a ~ election_number + countryname,
            data = df_elect)

m4_ea <- lm(EU_a ~ election_number + countryname,
            data = df_elect)

export_summs(m1_ea, m2_ea, m3_ea, m4_ea, robust = T,
             model.names = c("Nationalism, multiculturalism, immigration, minorities","Morality",
                             "Environment",
                             "EU"), 
             coefs = "election_number")
