library(tidyverse)
library(haven)
library(countrycode)
df_party <- read.csv("data/df_party_raw.csv")
df_elect <- read.csv("data/df_elect_raw.csv")

## WVS data 
unzip("data/F00011411-Trends_VS_1981_2022_rds_v4_0.zip", exdir = "data/WVS")
wvs <- read_rds("data/WVS/Trends_VS_1981_2022_rds_v4_0.rds")

# ESS data

unzip("data/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.zip", 
      exdir = "data/ESS")
ess <- read.csv("data/ESS/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv")



###### country legacies - based on Kitschelt et al. (1999, p. 39)

# national-accomodative 
nat <- c("Croatia", "Hungary", "Poland", "Slovenia")

# bureaucratic authoritarian 
bur <- "Czech Republic"

# patrimonial 
pat <- c("Estonia", "Latvia", "Lithuania", "Slovakia", "Romania",
         "Bulgaria")


############ politically significant ethnic minorities - based on Rovny (2024, 61)

# countries that do not have significant ethnic
# minorities 
no_minorities <- c("Czech Republic", "Hungary", "Poland")

############# religiosity levels 

# filter countries 
country_list <- c("BGR", "CZE", "EST", "HRV", "HUN",
                  "LTU", "LVA", "POL", "ROU",
                  "SVK", "SVN")

df_wvs <- wvs %>%
  filter(COUNTRY_ALPHA %in% country_list) %>%
  select(COUNTRY_ALPHA, S017, S020, F028) %>%
  mutate(across(c(S017, S020, F028),  ~as.numeric(zap_labels(.))))

rm(wvs)

# filter out missings 
df_wvs <- df_wvs %>%
  filter(F028 >= 1)

# share of respondents attending services at least once or more times per week
# measure based on Tavits and Letki (2014)

df_wvs <- df_wvs %>%
  mutate(religious = if_else(F028 %in% c(1,2), 1, 0))

df_wvs[100:300, c("F028", "religious")]

### 

df_share_wvs <- df_wvs %>%
  group_by(COUNTRY_ALPHA, S020) %>%
  reframe(
    share_rel = sum(religious * S017)/sum(S017)
    )


#ESS 
# filter out missings
df_ess <- ess %>%
  filter(rlgatnd < 76)

summary(df_ess$rlgatnd)

# share of respondents attending services at least once or more times per week

df_ess <- df_ess %>%
  mutate(religious = if_else(rlgatnd %in% c(1,2,3), 1, 0)) 

df_ess[100:300, c("rlgatnd", "religious")]

df_share_ess <- df_ess %>%
  group_by(cntry, essround) %>%
  reframe(
    share_rel = sum(religious * pspwght)/sum(pspwght)
  )

# year variable 

df_share_ess <- df_share_ess %>%
  mutate(
    survey_year = case_when(
     essround == 1 ~ 2002, 
     essround == 2 ~ 2004, 
     essround == 3 ~ 2006, 
     essround == 4 ~ 2008, 
     essround == 5 ~ 2010, 
     essround == 6 ~ 2012, 
     essround == 7 ~ 2014, 
     essround == 8 ~ 2016, 
     essround == 9 ~ 2018, 
     essround == 10 ~ 2020, 
     essround == 11 ~ 2023)
  )

df_share_ess[c("essround", "survey_year")]

# countrylabel iso3
df_share_ess <- df_share_ess %>% 
  mutate(
    iso3 = countrycode(
      cntry, 
      origin = "iso2c",
      destination = "iso3c"
    )
  )



######################## merge variables with MARPOR data
# legacies and minorities

df_party <- df_party %>%
  mutate(legacy = case_when(
    countryname %in% nat ~ "National accomodative",
    countryname %in% bur ~ "Bureaucratic authoritarianism",
    countryname %in% pat ~ "Patrimonial")) %>% 
  mutate(nat_acc = ifelse(legacy == "National accomodative", 1, 0)) %>%
  mutate(ethnic_minorities = ifelse (countryname %in% no_minorities, 0,1))


df_elect <- df_elect %>%
  mutate(legacy = case_when(
    countryname %in% nat ~ "National accomodative",
    countryname %in% bur ~ "Bureaucratic authoritarianism",
    countryname %in% pat ~ "Patrimonial")) %>% 
  mutate(nat_acc = ifelse(legacy == "National accomodative", 1, 0)) %>% 
  mutate(ethnic_minorities = ifelse (countryname %in% no_minorities, 0,1))

# religion 

# ESS election merge

merge_elect <- df_elect %>%
  left_join(
    df_share_ess,
    by = "iso3",
    relationship = "many-to-many") %>%
  mutate(year_diff = year - survey_year) %>%
  mutate(year_diff =case_when(
    year_diff < 0 ~ year_diff - 0.1, # slightly penalize surveys after an election
    TRUE ~ year_diff)) %>%
  mutate(abs_diff = abs(year_diff)) %>%
  group_by(countryname, year) %>%
  filter(abs_diff == min(abs_diff))%>%
  select(-c(X))



# ESS party merge

merge_party <- df_party %>%
  left_join(
    df_share_ess,
    by = "iso3",
    relationship = "many-to-many") %>%
  mutate(year_diff = year - survey_year) %>%
  mutate(year_diff =case_when(
    year_diff < 0 ~ year_diff - 0.1, # slightly penalize surveys that take place after an election
    TRUE ~ year_diff)) %>%
  mutate(abs_diff = abs(year_diff)) %>%
  group_by(countryname, year, party) %>%
  filter(abs_diff == min(abs_diff)) %>%
  select(-X)






# add variable indicating if a country has 
# significant ethnic minorities, national accomodative communism
# or none 
merge_elect <- merge_elect %>%
  mutate(minority_legacy = case_when(
    ethnic_minorities == 1 ~ "significant ethnic minority",
    legacy == "National accomodative" & ethnic_minorities == 0 ~ "national accomodative & homogeneous",
    TRUE ~ "non national accomodative & homogeneous")
  )
  
df_elect[68:82,c("ethnic_minorities", "legacy", "minority_legacy")]
    

merge_party <- merge_party %>%
  mutate(minority_legacy = case_when(
    ethnic_minorities == 1 ~ "significant ethnic minority",
    legacy == "National accomodative" & ethnic_minorities == 0 ~ "national accomodative & homogeneous",
    TRUE ~ "non national accomodative & homogeneous")
  )


# export 
file_out <- "data/df_elect_cl.csv"
write.csv(merge_elect, file_out, row.names = FALSE)

file_out_party <- "data/df_party_cl.csv"
write.csv(merge_party, file_out_party, row.names = FALSE)







