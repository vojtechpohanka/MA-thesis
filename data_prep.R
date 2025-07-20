###################################################
####### Data preparation ##########################
###################################################

# Party Competition on Cultural Issues in Central and Eastern Europe: Examining Patterns in Issue Salience and Content
# MA thesis by Vojtěch Pohanka 

# Initial MARPOR data manipulation and aggregation of issue categories --------


library(tidyverse)
#install.packages("countrycode")
library(countrycode)
##install.packages("manifestoR")
#install.packages("summarytools")
library(summarytools)
library(manifestoR)
library(ggpubr)



## load MARPOR API - instructions at https://manifesto-project.wzb.eu/tutorials/firststepsmanifestoR
mp_setapikey("manifesto_apikey.txt")

#mp_load_cache(file = "manifesto_cache.RData")
mpds <- mp_maindataset(version = "MPDS2024a")

countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")
countries_2 <- c(11:14, 21:23, 31, 41, 42, 43, 51, 53)

CEE <- mpds %>% 
  filter(countryname %in% countries)


##aggregate categories to ensure comparability overtime 

CEE <- aggregate_pers(CEE, groups = cee_aggregation_relations(),
                       na.rm = FALSE,
                       keep = FALSE,
                       overwrite = names(groups),
                       verbose = TRUE)




##################### Scale party-left right positions
# using logit_rile according to Lowe et al. (2011)

log_rile <- CEE %>% 
  logit_rile()

CEE <- CEE %>% 
  cbind(log_rile)

# Check coding
CEE %>% 
  filter(countryname == "Poland")%>%
  select(partyname, rile, log_rile)


## group countries into regions 
CEE <- CEE %>% 
  mutate(macro_region = case_when(
    countryname %in% c("Croatia", "Slovenia") ~ "South West", 
    countryname %in% c("Bulgaria", "Romania") ~ "South East", 
    countryname %in% c("Czech Republic", "Hungary", "Slovakia", "Poland") ~ "V4",
    countryname %in% c("Latvia", "Lithuania", "Estonia") ~ "Baltics")) %>%
  mutate(year = date %/% 100)



# creating aggregate issue categories -------------------------------------


nonecon_list <- c("foreign", "EU", "fd", "pol", "cor", "envi", 
                  "cult", "edu", "nat_multi", "mor", "law")

df_party <- CEE %>% 
  rowwise() %>%
  mutate(uncod = peruncod,
         econ = sum(c_across(c(per401:per404, per406:per415, per703, per704)), na.rm = TRUE),
         welf = sum(c_across(c(per405, per504, per505, per701, per702)), na.rm = TRUE),
         foreign = sum(c_across(c(per101:per107, per109)), na.rm = TRUE), 
         EU = sum(c_across(c(per108, per110)), na.rm = TRUE),
         fd = sum(c_across(c(per201:per204)), na.rm = TRUE),
         pol = sum(c_across(c(per301:per303, per305)), na.rm = TRUE),
         cor = per304,
         envi = sum(c_across(c(per416,per501)), na.rm = TRUE),
         cult = per502, 
         edu = sum(c_across(c(per506, per507)), na.rm = TRUE),
         nat_multi = sum(c_across(c(per601, per602, per607, per608, per705)), na.rm = TRUE),
         mor = sum(c_across(c(per603, per604)), na.rm = TRUE), 
         law = per605,
         econ_agg = sum(c_across(c(econ, welf)), na.rm = TRUE),
         cult_agg = sum(c_across(c(EU, nat_multi, mor, envi, law)), na.rm = TRUE),
         nat_multi_mor = sum(c_across(c(nat_multi, mor)), na.rm = TRUE),
         non_econ_all = sum(c_across(all_of(nonecon_list))))%>%
  ungroup()

library(lubridate)

df_party <- df_party %>% 
  mutate(year = year(ymd(edate)))


## add iso 3

df_party <- df_party %>%
  mutate(
    iso3 = countrycode(
      countryname,
      origin = "country.name",
      destination = "iso3c"
    ))



# election level data -------------------------------------


df_elect <- df_party %>%
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
          law_a = mean(law, na.rm = T),
          econ_agg_a = mean(econ_agg, na.rm = T),
          cult_agg_a = mean(cult_agg, na.rm = T),
          nat_multi_mor_a = mean(nat_multi_mor, na.rm = T),
          non_econ_all_a = mean(non_econ_all, na.rm = T)) %>% 
          ungroup() %>%
          mutate(decade = paste0(floor(year / 10) * 10, "s") # add decade variable
  )

# add ISO-3 codes and election labels

library(countrycode)

#df_elect <- read.csv("data/df_elect.csv")

df_elect <- df_elect %>%
  mutate(
    iso3 = countrycode(
      countryname,
      origin = "country.name",
      destination = "iso3c"
    ),
    year_short = substring(as.character(year), 3, 4),
    elect_label = paste0(iso3, year_short)
  )

############################  
## save datasets to file
# write.csv(df_elect, "data/df_elect_raw.csv")
# write.csv(CEE, "data/df_party_raw.csv")
##########################################

# Adding country-level variables to the election and party data -----------


library(haven)
library(data.table)
library(fuzzyjoin)


###############################
## load datasets from above
#df_party <- read.csv("data/df_party_raw.csv")
#df_elect <- read.csv("data/df_elect_raw.csv")
########################################


# ESS data

#unzip("data/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.zip", 
      exdir = "data/ESS")
#ess <- read.csv("data/ESS/ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-ESS5e03_5-ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-ESS10SC-ESS11-subset.csv")


unzip("data/ESS11.zip",
      exdir = "data/ESS")

ess <- read_csv("data/ESS/ESS11.csv")


# WB data 

# net migration

unzip("data/P_Data_Extract_From_World_Development_Indicators.zip", 
      exdir = "data/WB")


    ## Exact file name may be different for you
wb <- fread("data/WB/ea800207-ea51-4304-8297-70a5d33a8e78_Data.csv")


# population

unzip("data/wb_pop.zip", 
      exdir = "data/WB_pop")

    ## Exact file name may be different for you
wb_pop <- fread("data/WB_pop/76666be7-44b4-4ff3-b9a0-400df76b17d1_Data.csv")

# gdp 

unzip("data/wb_gdp.zip", 
      exdir = "data/WB_gdp")

  ## Exact file name may be different for you

wb_gdp <- fread("data/WB_gdp/34de5856-5647-43a9-946e-74f98c269533_Data.csv")



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



#ESS 
# filter out missings, select relevant variables
df_ess <- ess %>%
  filter(rlgatnd < 76) %>%
  select(essround, cntry, rlgatnd, pspwght)

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



merge_party <- merge_party %>%
  mutate(minority_legacy = case_when(
    ethnic_minorities == 1 ~ "significant ethnic minority",
    legacy == "National accomodative" & ethnic_minorities == 0 ~ "national accomodative & homogeneous",
    TRUE ~ "non national accomodative & homogeneous")
  )




# Net migration -----------------------------------------------------------
# rename columns 


colnames(wb) <- gsub("^([0-9]{4}).*", "\\1", colnames(wb))

colnames(wb)[colnames(wb) == "Country Name"] <- "countryname"


colnames(wb_pop) <- gsub("^([0-9]{4}).*", "\\1", colnames(wb_pop))

colnames(wb_pop)[colnames(wb_pop) == "Country Name"] <- "countryname"

# long format 

wb_long <- wb %>%
  gather(year, net_migration, `1990`:`2024`)

wb_pop_long <- wb_pop %>%
  filter(`Series Name` == "Population, total") %>%
  gather(year, pop, `1990`:`2024`)


# match country names
unique(wb_long$countryname)
unique(merge_elect$countryname)

wb_long <- wb_long %>%
  mutate(countryname = case_when(
    countryname == "Czechia" ~ "Czech Republic",
    countryname == "Slovak Republic" ~ "Slovakia",
    TRUE ~ countryname)
  )

wb_pop_long <- wb_pop_long %>%
  mutate(countryname = case_when(
    countryname == "Czechia" ~ "Czech Republic",
    countryname == "Slovak Republic" ~ "Slovakia",
    TRUE ~ countryname)
  )


# drop empty rows
wb_new <- wb_long %>%
  arrange(desc(countryname)) %>%
  filter(countryname != "") 

rm(wb_long)
wb_long <- wb_new 
rm(wb_new)

wb_pop_long <- wb_pop_long %>%
  select(countryname, year, pop)

# merge wb data 

wb_merge <- wb_long %>%
  left_join(wb_pop_long,
            by = c("countryname", "year"))

# convert year to numeric 

wb_merge$year_num <- as.numeric(wb_merge$year)

wb_merge <- wb_merge %>%
  mutate(pop_num = as.numeric(pop))

wb_merge[1:300, c("year_num", "year", "pop_num", "pop")]

# impute missing pop values

wb_merge <- wb_merge %>%
  mutate( pop_num = if_else(
    is.na(pop_num), lag(pop_num), pop_num
  ))

wb_merge[1:300, c("pop_num", "pop")]

# standardize migration 

wb_merge <- wb_merge %>%
  mutate(
    net_migration_prop = net_migration/pop_num
  )

wb_merge[1:300, c("net_migration_prop","net_migration","pop_num")]


# check 

sort(unique(wb_merge$countryname)) == sort(unique(merge_elect$countryname))


wb_red <- wb_merge %>%
  select(countryname, year_num, net_migration_prop)


# averaging over election cycles

merge_elect_cycle <- merge_elect %>%
  arrange(countryname, year) %>%
  group_by(countryname) %>%
  mutate(
    start_year = lag(year),
    end_year = year - 1
  ) %>%
  filter(!is.na(start_year))

joined_df <- fuzzy_inner_join(
  wb_red,
  merge_elect_cycle,
  by = c("countryname" = "countryname", "year_num" = "start_year", "year_num" = "end_year"),
  match_fun = list(`==`, `>=`, `<=`)
)


cycle_averages <- joined_df %>%
  group_by(countryname.x, year) %>%
  summarise(
    net_migration_avg = mean(net_migration_prop, na.rm = TRUE),
    .groups = "drop"
  )  %>%
  rename(countryname = countryname.x)


# merge election data
merge_elect_imm <- merge_elect %>%
  left_join( cycle_averages,
             by = c("countryname", "year" = "year"))


# check

merge_elect_imm[merge_elect_imm$countryname == "Romania", c("countryname", "year", "net_migration_avg")]

# merge party data

df_party_cl <- merge_party %>%
  left_join(cycle_averages,
            by = c("countryname", "year" = "year"))

# check

df_party_cl[df_party_cl$countryname == "Romania" & df_party_cl$year > 1990, 
                c("countryname", "year",  "net_migration_avg")]


# gdp 

# rename columns
colnames(wb_gdp) <- gsub("^([0-9]{4}).*", "\\1", colnames(wb))

colnames(wb_gdp)[colnames(wb_gdp) == "Country Name"] <- "countryname"


wb_gdp_long <-  wb_gdp %>%
  gather(year, gdp, `1990`:`2024`)

#match countrynames


wb_gdp_long <- wb_gdp_long %>%
  mutate(countryname = case_when(
    countryname == "Czechia" ~ "Czech Republic",
    countryname == "Slovak Republic" ~ "Slovakia",
    TRUE ~ countryname)
  )


# drop empty rows and select relevant columns
wb_gdp_long <- wb_gdp_long %>%
  arrange(desc(countryname)) %>%
  filter(countryname != "") %>% 
  select(countryname, year, gdp) %>%
  mutate(
    gdp = as.numeric(gdp),
    year = as.numeric(year)
  )

# log gdp 

wb_gdp_long <- wb_gdp_long %>%
  mutate(
    gdp_log = log(gdp)
  )

# merge to the rest of the data

df_elect_cl <- merge_elect_imm %>%
  left_join(wb_gdp_long,
            by = c("year", "countryname"))

# check 

df_elect_cl[df_elect_cl$countryname == "Poland" & df_elect_cl$year >= 1993, 
                c("countryname", "year", "gdp_log", "net_migration_avg")]


# save datasets to file

file_out_party <- "data/df_party_cl.csv"
write.csv(df_party_cl, file_out_party, row.names = FALSE)



# Final export of election-level MARPOR data ------------------------------

file_out <- "data/df_elect_cl.csv"
write.csv(df_elect_cl, file_out, row.names = FALSE)



# Adding in party-level variables  ----------------------------------------

library(arsenal)
library(lubridate)
library(readr)


### load data from above
cee_party <- read.csv("data/df_party_cl.csv")


# Coding radical right parties based on Populist --------------------------

pop <- read.csv("data/Populist.csv", sep= ";")
countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")

#selecting relevant variables from PopuList dataset
cee_far<- pop %>% 
  filter(country_name %in% countries & farright == 1) %>% 
  select(c(1:4, farright:farright_bl, partyfacts_id)) %>%
  transform(partyfacts_id = as.numeric(partyfacts_id))

################link Populist and Marpor based on party facts mapping 
################### the script below follows the tutorial at https://partyfacts.herokuapp.com/download/

file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
 url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

#link datasets (select only linked parties)

dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
dataset_2 <- partyfacts |> filter(dataset_key == "populist")
link_table <-
 dataset_1 |>
 inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))

#save results into file with dataset names in file name

file_out <- "data/partyfacts-linked.csv"
write_csv(link_table, file_out)

link_table <- read.csv("data/partyfacts-linked.csv")

IDs<- link_table %>% 
  select(dataset_party_id.x, partyfacts_id) 

IDs <- transform(IDs, dataset_party_id.x = as.numeric(dataset_party_id.x))
sapply(IDs, mode)
nrow(IDs)
IDs <- distinct(IDs, dataset_party_id.x, .keep_all = TRUE)
head(IDs)



cee_far<- left_join(cee_far, IDs, by = join_by(partyfacts_id == partyfacts_id))
cee_full <- left_join(cee_party, IDs, by = join_by(party == dataset_party_id.x), relationship= "many-to-one")


## code far-right parties based on Populist 
cee_faral <- cee_far %>% 
  dplyr::filter(farright_start==1900 & farright_end ==2100)


## parties where MARPOR id matched with Partyfacts
cee_faral_clean <- cee_faral %>% 
  filter(!is.na(dataset_party_id.x)) 

View(cee_faral_clean)

## manualy code unmatched parties
cee_faral_res <- cee_faral %>% 
  filter(is.na(dataset_party_id.x)) 


## code RRPs that where only  radical right for a given time period
cee_farp <- cee_far %>%
  filter(!farright_start==1900|!farright_end==2100)%>% 
  View()


# manually coded unmatched parties

cee_coded <- cee_full %>% 
  mutate(farright = case_when(party %in% cee_faral_clean$dataset_party_id.x ~ 1,
                              party == 81713 ~ 1, 
                              party == 97522 ~ 1, 
                              party == 92070 ~ 1, #Konfederacja Korwin
                              party == 92711 ~ 1, #Polish Western Union
                              party == 87071 ~ 1, #National Alliance All For Latvia
                              party == 83711 ~ 1, #Estonian National Independence Party 
                              party == 83720 ~ 1, # EKRE
                              party == 81714 ~ 1, # Škoro Movement
                              party == 80062 ~ 1, # Patriotic Front - NFSB and VMRO
                              party == 80071 ~ 1, # United Patriots
                              (party == 81711 & year <= 2000) ~ 1, #Croatian Democratic Union
                              (party == 86710 & year <= 2018) ~ 1, #Jobbik
                              (party == 86810 & year >= 2019) ~ 1, #Independent Smallholders’ Party
                              (party == 92713 & year <= 2010) ~ 1, #League of Polish Families
                              (party == 97330 & year >= 2015) ~ 1, #Slovenian Democratic Party
                              is.na(partyfacts_id) ~0,
                              TRUE ~ 0))


#Check for remaining uncoded farright coalitions
cee_coded %>% 
  filter(parfam == 70 & farright==0)%>% 
  distinct(party, .keep_all = TRUE)%>% 
  select(c(1:11,farright))%>%
  View()

##recode with added coalitions
cee_coded <- cee_full %>% 
  mutate(farright = case_when(party %in% cee_faral_clean$dataset_party_id.x ~ 1,
                              party == 81713 ~ 1, 
                              party == 97522 ~ 1, 
                              party == 92070 ~ 1, #Konfederacja Korwin
                              party == 92711 ~ 1, #Polish Western Union
                              party == 87071 ~ 1, #National Alliance All For Latvia
                              party == 83711 ~ 1, #Estonian National Independence Party 
                              party == 83720 ~ 1, # EKRE
                              party == 81714 ~ 1, # Škoro Movement
                              party == 80062 ~ 1, # Patriotic Front - NFSB and VMRO
                              party == 80071 ~ 1, # United Patriots 
                              party == 87722 ~ 1, # Latvia-Zigerista Party
                              party == 93719 ~ 1, # Romanian Unity Alliance
                              party == 92436 ~ 1, # PIS
                              (party == 86421 & year >= 2010) ~ 1, #FIDESZ 
                              (party == 81711 & year <= 2000) ~ 1, #Croatian Democratic Union
                              (party == 86710 & year <= 2018) ~ 1, #Jobbik
                              (party == 86810 & year >= 2019) ~ 1, #Independent Smallholders’ Party
                              (party == 92713 & year <= 2010) ~ 1, #League of Polish Families
                              (party == 97330 & year >= 2015) ~ 1, #Slovenian Democratic Party
                              is.na(partyfacts_id) ~0,
                              TRUE ~ 0))

cee_coded %>%
  filter(countryname == "Slovakia") %>%
  select(partyname, year, farright) %>%
  View()

file_out <- "data/df_party_coded_fr.csv"
write_csv(cee_coded, file_out)


df_party <- read.csv("data/df_party_coded_fr.csv")


# Coding other party families  ----------------------------------------------


# based on CHES
df_ches <- read.csv("data/1999-2019.csv")


# df for first join

df_ches1<- df_ches %>% 
  filter(eastwest == 0) %>%
  select(year, party_id, family) %>%
  rename(ches_id = party_id,
         year_ches = year)



# link CHES and MARPOR - using party facts linking table

########## download and read Party Facts mapping table
file_name <- "partyfacts-mapping.csv"

if( ! file_name %in% list.files()) {
 url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
 download.file(url, file_name)
}

partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

# link datasets (select only linked parties)
dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
dataset_2 <- partyfacts |> filter(dataset_key == "ches")
link_table_ches <-
  dataset_1 |>
 inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))

# write results into file with dataset names in file name
file_out <- "data/partyfacts-linked_ches.csv"
write_csv(link_table_ches, file_out)

link_table_ches <- read.csv("data/partyfacts-linked_ches.csv")

ches_ids <- link_table_ches %>%
  rename(party = dataset_party_id.x,
         ches_id = dataset_party_id.y) %>%
  select(party, ches_id) %>%
  mutate(
    party = as.numeric(party),
    ches_id = as.numeric(ches_id)
  )

# add ches id to our marpor data

df_party <- df_party %>% 
  left_join(
    ches_ids, 
    by = "party",
    relationship = "many-to-many"
  )


# merge with manifesto data 

df_merge <- df_party %>%
  left_join(
    df_ches1,
    by = "ches_id",
    relationship = "many-to-many") %>%
  mutate(year_diff_ches = year - year_ches) %>%
  mutate(year_diff_ches =case_when(
    year_diff_ches < 0 ~ year_diff_ches - 0.1, # slightly penalize surveys that take place after an election
    TRUE ~ year_diff_ches)) %>%
  mutate(abs_diff_ches = abs(year_diff_ches)) %>%
  group_by(year, party) %>%
  filter(abs_diff_ches == min(abs_diff_ches)
  )


# create linking variable unique to each party election 

df_merge <- df_merge %>%
  mutate(link = paste(party, year, sep = "_")) %>%
  ungroup() %>%
  distinct(link, .keep_all = TRUE) # drop duplicate party election observations

df_party <- df_party %>% 
  mutate(link = paste(party, year, sep = "_")) %>%
  ungroup()

#check
df_merge %>%
  filter(countryname == "Hungary") %>%
  select(partyname, year, link) %>%
  print()


#select only relevant variables in merging dataset

df_merge <- df_merge %>% 
  select(link, family) 

# join the datasets back together 

df_joined <- df_party %>%
  left_join(
    df_merge,
    by = "link"
  ) 

df_uncoded <- df_joined %>%
  filter(is.na(family) & farright == 0)

# 164 parties still left uncoded - 
# code these to the CHES party family using 
# the help of the MARPOR parfam variable 

df_coded1 <- df_uncoded %>%
  mutate(
    family = case_when(  ## code CHES family variable based on the MARPOR parfam variable
      parfam == 10 ~ 7,
      parfam == 20 ~ 6,
      parfam == 30 ~ 5,
      parfam == 40 ~ 3,
      parfam == 50 ~ 4,
      parfam == 60 ~ 2,
      parfam == 70 ~ 1,
      parfam == 80 ~ 11,
      parfam == 90 ~ 8,
      parfam == 95 ~ 9,
      parfam == 98 ~ 9,
      parfam == 999 ~ 9,
      TRUE ~ family
    )
  )

# merge datasets back together

df_joined_clean <- df_joined %>%
  filter(!is.na(family) | farright == 1)

df_combined <- rbind(df_joined_clean, df_coded1)

# export 
#file_out <- "data/df_combined_inter.csv"
#write_csv(df_combined, file_out)

#df_combined <- read.csv("data/df_combined_inter.csv")

## Compare Populist and CHES coding and fix potential miscodings

df_combined %>%
  filter(farright == 1 & family !=1) %>%
  select(countryname, party, partyname, parfam, family, year, link) %>%
  nrow()


# fix miscodings 
party_miscoded <- "97020_2014"
df_combined <- df_combined %>%
  mutate(
    farright = case_when(
      link %in% party_miscoded ~ 0,
      TRUE ~ farright)
  )


# code radical right parties  based on Populist
df_combinedfinal <- df_combined %>%
  mutate(
    family =
      case_when(
        farright == 1 & !(party %in% c(86421, 92436)) ~ 1,
        party %in% c(86421, 92436) ~ 2, ### Code PIS and FIDESZ as conservative
        TRUE ~ family)
  )



### alternate coding with PIS and FIDESZ as radical right

df_combinedfinal <- df_combinedfinal %>%
  mutate(
    family_alt =
      case_when(
        farright == 1 ~ 1,
        TRUE ~ family)
  )



# family labels 

family_labels <- c("RADRT", "CON", "LIB", "CD", "SOC", "RADLEFT", "GREEN",
                   "REG", "NOFAMILY", "AGRARIAN/CENTER")

df_combinedfinal <- df_combinedfinal %>% 
  mutate(
    fam_fact = factor(
      family,
      labels = family_labels
    ),
    fam_fact_alt = factor(
      family_alt,
      labels = family_labels),
    farright = case_when(
      family == 1 ~ 1, 
      TRUE ~ 0),
    farright_alt = case_when(
      family_alt == 1 ~ 1,
      TRUE ~ 0)
  )


df_combinedfinal %>% 
  filter(countryname == "Hungary") %>%
  arrange(desc(nat_multi)) %>%
  select(partyname, partyabbrev, year, nat_multi, log_rile,
         farright, fam_fact, fam_fact_alt)%>%
  View()


# check 

df_combinedfinal[c("family_alt", "fam_fact_alt")]

# export 

#file_out <- "data/df_party_coded.csv"
#write_csv(df_combinedfinal, file_out)



# Adding government participation variable --------------------------------
### the script below follows Hauke Licht's tutorial: 
# https://haukelicht.github.io/merge-cmp-and-pgv-data/

#' If NA, then ...
#'
#' @description Given a scalar value `x`, function replaces it with a specified value if the input is NA
#'
#' @param x a scalar value to be evaluated in \code{is.na(x)}
#'
#' @param then the value to replace \code{x} with if \code{is.na(x)} evaluates to true \code{TRUE}
#'
#' @return if \code{is.na(x)}, then \code{then}, else \code{x}
if_na <- function(x, then) ifelse( is.na(x), then, x )


#' Is distinct?
#'
#' @description Given a dataframe, matrix, list or vector object, function test if the number of unique rows or elements 
#'     equals the total number of rows or elements, respectively.
#'
#' @param x dataframe, matrix, list or vector object
#'
#' @return logical
is_unique <- function(x) {
  if (inherits(x, "data.frame") | is.matrix(x)){
    nrow(pgv_elcs) == nrow(unique(pgv_elcs))
  } else if (is.vector(x) | is.list(x)) {
    length(pgv_elcs) == nrow(length(pgv_elcs))
  } else {
    stop("`x` must be a data.frame, list or vector object")
  }
}

countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")

df_party <- read_csv("data/df_party_coded.csv")

df_party <- df_party %>%
  mutate(
    country_iso3c = countrycode(
      countryname,
      origin = "country.name",
      destination = "iso3c")
  )

cmp_elc_ptys <- df_party %>% 
  select(country_iso3c, edate, party, partyname, partyabbrev) %>% 
  rename_at(-1, ~ paste0("cmp_", .)) %>% 
  unique()

# any party more than one abbreviation?
cmp_elc_ptys %>% 
  group_by(country_iso3c, cmp_edate, cmp_party) %>% 
  summarize(n_abrvs = n_distinct(cmp_partyabbrev)) %>% 
  filter(n_abrvs > 1)


# any party more than one ID?
cmp_elc_ptys %>% 
  group_by(country_iso3c, cmp_edate, cmp_partyname) %>% 
  summarize(n_ids = n_distinct(cmp_party)) %>% 
  filter(n_ids > 1)


cabs <- read_csv("data/view_cabinet.csv")

ptys <- read_csv("data/view_party.csv")


## identify running cabinets 

# for PGV country-election-cabinets, ...
pgv_elcs <- cabs %>%
  # keep select countries
  filter(country_name_short %in% unique(df_party$country_iso3c)) %>%
  # go back further to also retain previous cabs
  filter(ymd(election_date) > ymd("1969-12-31")) %>%
  # get distinct country-election configurations
  select(country_name_short, election_date) %>%
  unique() %>% 
  # get date of next election
  group_by(country_name_short) %>% 
  mutate(next_election_date = lead(election_date)) %>% 
  # add cabinet info
  left_join(cabs, by = c("country_name_short", "election_date")) %>% 
  # keep select columns
  select(
    country_name_short, 
    election_date, next_election_date,
    cabinet_id, cabinet_name, start_date
  ) %>%
  unique() %>% 
  # keep only cabinet last formed from a given election 
  # (NOTE: this is the cabinet ruinning when the next election was held)
  group_by(country_name_short, election_date) %>% 
  filter(start_date == max(start_date)) %>% 
  # (NOTE: the previous step makes rows uniquely identified election dates 
  #  within countries, since only one cabinet per country-election is retained)
  ungroup() %>% 
  # again apply date filter
  filter(next_election_date > ymd("1974-12-31")) %>% 
  # rename
  rename(
    country_iso3c = country_name_short
    , cabinet_start_date = start_date
  ) %>%
  rename_at(-1, ~ paste0("pgv_", .))


is_unique(pgv_elcs)


# join country-elections on (inexact) election dates
ctr_elcs <- pgv_elcs %>% 
  # NOTE: take next election due to differing data logics in CMP and PGV (see explanation above)
  select(country_iso3c, pgv_next_election_date) %>% 
  unique() %>% 
  # get cross-product of elections within countries 
  full_join(
    # join CMP data
    cmp_elc_ptys %>% 
      select(country_iso3c, cmp_edate) %>% 
      unique()
    , by = "country_iso3c"
  ) %>% 
  # compute date difference in days for each CMP-PGV election data pairing 
  # THIS POINT IS KEY!: take next election in PGV data due to differing data logics in CMP and PGV
  mutate(abs_elc_date_diff = abs(pgv_next_election_date - cmp_edate)) %>%
  # at the country CMP-election level (reference data), keep the PGV (next) 
  #   election with the lowest data difference (0 days all but a few instance)
  group_by(country_iso3c, cmp_edate) %>% 
  top_n(1, wt = desc(abs_elc_date_diff)) %>% 
  ungroup()


is_unique(pgv_elcs)


# ensure that no CMP election maps to multiple PGV elections
ctr_elcs %>% 
  group_by(country_iso3c, cmp_edate) %>% 
  summarize(n_pgv_dates = n_distinct(pgv_next_election_date)) %>% 
  filter(n_pgv_dates  > 1)

# ensure that no PGV election maps to multiple CMP elections
ctr_elcs %>% 
  group_by(country_iso3c, pgv_next_election_date) %>% 
  summarize(n_cmp_dates = n_distinct(cmp_edate)) %>% 
  filter(n_cmp_dates  > 1)


ctr_elcs_clean <- ctr_elcs %>% 
  group_by(country_iso3c, pgv_next_election_date) %>% 
  filter(abs_elc_date_diff == min(abs_elc_date_diff))

ctr_elcs_clean %>% 
  group_by(country_iso3c, pgv_next_election_date) %>% 
  summarize(n_cmp_dates = n_distinct(cmp_edate)) %>% 
  filter(n_cmp_dates  > 1)


# inexact (best) matches ? 
ctr_elcs_clean %>% 
  filter(abs_elc_date_diff > 0)


# take country-election link table ...
running_cabinets <- ctr_elcs_clean %>% 
  # and left-join PGV cabinet info (only info of running cabinets matched)
  left_join(pgv_elcs, by = c("country_iso3c", "pgv_next_election_date")) %>% 
  rename(
    pgv_election_date = pgv_next_election_date
    , pgv_running_cabinet_id = pgv_cabinet_id
    , pgv_running_cabinet_name = pgv_cabinet_name
    , pgv_running_cabinet_start_date = pgv_cabinet_start_date
    , pgv_running_cabinet_election_date = pgv_election_date
  ) %>% 
  select(
    country_iso3c
    , cmp_edate
    , pgv_election_date
    , pgv_running_cabinet_name
    , pgv_running_cabinet_id
    , pgv_running_cabinet_start_date
    , pgv_running_cabinet_election_date
  )

# any duplicates?
running_cabinets %>% 
  group_by(country_iso3c, cmp_edate) %>% 
  filter(n_distinct(pgv_running_cabinet_id) != 1)



# define your data dir
data_dir <- "C:/Users/voyta/Desktop/Thesis/data"

# download link table
file_name <- "partyfacts-mapping.csv"
if (!file_name %in% list.files(data_dir)) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file.path(data_dir, file_name))
}

# read link table
ptf <- read_csv(file.path(data_dir, file_name))
# NOTE: on 2019-02-23, this raised some warnings, which can be ignored

# check if all countries covered in party-facts (PTF) data
all(df_party$country_iso3c %in% ptf$country)


# create CMP-PGV party link table
pty_links <- ptf %>% 
  # for select countries
  filter(country %in% df_party$country_iso3c) %>% 
  # take parties CMP IDs
  filter(dataset_key == "manifesto") %>% 
  select(partyfacts_id, country, dataset_party_id) %>% 
  # inner join drops both CMP parties that have no matching PGV ID,
  # and PGV parties for which no matching CMP code exists
  inner_join(
    ptf %>% 
      # parties PGV IDs (where possible)
      filter(dataset_key == "parlgov") %>% 
      select(partyfacts_id, country, dataset_party_id)
    , by = c("partyfacts_id" = "partyfacts_id")
    , suffix = c("_cmp", "_pgv")
  ) %>% 
  filter(country_cmp == country_pgv) %>% 
  select(-country_pgv) %>% 
  rename(
    country_iso3c = country_cmp
    , cmp_party = dataset_party_id_cmp
    , pgv_party_id = dataset_party_id_pgv
    , ptf_party_id = partyfacts_id
  ) %>% 
  mutate_at(3:4, as.integer)


nrow(pty_links)

# is distinct?
is_unique(pty_links)


# any PGV ID matches to multiple CMP parties?
pty_links %>% 
  group_by(pgv_party_id) %>% 
  filter(n_distinct(cmp_party) > 1)

# any CMP ID matches to multiple PGV parties?
pty_links %>% 
  group_by(cmp_party) %>% 
  filter(n_distinct(pgv_party_id) > 1)


# how many matched?
cmp_elc_ptys %>% 
  left_join(pty_links) %>% 
  summarise(
    n = n()
    , n_matched = sum(!is.na(pgv_party_id))
  )



# any CMP party in data matches to no PGV parties?
cmp_elc_ptys %>% 
  left_join(pty_links) %>% 
  group_by(country_iso3c, cmp_edate, cmp_party) %>% 
  filter(n_distinct(pgv_party_id, na.rm = TRUE) < 1)


# any CMP party in data matches to multiple PGV parties?
cmp_elc_ptys %>% 
  left_join(pty_links) %>% 
  group_by(country_iso3c, cmp_edate, cmp_party) %>% 
  filter(n_distinct(pgv_party_id, na.rm = TRUE) > 1)

## add pgv party ID
cmp_w_pgv_ids <- cmp_elc_ptys %>% 
  left_join(pty_links, by = c("country_iso3c", "cmp_party")) %>% 
  select(-ptf_party_id) %>% 
  group_by(country_iso3c, cmp_edate, cmp_party) %>% 
  mutate(cmp_n_pgv_ids = n_distinct(pgv_party_id, na.rm = TRUE)) %>% 
  ungroup()


# create running-cabinet party-level dataset
running_parties <- cabs %>% 
  # select party-cabinet info from original PGV cabinets view
  select(
    country_name_short, cabinet_id,
    party_id, party_name_english, party_name_short,
    caretaker, cabinet_party
  ) %>%
  # add party CMP ID (where exists) from original PGV parties view
  left_join(
    ptys %>% select(party_id, cmp)
    , by = "party_id"
  ) %>%
  # compute cabinet size
  group_by(country_name_short, cabinet_id) %>%
  mutate(cabinet_size = sum(cabinet_party)) %>%
  ungroup() %>% 
  # add prefixes to all but the first column
  rename_at(-1, ~ paste0("pgv_", .)) %>%
  # join only running cabinets
  right_join(
    running_cabinets
    , by = c(
      "country_name_short" = "country_iso3c"
      , "pgv_cabinet_id" = "pgv_running_cabinet_id"
    )
  ) %>% 
  # rename
  rename(
    country_iso3c = country_name_short
    , pgv_running_cabinet_id = pgv_cabinet_id
  ) %>% 
  # select columns
  select(
    # all columns as ordered in `running_cabinets` dataframe
    !!names(running_cabinets)
    # other columns ...
    , pgv_cabinet_size
    , pgv_caretaker
    , pgv_party_name_short
    , pgv_party_name_english
    , pgv_party_id
    , pgv_cabinet_party
    , pgv_cmp
  )

# join PGV party-level data to PGV-ID-enriched CMP country-election-party data
cmp_full_join_pgv <- cmp_w_pgv_ids %>% 
  # add running parties (full outer join!)
  full_join(running_parties, by = c("country_iso3c", "cmp_edate", "pgv_party_id")) 

# inspect an example configuration
cmp_full_join_pgv %>% 
  filter(country_iso3c == "CZE") %>% 
  select(
    country_iso3c, cmp_edate, 
    pgv_running_cabinet_name, pgv_running_cabinet_start_date, 
    cmp_partyabbrev, pgv_party_name_short
  )

# fill-in missing information by inferring from configuration's contexts
cmp_full_join_pgv_filled <- cmp_full_join_pgv %>% 
  # a) take PGV country-election groupings ...
  group_by(country_iso3c, pgv_election_date) %>% 
  #    ... fill-in missing CMP election date (can be inferred from grouping)
  fill(cmp_edate, .direction = "up") %>%
  fill(cmp_edate, .direction = "down") %>%
  #    ... and remove PGV configurations that are completly missing in CMP data (if any)
  filter(!is.na(cmp_edate)) %>% # should be all
  # b) take CMP country-election groupings ...
  group_by(country_iso3c, cmp_edate) %>% 
  #    ... and fill-in missing but inferrable PGV info
  fill(
    pgv_election_date
    , pgv_running_cabinet_start_date
    , pgv_running_cabinet_name
    , pgv_running_cabinet_id 
    , pgv_running_cabinet_election_date
    , pgv_cabinet_size
    , pgv_caretaker
    , .direction = "down"
  ) %>%
  fill(
    pgv_election_date
    , pgv_running_cabinet_start_date
    , pgv_running_cabinet_name
    , pgv_running_cabinet_id 
    , pgv_running_cabinet_election_date
    , pgv_cabinet_size
    , pgv_caretaker
    , .direction = "up"
  ) %>% 
  ungroup()


cmp_full_join_pgv_filled %>% 
  filter(country_iso3c == "CZE") %>% 
  select(
    country_iso3c, cmp_edate, 
    pgv_running_cabinet_name, pgv_running_cabinet_start_date, 
    cmp_partyabbrev, pgv_party_name_short,
    pgv_cabinet_size, pgv_cabinet_party
  )

cmp_full_join_pgv_filled %>% 
  group_by(country_iso3c, cmp_edate, pgv_running_cabinet_start_date) %>%
  mutate(
    postmatch_cabinet_size = n_distinct(ifelse(pgv_cabinet_party == 1, pgv_party_name_short, NA), na.rm = TRUE)
    , flag = pgv_cabinet_size == postmatch_cabinet_size
  ) %>%
  filter(!flag)





# take the filled-in fully-joined dataset
cmp_w_pty_govt_status <- cmp_full_join_pgv_filled %>% 
  # get rid of PGV parties that are non-matching in CMP data
  filter(!is.na(cmp_party)) %>% 
  # infer missing government status by ...
  # ... a) looking within CMP country-election-party configurations, and
  group_by(country_iso3c, cmp_edate, cmp_party) %>% 
  fill(pgv_cabinet_party) %>% 
  # ... b) replace with 0 where still NA
  mutate(pgv_cabinet_party = ifelse(is.na(pgv_cabinet_party), 0, pgv_cabinet_party)) %>% 
  # aggregate at party-level within CMP data
  group_by(
    country_iso3c
    , cmp_edate
    , pgv_running_cabinet_name
    , pgv_running_cabinet_id
    , pgv_running_cabinet_start_date
    , pgv_running_cabinet_election_date
    , pgv_cabinet_size
    , pgv_caretaker
    , cmp_party
    , cmp_partyname
    , cmp_partyabbrev
    , pgv_cabinet_party
  ) %>% 
  # add informative comments
  summarize(
    comment = case_when(
      cmp_n_pgv_ids == 0 ~ "no matching party found within matching ParlGov cabinet configuration"
      , cmp_n_pgv_ids == 1 ~ sprintf(
        "CMP party matches to party %s (%s) within ParlGov cabinet configuration"
        , if_na(pgv_party_name_short, "?")
        , if_na(pgv_party_id, "?")
      )
      , cmp_n_pgv_ids > 1 ~ sprintf(
        "CMP party matches to multiple parties within ParlGov cabinet configuration: %s"
        , paste0(
          sprintf(
            "%s (%s)"
            , if_na(pgv_party_name_short, "?")
            , if_na(pgv_party_id, "?")
          )
          , collapse = ", "
        )
      )
      , TRUE ~ NA_character_
      # in order to aggregate, keep distinct comments (always 1 within grouping)
    ) %>% unique()
  ) %>% 
  ungroup()


cmp_w_pty_govt_status %>%
  filter(is.na(cmp_party))


cmp_gov <- cmp_w_pty_govt_status  %>%
  select(
    "cmp_edate",
    "cmp_party",
    "pgv_cabinet_party",
    "pgv_running_cabinet_name",
  )


df_party_gov <- df_party %>% 
  left_join(
    cmp_gov,
    by = c("edate" = "cmp_edate",
           "party" = "cmp_party")
  )


df_party_gov %>%
  filter(countryname == "Czech Republic") %>%
  select(
    partyname,
    pgv_cabinet_party,
    pgv_running_cabinet_name,
    edate)


# fill in NA running cabinets manually 

# check NAS
df_party_gov %>%
  group_by(countryname, edate) %>%
  filter(is.na(pgv_running_cabinet_name)) %>%
  distinct(edate)%>%
  View()
# only Croatia has relevant NAs, since all other NAs are 
# founding elections after communism 

df_party_gov %>%
  filter(countryname == "Croatia") %>%
  select(partyname, party, pgv_running_cabinet_name,
         pgv_cabinet_party,
         partyabbrev, edate) %>%
  View()

# name running coalition/gov party IDs 
coalition_92 <- c(81711, 81220, 81410, 81712)

gov_95 <- 81711

gov_20 <- 81711



df_party_gov %>%
  filter(countryname == "Croatia") %>%
  distinct(date)

# assign running cabinet parties and cabinet names 
df_party_gov_full <- df_party_gov %>%
  mutate(pgv_cabinet_party = case_when(
    date == 199208 & party %in% coalition_92 ~ 1,
    date == 199510 & party %in% gov_95 ~ 1,
    date == 200001 & party %in% gov_20 ~ 1,
    TRUE ~ pgv_cabinet_party),
    pgv_running_cabinet_name = case_when(
      date == 199208 ~ "Gregurić",
      date == 199510 ~ "Valentić",
      date == 200001 ~ "Mateša",
      TRUE ~ pgv_running_cabinet_name
    )
  )

# check cabinet codes

df_party_gov_full %>%
  filter(countryname == "Croatia") %>%
  select(partyname, party, pgv_running_cabinet_name,
         pgv_cabinet_party,
         partyabbrev, edate) %>%
  View()


df_party_gov_full %>%
  group_by(countryname, edate) %>%
  filter(is.na(pgv_running_cabinet_name)) %>%
  distinct(edate)%>%
  View()


df_party_gov_full %>%
  filter(countryname == "Poland") %>%
  select(pgv_running_cabinet_name) %>%
  View()



# Final export of MARPOR party data ------------------------------------------------------------

write_csv(df_party_gov_full, "data/df_party_coded_gov.csv")




# Preparation of Chapell Hill Expert Survey (CHES) data -----------------


### load CHES datasets

trend <- fread("data/1999-2019.csv")
df17 <- fread("data/CHES_2017.csv")
df24 <- fread("data/CHES_2024.csv")

trend %>% 
  filter(country == "Bul") %>%
  View()

# unify country indicator 

country_no <- unique(trend$country)

country_name <- unique(df24$country[!df24$country%in% c("tur", "nor", "swi", "ice")])

lookup <- data.frame(country_no, cname = c(country_name, NA))

lookup <- lookup %>% 
  mutate(
    cname = case_when(
      country_no == 40 ~ "cyp",
      country_no == 38 ~ NA,
      TRUE ~ cname
    )
  )




trend_fix <- trend %>% 
  left_join(
    lookup,
    by = c("country" = "country_no"),
    relationship = "many-to-one") %>%
  relocate(
    cname, .after = country
  )

table(trend_fix$country, trend_fix$cname, useNA = "always")


trend_fix <- trend_fix %>%
  select(!country) %>%
  rename(country = cname)

head(trend_fix)


trend_short <- trend_fix %>% 
  select(country, party, party_id,
         year, govt, family,
         electionyear, vote, seat, 
         lrgen,lrecon, lrecon_salience,
         galtan, galtan_salience,
         multiculturalism, multicult_salience,
         immigrate_policy, immigrate_salience) %>%
  mutate(
    fam_dummy = case_when(
      family == 1 ~ "RADRT",
      family == 2 ~ "CON",
      family == 3 ~ "LIB",
      family == 4 ~ "CD",
      family == 5 ~ "SOC",
      family == 6 ~ "RADLEFT",
      family == 7 ~ "GREEN",
      family == 8 ~ "REG",
      family == 9 ~ "NOFAMILY",
      family == 10 ~ "CONFESS",
      family == 11 ~ "AGRARIAN/CENTER")
  ) %>%
  select(!family)


df24_short <- df24 %>%
  mutate(year = 2024) %>%
  select(country, party, party_id,
         year,family,
         electionyear, vote, seat, 
         lrgen,lrecon, lrecon_salience,
         galtan, galtan_salience,
         multiculturalism, multicult_salience,
         immigrate_policy, immigrate_salience) %>%
  mutate(
    fam_dummy = case_when(
      family == "radrt" ~ "RADRT",
      family == "con" ~ "CON",
      family == "lib"  ~ "LIB",
      family == "cd" ~ "CD",
      family == "soc" ~ "SOC",
      family == "radleft" ~ "RADLEFT",
      family == "green" ~ "GREEN",
      family == "reg" ~ "REG",
      family == "nofamily" ~ "NOFAMILY",
      family == "confessional" ~ "CONFESS",
      family == "agrarian/center"  ~ "AGRARIAN/CENTER")
  ) %>%
  select(!family)


df17_short <- df17%>%
  mutate(year = 2017) %>%
  select(!immigrate_salience) %>%
  rename(immigrate_salience = immigra_salience) %>% # fix duplicate variable
  select(country, party, party_id,
         year, family, govt,
         electionyear, vote, seat, 
         lrgen,lrecon, lrecon_salience,
         galtan, galtan_salience,
         multiculturalism, multicult_salience,
         immigrate_policy, immigrate_salience) %>%
  mutate(
    fam_dummy = case_when(
      family == "1. radical TAN" ~ "RADRT",
      family == "2. conservative" ~ "CON",
      family == "3. liberal"  ~ "LIB",
      family == "4. Christian-democratic" ~ "CD",
      family == "5. socialist" ~ "SOC",
      family == "6. radical left" ~ "RADLEFT",
      family == "7. green" ~ "GREEN",
      family == "8. regionalist" ~ "REG",
      family == "9. no family" ~ "NOFAMILY",
      family == "10. confessional" ~ "CONFESS",
      family == "11. agrarian/center"  ~ "AGRARIAN/CENTER")
  )  %>%
  select(!family)


trend_24 <- bind_rows(trend_short, df17_short, df24_short)

# map country names for later conversion into ISO codes 

country_map <- c(
  be = "Belgium",
  dk = "Denmark",
  ge = "Germany",
  ger = "Germany",
  gr = "Greece",
  esp = "Spain",
  fr = "France",
  irl = "Ireland",
  it = "Italy",
  nl = "Netherlands",
  uk = "United Kingdom",
  por = "Portugal",
  aus = "Austria",
  fin = "Finland",
  sv = "Sweden",
  swe = "Sweden",
  bul = "Bulgaria",
  cz = "Czech Republic",
  est = "Estonia",
  hun = "Hungary",
  lat = "Latvia",
  lith = "Lithuania",
  pol = "Poland",
  rom = "Romania",
  slo = "Slovakia",
  sle = "Slovenia", 
  cro = "Croatia",
  mal = "Malta",
  cyp = "Cyprus"
)

# create new full country name variable 

trend_24 <- trend_24 %>% 
  mutate(country_full = country_map[country])

table(trend_24$country_full, trend_24$country, useNA = "always")

cee_countries <- c("bul", "cz", "est", "hun", "lat", "lith", "pol", "rom", "slo", "sle", "cro")

trend_24_cee <- trend_24 %>%
  filter(country %in% cee_countries) %>%
  mutate(fam_dummy = case_when(
    party_id == 2607 ~ "RADRT",  # consistent coding of LPR 
    TRUE ~ fam_dummy
  ))



# Immigration salience at survey years ---------------------------------------------------

theme_set(theme_classic2(base_size = 22))

survey_means <- trend_24 %>%
  filter(country %in% cee_countries) %>% 
  group_by(country_full, year) %>% 
  reframe(
    immigrate_mean = 
      mean(immigrate_salience, na.rm = TRUE),
    galtan_mean = 
      mean(galtan_salience, na.rm = TRUE)
  ) %>% 
  mutate(
    iso = countrycode(
      country_full,
      origin = "country.name",
      destination = "iso3c")
  )%>%
  mutate(year_short = substring(as.character(year), 3, 4),
         survey_label = paste0(iso, year_short)
  )


#### export ches data

write_csv(survey_means, "data/ches_elections.csv")
fwrite(trend_24_cee, "data/ches_party.csv")











