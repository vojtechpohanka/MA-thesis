library(tidyverse)
CEE_party <- read.csv("data/CEE_data.csv")
View(CEE_party)

Pop <- read.csv("data/Populist.csv", sep= ";")
countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")
head(Pop)

#selecting relevant variables from PopuList dataset
CEE_far<- Pop %>% 
  dplyr::filter(country_name %in% countries & farright == 1) %>% 
  select(c(1:4, farright:farright_bl, partyfacts_id)) %>%
  transform(partyfacts_id = as.numeric(partyfacts_id))

#linking Populist and Marpor
#file_name <- "partyfacts-mapping.csv"
#if( ! file_name %in% list.files()) {
 # url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  #download.file(url, file_name)
#}
#partyfacts_raw <- read_csv(file_name, guess_max = 50000)
#partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

 #link datasets (select only linked parties)
#dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
#dataset_2 <- partyfacts |> filter(dataset_key == "populist")
#link_table <-
 # dataset_1 |>
 # inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))
 #results into file with dataset names in file name
#file_out <- "data/partyfacts-linked.csv"
#write_csv(link_table, file_out)

link_table <- read.csv("data/partyfacts-linked.csv")

IDs<- link_table %>% 
  select(dataset_party_id.x, partyfacts_id) 

IDs <- transform(IDs, dataset_party_id.x = as.numeric(dataset_party_id.x))
sapply(IDs, mode)
nrow(IDs)
IDs <- distinct(IDs, dataset_party_id.x, .keep_all = TRUE)
head(IDs)



CEE_far<- left_join(CEE_far, IDs, by = join_by(partyfacts_id == partyfacts_id))
CEE_full <- left_join(CEE_party, IDs, by = join_by(party == dataset_party_id.x), relationship= "many-to-one")


## code far-right parties based on Populist 


CEE_faral <- CEE_far %>% 
  dplyr::filter(farright_start==1900 & farright_end ==2100)


## parties where MARPOR id matched with Partyfacts
CEE_faral_clean <- CEE_faral %>% 
  filter(!is.na(dataset_party_id.x)) 

View(CEE_faral_clean)

## manualy code unmatched parties
CEE_faral_res <- CEE_faral %>% 
  filter(is.na(dataset_party_id.x)) 


## code RRPs that where only  radical right for a given time period
CEE_farp <- CEE_far %>%
  filter(!farright_start==1900|!farright_end==2100)%>% 
  View()


CEE_coded <- CEE_full %>% 
 mutate(farright = case_when(party %in% CEE_faral_clean$dataset_party_id.x ~ 1,
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
                             (party == 86421 & year >= 2010) ~ 1,# Fidesz
                             (party == 81711 & year <= 2000) ~ 1, #Croatian Democratic Union
                             (party == 86710 & year <= 2018) ~ 1, #Jobbik
                             (party == 86810 & year >= 2019) ~ 1, #Independent Smallholders’ Party
                             (party == 92713 & year <= 2010) ~ 1, #League of Polish Families
                             (party == 92436 & year >= 2005) ~ 1,# PIS
                             (party == 97330 & year >= 2015) ~ 1, #Slovenian Democratic Party
                             is.na(partyfacts_id) ~0,
                             TRUE ~ 0))

length(CEE_faral_clean$dataset_party_id.x)

#Check for remaining uncoded farright coalitions
CEE_coded %>% 
  filter(parfam == 70 & farright==0)%>% 
  distinct(party, .keep_all = TRUE)%>% 
  select(c(1:11,farright))%>%
  View()

##recode with added coalitions
CEE_coded <- CEE_full %>% 
  mutate(farright = case_when(party %in% CEE_faral_clean$dataset_party_id.x ~ 1,
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
                              (party == 86421 & year >= 2010) ~ 1,# Fidesz
                              (party == 81711 & year <= 2000) ~ 1, #Croatian Democratic Union
                              (party == 86710 & year <= 2018) ~ 1, #Jobbik
                              (party == 86810 & year >= 2019) ~ 1, #Independent Smallholders’ Party
                              (party == 92713 & year <= 2010) ~ 1, #League of Polish Families
                              (party == 92436 & year >= 2005) ~ 1,# PIS
                              (party == 97330 & year >= 2015) ~ 1, #Slovenian Democratic Party
                              is.na(partyfacts_id) ~0,
                              TRUE ~ 0))

CEE_coded%>% 
  filter(farright==1)%>%
  filter(is.na(pervote))%>% 
  View()

file_out2 <- "data/CEE_coded.csv"
write_csv(CEE_coded, file_out2)
#Create smaller dataset for analysis 


CEE_coded <- read.csv("data/CEE_coded.csv")



CEE_coded %>% 
  select(c(1:11,pervote,year, farright, multi,nat, law, cult_total))
  
CEE_final <- CEE_coded %>% 
  select(c(1:11,pervote,year, farright, multi,nat, law, cult_total))%>%
  # Group by country and election to get vote share per country-election
  group_by(Country, date) %>%
  # Calculate the sum of vote shares for far-right parties
  mutate(far_right_vote = sum(pervote[farright == 1], na.rm = TRUE)) %>%
  ungroup() %>%
  # Create the lagged vote share by country
  group_by(Country, party) %>%
  mutate(lagged_far_right_vote = lag(far_right_vote)) %>%
  ungroup() %>%
  #Create lagged cult_total variable 
  group_by(Country,party) %>% 
  mutate(cult_total_lag = lag(cult_total))%>%
  mutate(multi_lag = lag(multi))%>% 
  mutate(nat_lag = lag(nat))%>% 
  mutate(law_lag = lag(law))%>%
  ungroup()

file_out3 <- "data/CEE_final.csv"
write_csv(CEE_final, file_out3)
## Preliminary model 
#install.packages("fixest")
library(fixest)

model<-  feols(cult_total ~ lagged_far_right_vote + cult_total_lag | party, data = CEE_final)
summary(model)

