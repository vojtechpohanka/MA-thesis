library(tidyverse)
#install.packages("arsenal")
library(arsenal)

cee_party <- read.csv("data/df_party_cl.csv")

# Coding radical right parties based on Populist --------------------------

pop <- read.csv("data/Populist.csv", sep= ";")
countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")

#selecting relevant variables from PopuList dataset
cee_far<- pop %>% 
  filter(country_name %in% countries & farright == 1) %>% 
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
#file_name <- "partyfacts-mapping.csv"

#if( ! file_name %in% list.files()) {
 # url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
 # download.file(url, file_name)
#}

#partyfacts_raw <- read_csv(file_name, guess_max = 50000)
#partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

# link datasets (select only linked parties)
#dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
#dataset_2 <- partyfacts |> filter(dataset_key == "ches")
#link_table_ches <-
#  dataset_1 |>
 # inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))

# write results into file with dataset names in file name
#file_out <- "data/partyfacts-linked_ches.csv"
#write_csv(link_table_ches, file_out)

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
      labels = family_labels
    )
  )


# check 

df_combinedfinal[c("family_alt", "fam_fact_alt")]

# export 

file_out <- "data/df_party_coded.csv"
write_csv(df_combinedfinal, file_out)


# Adding government participation variable --------------------------------
### based on Hauke Licht's tutorial: 
# https://haukelicht.github.io/merge-cmp-and-pgv-data/

library(dplyr)
library(lubridate)
library(manifestoR)
library(readr)
library(tidyr)


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
data_dir <- "C:/Users/voyta/Desktop/Thesis"

# download link table
file_name <- "partyfacts-mapping.csv"
if (!file_name %in% list.files(data_dir)) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file.path(data_dir, file_name))


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


#save 
write_csv(df_party_gov, "data/df_party_coded_gov")

df_party_gov <- read_csv("data/df_party_coded_gov")
 

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

# check 

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



# Final export ------------------------------------------------------------

write_csv(df_party_gov_full, "data/df_party_coded_gov")





