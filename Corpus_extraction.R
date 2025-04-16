library(manifestoR)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(ggpubr)
library(stargazer)
library(ggpubr)

#CEE <- read.csv("data/CEE_data.csv")
#mp_setapikey("manifesto_apikey.txt")
mp_load_cache(file = "data/manifesto_cache.RData")

mp_use_corpus_version("2024-1")


#Eastern Europe
countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")

#Western Europe
countries_west <- c(11:14, 21:23, 31, 41, 42, 43, 51, 53)

available <- mp_availability(countryname %in% countries, cache = TRUE) %>% print


?mp_availiability()


corpus <- mp_corpus(CEE) #%>%
  #as.data.frame(with.meta = TRUE) %>%
  #corpus(docid_field = "manifesto_id", unique_docnames = FALSE) %>%
  #docvars(field = "cmp_code") 


corpus1 <- mp_corpus(countryname %in% countries) %>%
  recode_v5_to_v4() %>% # recode categories for comparability over time
  recode_cee_codes() %>% # recode CEE categories to main coding scheme
  as.data.frame(with.meta = TRUE) 
  # %>% corpus(docid_field = "manifesto_id", unique_docnames = FALSE) %>%
  #docvars(field = "cmp_code") 


#file_out <- "data/Corpus_CEE.csv"
#write_csv(corpus1, file_out)



corpus2 <- mp_corpus(country %in% countries_west) %>%
  recode_v5_to_v4() %>% # recode categories for comparability over time
  as.data.frame(with.meta = TRUE)

# subset out categories potentially related to cultural issues 
national_way <- c("601", "602")
morality <- c("603", "604")
law <- c("605")
multi <- c("607", "608")
equality <- "503" # Equality: Positive
minority <- "705" #underpriviliged minority groups
nonecon_d <- "706" #non-economic demographic groups

corpus1 <- read.csv("data/Corpus_CEE.csv")


corpus1$year <- corpus1$date %/% 100

manifesto_table <- corpus1 %>% 
  distinct(manifesto_id, .keep_all = TRUE) %>% 
  group_by(language, year)%>%
  summarise(count = n(), .groups = NULL) 



corpus_cult <- corpus1 %>% 
  filter(cmp_code %in% c(national_way, morality, law, multi,
                         equality, minority, nonecon_d))

#file_out <- "data/Corpus_cult1.csv"
#write_csv(corpus_cult1, file_out)

corpus_cult2 <- corpus2 %>% 
  filter(cmp_code %in% c(national_way, morality, law, multi,
                         equality, minority, nonecon_d))

#file_out <- "data/Corpus_cult2.csv"
#write_csv(corpus_cult2, file_out)

#corpus description
corpus_cult <- read.csv("data/Corpus_cult1.csv")
corpus_cult$year <- corpus_cult$date %/% 100
summary(corpus_cult$year)

prop.table(table((corpus_cult$cmp_code)))



year_counts <- corpus_cult %>%
  group_by(year) %>%
  summarise(Count = n(), .groups = "drop" )

ggplot(year_counts, aes(year, Count)) +
  geom_line() +
  labs(x= "Year", y= "Count")+
  theme_pubr()
ggsave("plots/sentence_number.jpg",  width = 10, height = 5.625, units = "in")



manifesto_counts <- corpus_cult %>%
  group_by(year) %>%
  summarise(Count = n_distinct(manifesto_id),
            .groups = "drop")

ggplot(manifesto_counts, aes(year, Count)) +
  geom_line() +
  labs(x= "Year", y= "Count")+
  theme_pubr()
ggsave("plots/manifesto_number.jpg",  width = 10, height = 5.625, units = "in")







year_counts <- corpus_cult %>%
  group_by(year, cmp_code) %>%
  summarise(Count = n(), .groups = "drop" )

decades <- c(
  min(year_counts$year, na.rm = TRUE),
  1999,
  2009,
  2019,
  max(year_counts$year, na.rm = TRUE)
)

year_counts$decades <- cut(
  year_counts$year, 
  breaks = decades,
  include.lowest = TRUE,
  labels = c("1990s", "2000s", "2010s", "2020s")
)

year_counts_wide <- year_counts %>% 
  pivot_wider(
    names_from = cmp_code,
    values_from = Count,
    names_prefix = "per_"
  )


year_counts_wide <- year_counts_wide %>%
  rowwise %>%
  mutate(
    national_way = sum(per_601, per_602, na.rm = TRUE),
    morality = sum(per_603, per_604, na.rm = TRUE),
    law = per_605,
    multi = sum(per_607, per_608, na.rm = TRUE),
    equality = per_503,
    minority = per_705,
    nonecon_d = per_706)



decade_sums <- year_counts_wide %>%
  group_by(decades) %>%  
  summarise(
    national_way = sum(national_way, na.rm = TRUE),
    morality = sum(morality, na.rm = TRUE),
    law = sum(law, na.rm = TRUE),
    multi = sum(multi, na.rm = TRUE),
    equality = sum(equality, na.rm = TRUE),
    minority = sum(minority, na.rm = TRUE),
    nonecon_d = sum(nonecon_d, na.rm = TRUE)
  )



write.table(decade_sums, "plots/decade_table_cee.txt", row.names = FALSE) 

#########################
#corpus description West

corpus_cult2 <- read.csv("data/Corpus_cult2.csv")
corpus_cult2$year <- corpus_cult$date %/% 100


prop.table(table((corpus_cult2$cmp_code)))


year_counts2 <- corpus_cult2 %>%
  group_by(year) %>%
  summarise(Count = n(), .groups = "drop" )

ggplot(year_counts2, aes(year, Count)) +
  geom_line() +
  labs(x= "Year", y= "Count")+
  theme_pubr()
ggsave("plots/manifesto_number_west.jpg",  width = 10, height = 5.625, units = "in")


year_counts2 <- corpus_cult2 %>%
  group_by(year, cmp_code) %>%
  summarise(Count = n(), .groups = "drop" )

decades <- c(
  1970,
  1979,
  1989,
  1999,
  2009,
  2019,
  max(year_counts2$year, na.rm = TRUE)
)

year_counts2$decades <- cut(
  year_counts2$year, 
  breaks = decades,
  include.lowest = TRUE,
  labels = c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")
)

year_counts_wide2 <- year_counts2 %>% 
  pivot_wider(
    names_from = cmp_code,
    values_from = Count,
    names_prefix = "per_"
  )


year_counts_wide2 <- year_counts_wide2 %>%
  rowwise %>%
  mutate(
    national_way = sum(per_601, per_602, na.rm = TRUE),
    morality = sum(per_603, per_604, na.rm = TRUE),
    law = per_605,
    multi = sum(per_607, per_608, na.rm = TRUE),
    equality = per_503,
    minority = per_705,
    nonecon_d = per_706)



decade_sums2 <- year_counts_wide2 %>%
  group_by(decades) %>%  
  summarise(
    national_way = sum(national_way, na.rm = TRUE),
    morality = sum(morality, na.rm = TRUE),
    law = sum(law, na.rm = TRUE),
    multi = sum(multi, na.rm = TRUE),
    equality = sum(equality, na.rm = TRUE),
    minority = sum(minority, na.rm = TRUE),
    nonecon_d = sum(nonecon_d, na.rm = TRUE)
  )



write.table(decade_sums2, "plots/decade_table_west.txt", row.names = FALSE) 
















