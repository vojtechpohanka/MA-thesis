library(data.table)
library(tidyverse)
library(quanteda)
library(rlang)
library(lubridate)
library(stringr)



df <- fread("data/debate_data.csv")

# keep only relevant variables
df <- df %>%
  select(! left_mentions:wing)


# Regex patterns ------------------------------------------------------------

# define patterns 
pattern_list = list(
  econ = paste0("\\bzaměstnanost\\b|\\bzaměstnat\\b|\\bzaměstnal\\w*\\b|\\bzaměstná\\w*\\b|",
                "\\bmzd\\w*\\b|\\bmezd\\w*\\b|",
                "\\bclo\\b|\\bcla\\b|\\bcelní\\w*\\b|\\bcel\\b|",
                "\\bcena\\b|\\bcen\\b|\\bcenov\\w*\\b|",
                "\\bekonom\\w*\\b|\\bfinan\\w*\\b|\\bdůchod\\w*\\b|",
                "\\bkapitalism\\w*\\b|\\bkapitál\\w*\\b|\\bhospodář\\w*\\b|\\bvýrob\\w*\\b|",
                "\\bdaň\\w*\\b|\\btrh\\w*\\b|\\btrž\\w*\\b|\\bmonopol\\w*\\b"),
  
  minorities = paste0("\\bras\\w*\\b|\\betnick\\w*\\b|\\betnikum\\b|",
                      "\\bvietnam\\w*\\b|\\brom\\b|\\bromští\\b|\\broms\\w*\\b|\\bcikan\\w*\\b|",
                      "\\bmenšin\\w*\\b|\\bminorit\\w*\\b"),
  
  nation = paste0("\\bnárod\\w*\\b|\\bčech\\w*\\b|\\bčeši\\b|\\bčešt\\w*\\b"),
  
  multicult = "\\bmultikult\\w*\\b",
  
  immigration = paste0("\\bmigrant\\w*\\b|\\bimigrant\\w*\\b|",
                       "\\buprchlick\\w*\\b|\\buprchlí\\w*\\b|",
                       "\\bpřistěhoval\\w*\\b|\\bcizin\\w*\\b|", 
                       "\\bhranice\\b|\\bazyl\\w*\\b"),
  
  lgbt = paste0("\\bgay\\w*\\b|\\blesb\\w*\\b|",
                "\\bstejnopohlavní\\b|\\bregistrované partnerství\\b|",
                "\\bmanželství pro všechny\\b"),
  
  gender = paste0("\\bpráva žen\\b|\\bženská práva\\b|",
                  "\\w*feminist\\w*\\b|\\w*feminismus\\b|",
                  "\\w*gender\\w*\\b|\\bpohlaví\\b|",
                  "\\brovnost žen a mužů\\b|\\brovnost mužů a žen\\b|",
                  "\\brovnost pohlaví\\b|\\bgenderov\\w*\\b"),
  
  reproductive = "\\binterrupce\\b|\\bpotrat\\w*\\b|\\bantikoncepc\\w*\\b",
  
  family = "\\brodin\\w*\\b",
  
  religion = paste0("\\bkostelní\\w*\\b|\\bcírkev\\w*\\b|",
                    "\\bbůh\\b|\\bbohoslužb\\w*\\b|",
                    "\\bnábožen\\w*\\b|\\bsvětsk\\w*\\b|",
                    "\\bsekulární\\b|\\bsekularizace\\b|",
                    "\\bkřesťan\\w*\\b|\\bvíra\\b|\\bduchov\\w*\\b"),
  
  islam = paste0("\\bislám\\w*\\b|\\bmuslim\\w*\\b|",
                 "\\bšarí\\w*\\b|\\bmešit\\w*\\b")
)

table <- as.data.frame(pattern_list) %>%
  pivot_longer(cols = everything(),
               names_to = "Issue",
               values_to = "Keywords")



## export dictionary as an illustrative table 

table$Keywords <- gsub("\\\\b|\\\\w", "", table$Keywords)

table$Keywords <- gsub("[\\\\^$.|?+()\\[\\]]", "", table$Keywords)

table$Keywords <- gsub("\\|", " ", table$Keywords)

table$Keywords <- gsub("\\s+", " ", table$Keywords)

table$Keywords <- trimws(table$Keywords)

write.csv(table, "output_final/dictionaries.txt",
          quote = FALSE,
          row.names = FALSE)

#  create helper function to detect mentions - create new columns in 
# the datase that indicate whether a keyword was mentioned or not

detect_mentions <- function(df, text_col, patterns) {
  text_col <- ensym(text_col)
  
  for (name in names(patterns)) {
    df <- df %>%
      mutate(!!paste0("pattern_", name) := as.integer(
        str_detect(!!text_col, regex(patterns[[name]], ignore_case = TRUE))
      ))
  }
  
  return(df)
}

# apply

df_mentions <- df %>%
  detect_mentions(text, pattern_list)


## export dataframe with indicator of mentions
write_csv2(df_mentions, "data/df_mentions.csv")


# Salience calculation ----------------------------------------------------

df_mentions <- fread("data/df_mentions.csv", sep = ";")



## fix party variable

df_mentions <- df_mentions %>%
  mutate(party_fix = case_when(
    party %in% c("SZ (NEZ)", "SZ") ~ "SZ",
    party %in% c("Úsvit (NEZ)", "Úsvit  (NEZ)", "Úsvit") ~ "Úsvit",
    party %in% c("ANO (NEZ)", "ANO") ~ "ANO",
    party %in% c("ODS (NEZ)", "ODS") ~ "ODS",
    party %in% c("ČSSD (NEZ)", "ČSSD") ~ "ČSSD",
    TRUE ~ party
  )
  )




column_names <- df_mentions %>%
  select(pattern_econ:pattern_islam) %>%
  names() 

# calculate monthly salience for each party 
df_sal_party_month <- df_mentions %>%
  group_by(party_fix, month) %>%
  summarise(across(all_of(column_names),
                   ~ sum(.x == 1, na.rm = TRUE) / n(),
                   .names = "{.col}"),
            .groups = "drop") 


# shorten column names
colnames(df_sal_party_month) <- colnames(df_sal_party_month) |>
    gsub(pattern = "pattern_{1}", replacement = "")


write_csv(df_sal_party_month, "data/df_sal_party_monthly.csv")




# calculate yearly salience for each party 
df_sal_party_year <- df_mentions %>%
  group_by(party_fix, year) %>%
  summarise(across(all_of(column_names),
                   ~ sum(.x == 1, na.rm = TRUE) / n(),
                   .names = "{.col}"),
            .groups = "drop") 


# shorten column names
colnames(df_sal_party_year) <- colnames(df_sal_party_year) |>
  gsub(pattern = "pattern_{1}", replacement = "")

write_csv(df_sal_party_year, "data/df_sal_party_yearly.csv")



# aggregate numbers 

df_sal_agg <- df_mentions %>%
  group_by(month) %>%
  summarise(across(all_of(column_names),
                   ~ sum(.x == 1, na.rm = TRUE) / n(),
                   .names = "{.col}_sal"),
            .groups = "drop")

colnames(df_sal_agg) <- colnames(df_sal_agg) |>
  gsub(pattern = "pattern_{1}", replacement = "")

write_csv(df_sal_agg, "data/df_sal_agg.csv")


 





                      
                      

