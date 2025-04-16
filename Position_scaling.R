library(tidyverse)
library(manifestoR)
df <- read.csv("data/CEE_data.csv")
mp_setapikey("manifesto_apikey.txt")

mpds <- mp_maindataset(version = "MPDS2024a")


# logged RILE scale
# right and left components based on RILE 
df<- df %>% 
  mutate(right = (per104 + per201 + per203 + per305 + per401 + per402 + per407 + per414 + per505 + per601 + per603 + per605 + per606),
        left = (per103 + per105 + per106 + per107 + per403 + per404 + per406 + per412 + per413 + per504 + per506 + per701 + per202))

# calculation with logarithmic transformation
df<- df %>% 
  mutate(log_rile = log(right + 0.5) - log(left+0.5))


df %>% 
  filter(countryname == "Czech Republic" & date == 201710) %>%
  reframe(party, log_rile)


## using manifestor R function

logit_rile(subset(mpds, countryname == "Czech Republic" & date == 201710))


## logrile at the document level 

corpus <- mp_corpus(countryname == "Czech Republic" & date == 201710)
mp_scale(corpus, scalingfun = logit_rile)


