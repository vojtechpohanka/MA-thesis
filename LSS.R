#install.packages("LSX")
library(LSX)
library(quanteda)
library(tidyverse)
library(countrycode)
library(manifestoR)
setwd("C:/Users/voyta/Desktop/Thesis")

mp_load_cache(file = "manifesto_cache_LSS.RData")

english_annotated <- mp_availability(TRUE) %>% filter(annotations == TRUE & language == "english")

corp <- mp_corpus(english_annotated)

mp_save_cache(file = "manifesto_cache_LSS.RData")

quanteda_corpus <- corp %>%
  as.data.frame(with.meta = TRUE) %>%
  corpus(docid_field = "manifesto_id", unique_docnames = FALSE) 

toks <- tokens(quanteda_corpus, remove_punct = TRUE, remove_symbols = TRUE, 
               remove_numbers = TRUE, remove_url = TRUE)
dfmt <- dfm(toks) |> 
  dfm_remove(stopwords("en"))

dict <- dictionary(file = "dictionary_culture.yml")

load("data_dictionary_sentiment.RData")

seed <- as.seedwords(data_dictionary_sentiment)
term <- char_context(toks, pattern = dict$culture, p = 0.01)
lss <- textmodel_lss(dfmt, seeds = seed, terms = term, cache = TRUE, 
                     include_data = TRUE, group_data = TRUE)
textplot_terms(lss, highlighted = data_dictionary_LSD2015[1:2])


## predicting polarity of documents - extract document variables and save polarity scores 
dat <- docvars(lss$data)
dat$lss <- predict(lss)
print(nrow(dat))
view(dat)


write.csv(corp, "english_annotated_corpus.csv" )




