library(tidyverse)
library(countrycode)
##install.packages("manifestoR")
library(manifestoR)
setwd("C:/Users/voyta/Desktop/Thesis")
mp_setapikey("manifesto_apikey.txt")

#mp_load_cache(file = "manifesto_cache.RData")



## Downloading the main dataset
mpds <- mp_maindataset()
names(mpds)
view(mpds)

## Downloading documents from the manifesto corupus 
mp_availability(TRUE) ## check the availability of documents 
available_docs <- mp_availability(countryname %in% filtered_countries) ## create df showing summary of available documents 
available_docs2 <- mp_availability(countryname %in% filtered_countries) %>% 
  filter(date > 199000) %>%  
  filter(annotations = TRUE)
view(available_docs)

filtered_countries <- c("Sweden", "Norway", "Finland", "Iceland", "Netherlands", "Luxembourg", "Italy", "Spain", "Greece", "Portugal", "Austria", "Switzerland", "Great Britain", "Northern Ireland", "Ireland", "Malta", "Cyprus", "Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia", "France", "Belgium", "Denmark")

A_corpus <- mp_corpus(available_docs)
B_corpus <- mp_corpus(available_docs2) 


print(is.vector(mpds$countryname))
print(is.vector(filtered_countries))

## bulk-downloading documents
my_corpus <- mp_corpus(fra_annotated_docs)##download documents by passing the df with available documents from above to the mp_corpus() function
#mp_corpus(TRUE) ## downloads the whole corpus (takes a while)
## processing the corpus documents 
txt <- content(my_corpus)
head(txt)


#text preprocessing
corpus_cleaned <- tm_map(my_corpus, content_transformer(tolower))
corpus_cleaned  <- tm_map(corpus_cleaned , removePunctuation)
corpus_cleaned  <- tm_map(corpus_cleaned , removeNumbers)
corpus_cleaned  <- tm_map(corpus_cleaned , removeWords, stopwords("english"))
# Stemming
corpus_cleaned <- tm_map(corpus_cleaned , stemDocument)
view(corpus_cleaned)

code_layers(my_corpus)
  
  



## to remove stopwords, translation will be necessary first 
#corpus_nonstop <- tm_map(corpus_cleaned, removeWords, stopwords("english")) 

## create term document matrix 
tdm <- TermDocumentMatrix(corpus_cleaned) 
inspect(tdm)

## viewing original documents 
mp_view_originals(available_docs) ## takes exactly the same arguments as the mp_corpus () function


## caching - for reproducibility and to save unnecessary online traffic, it is advisable to save the cache to the hard drive and work with one cached file throughout the analyses, since the data may change if the database is updated 
mp_save_cache(file = "manifesto_cache.RData")


##exporting documents as a data frame
doc_df <- as.data.frame(my_corpus, with.meta = TRUE)
head(within(doc_df, {
  ## for pretty printing
  text <- paste0(substr(text, 1, 60), "...")
}))

view(doc_df)


## quanteda 
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(tidyr)
library(stringr)

quanteda_corpus <- A_corpus %>%
  as.data.frame(with.meta = TRUE) # %>%
  #corpus(docid_field = "manifesto_id", unique_docnames = FALSE) ## quanteda's corpus function+

write.csv(quanteda_corpus, "Europe_corpus.csv")
view(quanteda_corpus)

post1990 <- B_corpus %>%
  as.data.frame(with.meta = TRUE) # %>%
#corpus(docid_field = "manifesto_id", unique_docnames = FALSE) ## quanteda's corpus function

write.csv(post1990, "Europe_corpus_post1990.csv")
view(post1990)
false<- post1990 %>%
  filter(annotations == FALSE)

view(post1990)
head(post1990)

nrow(post1990$annotations == FALSE)

## cleaning the corpus
frade_corpus <- quanteda_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))  %>%
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)



english_annotated <- mp_availability(TRUE) %>% filter(annotations == TRUE & language == "english")
english_corpus <- mp_corpus(english_annotated)

quanteda_uk <- english_corpus %>%
  as.data.frame(with.meta = TRUE) %>%
  corpus(docid_field = "manifesto_id", unique_docnames = FALSE) ## quanteda's corpus function



corpus1990 <- read.csv("Europe_corpus_post1990.csv")
view(corpus1990)




