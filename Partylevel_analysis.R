library(tidyverse)
library(ggpubr)
library(ggeffects)
library(jtools)
#install.packages("ggExtra")
library(ggExtra)
library(fixest)
library(marginaleffects)
#install.packages("coefplot")
library(coefplot)


party_df <- read.csv("data/df_party_coded.csv")

# add lagged DVs 
party_df <- party_df %>% 
  group_by(party)%>% 
  mutate(nat_multi_lag = lag(nat_multi),
         cult_agg_lag = lag(cult_agg)
         ) %>%
  mutate(fam_fact = factor(fam_fact)) ## convert family to factor

party_df %>% 
  relevel(fam_fact, ref = "RADLEFT")

# dataset exluding radical right 
party_df_exclude <- party_df %>%
  filter(family != 1)

# Radical right vs other parties comparison ------------------------------------------------
# OIIIa 

#aggregate cultural issues
ggplot(party_df, aes(x = fam_fact, y = cult_agg)) +
  geom_boxplot() +
  #geom_point(position = position_jitter(),
  #alpha = 0.3) +
  theme_pubclean() +
  labs(
    x = "family",
    y = "% of quasisentences"
  )

# nationalism, multiculturalism, immigration and minorities 
ggplot(party_df, aes(x = fam_fact, y = nat_multi)) +
  geom_boxplot() +
  theme_pubclean() + +
  labs(
    x = "family",
    y = "% of quasisentences"
  )

# morality
ggplot(party_df, aes(x = fam_fact, y = mor)) +
  geom_boxplot() +
  theme_pubclean() +
  labs(
    x = "family",
    y = "% of quasisentences"
  )


# EU 

ggplot(party_df, aes(x = fam_fact, y = EU)) +
  geom_boxplot() +
  theme_pubclean() +
  labs(
    x = "family",
    y = "% of quasisentences"
  )

## party family regression 

model_agg_family <- feols(cult_agg ~ fam_fact + pervote |factor(country) + year, 
                       data = party_df)

model_multi_family <- feols(nat_multi ~ fam_fact + pervote |factor(country) + year, 
                          data = party_df)

plot_coefs(model_agg_family, model_multi_family) +
  theme_classic2() + 
  scale_y_discrete(labels = function(x) gsub("fam_fact", "", x))



# Salience and left-right positions -------------------------------------
#OIIIb

# OLS for all parties
model_agg_all <- lm(cult_agg ~ log_rile + pervote + factor(country), data = party_df)
model_multi_all <- lm(nat_multi ~ log_rile + pervote + factor(country), data = party_df)

model_multi_all %>% summary()




# excluding radical right 

model_agg_ex <- feols(cult_agg ~ log_rile + pervote |  factor(country) + year, data = party_df_exclude)
model_multi_ex <- feols(nat_multi ~ log_rile + pervote | factor(country) + year, data = party_df_exclude)

model_agg_ex %>% summary()
model_multi_ex %>% summary()

# prediction plot 
pred1 <- ggpredict(model_multi_ex, terms = "log_rile")

ggplot(pred1, aes(x  = x, y = predicted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "left-right", y = "% of quasisentences") +
  theme_pubclean()


# country separate models 

countries <- c("Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", 
               "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")

# Function to run regression for a specific country
run_regression <- function(country_name) {
  model <- feols(nat_multi ~ log_rile + pervote | year, 
              data = subset(party_df_exclude, countryname == country_name))
  return(model)
}

# apply function to each country and store models in a named list
models <- setNames(lapply(countries, run_regression), countries)


# get predictions for each country
predictions_list <- map(set_names(countries), function(country) {
  model_pred <- ggpredict(models[[country]], terms = "log_rile") %>%
    as_tibble() %>%  # Ensure it's a tibble
    mutate(countryname = country)  # Add country name for faceting
  return(model_pred)
})

predictions_df <- bind_rows(predictions_list)

# plot predictions
ggplot(predictions_df, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1) +  # Predicted values
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # Confidence interval
  facet_wrap(~ countryname, scales = "free_y") +  # Facet by country
  theme_pubclean() +
  scale_x_continuous(
    breaks = seq(-2, 2, by = 1)
  )



# Radical right electoral success and mainstream party salience -----------
#OIIId


# data prep 

party_df_rr <- party_df %>% 
  group_by(countryname, year) %>%
  summarize(far_right_vote = sum(pervote[family == 1], na.rm = TRUE), .groups = "drop") %>%
  group_by(countryname) %>%
  mutate(far_right_vote_lag = lag(far_right_vote)) %>%
  ungroup()

party_df_rr <- party_df %>%
  left_join(
    party_df_rr, 
    by = c("countryname", "year"),
    relationship = "many-to-one"
  )

# check coding
party_df_rr %>%
  select(c("countryname","year",
           "partyname","far_right_vote_lag",
           "far_right_vote")) # %>% View()

# remove radical right parties 

party_df_rr <- party_df_rr %>%
  filter(family !=1) 
 


## model 


model_rr <-  feols(
  nat_multi ~ far_right_vote_lag*log_rile + pervote | factor(party) + factor(year),
  data = party_df_rr)


ame_results <- slopes(model_rr)
plot_slopes(model_rr, variables = "far_right_vote_lag", condition = "log_rile") +
  theme_classic2()







