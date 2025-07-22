library(tidyverse)
library(ggpubr)
library(jtools)
library(fixest)
library(marginaleffects)
library(patchwork)
library(stringr)
library(forcats)
library(modelsummary)
library(ggbeeswarm)
library(marginaleffects)
library(purrr)

party_df <- read_csv("data/df_party_coded_gov.csv")
party_ches <- read_csv("data/ches_party.csv")



# Data prep ---------------------------------------------------------------

# edit party family names
party_df <- party_df %>%
  mutate(fam_fact = tolower(fam_fact))


party_ches <- party_ches %>%
  mutate(fam_dummy= tolower(fam_dummy)) 


# add lagged DVs 
party_df <- party_df %>% 
  group_by(party)%>% 
  mutate(nat_multi_lag = lag(nat_multi),
         cult_agg_lag = lag(cult_agg),
         mor_lag = lag(mor)
         ) %>%
  mutate(fam_fact = factor(fam_fact)) %>% ## convert family to factor
  ungroup()

#check lagging
party_df %>% 
  filter(countryname == "Czech Republic", 
         partyabbrev == "ODS") %>%
  arrange(desc(year)) %>%
  select(year, party, nat_multi, nat_multi_lag, mor, mor_lag) #%>% View()


# Reference category
party_df$fam_fact<- relevel(party_df$fam_fact, ref = "radleft")


# family factor CHES
party_ches <- party_ches %>%
  mutate(fam_fact = factor(fam_dummy))

# Reference category
party_ches$fam_fact<- relevel(party_ches$fam_fact, ref = "radleft")


# dataset exluding radical right 
party_df_exclude <- party_df %>%
  filter(family != 1)



# radical right party summary table - Appendix No. 3 -----------------------------------------------

rr_table <- party_df %>% 
  filter(farright == 1) %>%
  distinct(party, .keep_all = TRUE) %>% 
  group_by(countryname) %>%
  reframe(names = paste0(partyname, " (", partyabbrev, ")", collapse = "; ")) 

write.table(rr_table, "output_final/rr_summary_table.txt",
            sep = "~", quote = FALSE, row.names = FALSE,col.names = TRUE,
            na = "")


family_table <- party_df %>% 
  distinct(party, .keep_all = TRUE) %>% 
  summarise(countryname, partyname, partyabbrev, fam_fact)

family_table_wide   <- family_table %>%
  group_by(countryname, fam_fact) %>%
  reframe(names = paste0(partyname, " (", partyabbrev, ")", collapse = "; ")) %>%
  pivot_wider(names_from = fam_fact, values_from = names)

write.table(family_table_wide, "output_final/family_summary_table.txt",
            sep = "~", quote = FALSE, row.names = FALSE,col.names = TRUE,
            na = "")



# Radical right vs other parties comparison, descriptives - Figure 6 ------------------------------------------------
# OIIIa 

party_df_long <- party_df %>%
  pivot_longer(
    cols = c("cult_agg", "nat_multi", "mor"),
    values_to = "salience",
    names_to = "issue_area"
  ) %>%
  mutate(
    issue_area_fact = factor(
      issue_area,
      levels = c("cult_agg", "nat_multi", "mor"),
      labels = c("Aggregate cultural issues",
                 "Nationalism, multiculturalism, and minorities",
                 "Morality")
    )
  )



#aggregate cultural issues
Fig.6<- ggplot(party_df_long, aes(x = fam_fact, y = salience, color = fam_fact)) +
  geom_beeswarm(alpha = 0.3) +
  geom_boxplot(notch = T,
              alpha  = 0.4) +
  theme_minimal() +
  facet_wrap(~issue_area_fact, scales = "free_y",
             nrow = 2)+ 
  labs(
    x = "",
    y = "% of quasisentences",
    
  ) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   size = 10,
                                   color = "black"),
        strip.text   = element_text(size = 12))


ggsave("output_final/Fig6_desc.jpg", Fig.6, height = 7, width = 8)






## party family regression 

model_agg_family <- feols(cult_agg ~ fam_fact + pervote + factor(pgv_cabinet_party) |factor(country) + factor(year),
                          cluster = c("factor(party)", "factor(year)"),
                       data = party_df)

model_multi_family <- feols(nat_multi ~ fam_fact + pervote + factor(pgv_cabinet_party) |factor(country) + factor(year), 
                            cluster = c("factor(party)", "factor(year)"),
                          data = party_df)

model_mor_family <- feols(mor ~ fam_fact + pervote + factor(pgv_cabinet_party) |factor(country) + factor(year), 
                          cluster = c("factor(party)", "factor(year)"),
                            data = party_df)


## party family regression - CHES 


model_galtan <- feols(galtan_salience ~ fam_fact + vote + factor(govt) |factor(country) + factor(year),
                      cluster = c("factor(party)"),
                      data = party_ches)

model_imm <- feols(immigrate_salience ~ fam_fact + vote + factor(govt) |factor(country) + factor(year), 
                   cluster = c("factor(party)"),
                   data = party_ches)

model_multi <- feols(multicult_salience ~ fam_fact + vote + factor(govt) |factor(country) + factor(year), 
                     cluster = c("factor(party)"),
                     data = party_ches)



# Plot predictions - Figure 7  -----------------------------------------------

pred_agg<- plot_predictions(model_agg_family, by = "fam_fact") +
  theme_grey() +
  coord_flip() +
  labs(x = "family",
       y = "",
       title = "Aggregate cultural issues") 

pred_multi <- plot_predictions(model_multi_family, by = "fam_fact") +
  theme_grey() +
  coord_flip() +
  labs(x = "family",
       y = "",
       title = "Nationalism, multiculturalism, and minorities")

pred_mor <- plot_predictions(model_mor_family, by = "fam_fact") +
  theme_grey() +
  coord_flip() +
  labs(x = "family",
       y = "% quasisentences",
       title = "Morality") 

combo <-  pred_multi /pred_agg / pred_mor


ggsave("output_final/Fig7_preds.jpg", combo, height = 7, width = 8)



# ches 

pred_gal <- plot_predictions(model_galtan, by = "fam_fact") +
  theme_grey() +
  coord_flip() +
  labs(x = "family",
       y = "",
       title = "GALTAN")

pred_imm <- plot_predictions(model_imm, by = "fam_fact") +
  theme_grey() +
  coord_flip() +
  labs(x = "family",
       y = "",
       title = "Immigration")

pred_multicult <- plot_predictions(model_multi, by = "fam_fact") +
  theme_grey() +
  coord_flip() +
  labs(x = "family",
       y = "salience (expert score)",
       title = "Multiculturalism")


combo_ches <- pred_gal / pred_imm / pred_multicult


ggsave("output_final/Fig7_preds_ches.jpg", combo_ches, height = 7, width = 8)



# Salience and left-right positions -------------------------------------
#OIIIb

# OLS for all parties
model_agg_all <- lm(cult_agg ~ log_rile + pervote + factor(country), data = party_df)
model_multi_all <- lm(nat_multi ~ log_rile + pervote + factor(country), data = party_df)

model_multi_all %>% summary()


## Party family positions - validation

ggplot(party_df, aes(x = fam_fact, y = log_rile))+
  geom_boxplot() +
  coord_flip() +
  labs(x = "family")


# excluding radical right - scatterplots

party_df <- party_df %>%
  mutate(farright = factor(farright))


Nat_multi_rile <- ggplot(party_df, aes(x = log_rile, y = nat_multi))+
  geom_point(aes(shape = farright, color = farright)) +
  geom_smooth(method = "loess",
              span = 0.66,
              color = "gray17")+
  facet_wrap(~ countryname,
             ) +
  theme_pubr()+
  theme(
    legend.position = "none"
  )+
  scale_color_brewer(palette = "Set1") +
  labs(y = "Salience (%)",
       x = "Rile (log)",
       title = "Nationalism, multiculturalism and minorities")


Morality_rile <- ggplot(party_df, aes(x = log_rile, y = mor))+
  geom_point(aes(shape = farright, color = farright))  +
  facet_wrap(~ countryname) +
  theme_pubr() +
  geom_smooth(method = "loess",
              span = 0.66,
              color = "gray17")+
  theme(
    legend.position = "none"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0,30))+
  labs(y = "Salience (%)",
       x = "Rile (log)",
       title ="Morality issues")

Combo_rile <- Nat_multi_rile/Morality_rile

ggsave("output_final/Fig_Combo_rile.jpg", Combo_rile, height = 10, width = 8)
ggsave("output_final/Fig_Morality_rile.jpg", Morality_rile, height = 7, width = 8)
ggsave("output_final/Fig_Nat_multi_rile.jpg", Nat_multi_rile, height = 7, width = 8)


# inspect outliers 

party_df %>% 
  arrange(desc(nat_multi)) %>%
  select(partyname, partyabbrev, year, nat_multi, log_rile,
         farright, fam_fact, fam_fact_alt, pervote, totseats, absseat)%>%
  head()





## Split models - Appendices No. 9 , No. 10 

model_agg_split <- feols(cult_agg ~ log_rile + pervote + factor(pgv_cabinet_party) | factor(year), 
                           cluster = c("factor(party)", "factor(year)"),
                           split = ~countryname,
                           data = party_df_exclude)


model_multi_split <- feols(nat_multi ~ log_rile + pervote + factor(pgv_cabinet_party)| factor(year), 
                           cluster = c("factor(party)", "factor(year)"),
                        split = ~countryname,
                        data = party_df_exclude)


model_mor_split <- feols(mor ~ log_rile + pervote + factor(pgv_cabinet_party) | factor(year), 
                           cluster = c("factor(party)", "factor(year)"),
                           split = ~countryname,
                           data = party_df_exclude)


modelsummary(model_multi_split,
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/logrile_split_table_multi.docx")


modelsummary(model_mor_split,
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/logrile_split_table_mor.docx")



# Radical right electoral success and mainstream party salience -----------
#OIIId


# data prep 

party_df_rr <- party_df %>% 
  group_by(countryname, year) %>%
  summarize(far_right_vote = sum(pervote[family == 1], na.rm = TRUE), 
            far_right_seats = sum(absseat[family == 1], na.rm = TRUE),
            .groups = "drop") %>%
  arrange(countryname, year) %>%
  group_by(countryname) %>%
  mutate(far_right_vote_lag = dplyr::lag(far_right_vote),
         far_right_seats_lag = dplyr::lag(far_right_seats)) %>%
  ungroup()

party_df_rr <- party_df %>%
  left_join(
    party_df_rr, 
    by = c("countryname", "year"),
    relationship = "many-to-one"
  )


## binary far right variable 
party_df_rr  <- party_df_rr %>% 
  mutate(
  far_right_pres = if_else(
    far_right_seats_lag >0, 1, 0)
  )


# remove radical right parties 

party_df_rr <- party_df_rr %>%
  filter(family !=1) 



# summary stats table - Appendix No. 4 ------------------------------------------------------
library(sjmisc)

party_df_rr_sum <- party_df_rr %>%
  mutate(pgv_cabinet_party_fac = factor(pgv_cabinet_party))

party_df_rr_sum %>% 
  select(nat_multi, nat_multi_lag, 
         far_right_vote_lag, far_right_pres,
        pervote, 
        pgv_cabinet_party_fac, 
        net_migration_avg) %>%
  descr(show = c("n", "mean", "sd", "range"),
        out = "browser",
        file = "output_final/desc_stats.html")
  


# model nat_multi salience on far_right_vote_lag  -------------------------


# simple model


model_rr.simple <-  feols(
  nat_multi ~ far_right_vote_lag + nat_multi_lag | factor(party),
  data = party_df_rr)

summary(model_rr.simple)


model_rr <-  feols(
  nat_multi ~ far_right_vote_lag + pervote + factor(pgv_cabinet_party) + nat_multi_lag + net_migration_avg | factor(party),
  data = party_df_rr)


# binary presence 

model_rr_pres <-  feols(
  nat_multi ~ factor(far_right_pres) + pervote + factor(pgv_cabinet_party) + nat_multi_lag + net_migration_avg | factor(party),
  data = party_df_rr)



# RR models Table 5 -----------------------------------------------------




modelsummary(list(model_rr, model_rr_pres),
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/modelrr_table_double.docx")



# interaction - left right and party size

model_rr_int <-  feols(
  nat_multi ~ far_right_vote_lag*log_rile + far_right_vote_lag + log_rile + pervote + factor(pgv_cabinet_party) +nat_multi_lag | factor(party),
  cluster = "factor(party)",
  data = party_df_rr)

model_rr_int_size <-  feols(
  nat_multi ~ far_right_vote_lag*pervote + far_right_vote_lag + pervote + log_rile + factor(pgv_cabinet_party) +nat_multi_lag | factor(party),
  cluster = "factor(party)",
  data = party_df_rr)


modelsummary(list(model_rr_int, model_rr_int_size),
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             output = "output_final/modelrr_table_interactions.docx")



# Interaction plot - RR vote share and left-right position Figure 9 -------



Fig_interaction <- plot_slopes(model_rr_int, variables = "far_right_vote_lag", condition = "log_rile",
            rug = TRUE) +
  theme_pubr() +
  geom_hline(yintercept   = 0, linetype = "dotted") +
  scale_y_continuous(breaks = seq(-1, 2, by =0.5)) +
  scale_x_continuous(limits = c(-2,2)) +
  labs(y = "Marginal effect of radical-right 
  vote at t-1", 
       x = "Left-right position (log)")

ggsave("output_final/Fig_interaction.jpg", Fig_interaction, height = 7, width = 8)

Fig_interaction_size <- plot_slopes(model_rr_int_size, variables = "far_right_vote_lag", condition = "pervote",
                               rug = TRUE) +
  theme_pubr() +
  geom_hline(yintercept   = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(0,50)) +
  labs(y = "Marginal effect of radical-right 
  vote share at previous election", 
       x = "Party size")

ggsave("output_final/Fig_interaction_size.jpg", Fig_interaction_size, height = 7, width = 8)




# Further robustness checks -----------------------------------------------



# inspecting party positions 
party_df_rr %>% 
  filter(log_rile <= -1) %>% 
  select(partyname) %>%
  View()



# interactions presence - success and left-right position

model_rr_int_pres <-  feols(
  nat_multi ~ far_right_pres*log_rile + pervote + factor(pgv_cabinet_party) +nat_multi_lag | factor(party),
  cluster = "factor(party)",
  data = party_df_rr)

# success and size 

model_rr_int_size_pres <-  feols(
  nat_multi ~ far_right_pres*pervote + log_rile + factor(pgv_cabinet_party) +nat_multi_lag | factor(party),
  cluster = "factor(party)",
  data = party_df_rr)

ame_results <- slopes(model_rr_int_pres)
plot_slopes(model_rr_int_pres, variables = "far_right_pres", condition = "log_rile") +
  theme_classic2() +
  geom_hline(yintercept = 0, linetype = "dashed")




## Keep only the socialist, liberal, conservative, or Christian Democratic parties

party_df_rr_lim <- party_df_rr %>%
  filter(fam_fact %in% c("soc", "lib", "con", "cd"))


model_rr_lim <-  feols(
  nat_multi ~ far_right_vote_lag + pervote + factor(pgv_cabinet_party) + nat_multi_lag + net_migration_avg | factor(party),
  data = party_df_rr_lim)


# binary presence 

model_rr_pres_lim <-  feols(
  nat_multi ~ factor(far_right_pres) + pervote + factor(pgv_cabinet_party) + nat_multi_lag + net_migration_avg | factor(party),
  data = party_df_rr_lim)




modelsummary(list(model_rr_lim, model_rr_pres_lim),
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/modelrr_table_lim.docx")



# alternative dv operationalizations --------------------------------------


# operationalization according to Abou-Chadi (2016)


party_df_rr <- party_df_rr %>%
  mutate(
    multi_pos = (per608 - per607)/ (per607 + per608),
    multi_neg = per608 + per602,
    multicult_alt = per608 + per607
  )

# add laggs
party_df_rr <- party_df_rr %>%
  group_by(party) %>%
  mutate(multi_pos_lag = lag(multi_pos),
         multi_neg_lag = lag(multi_neg),
         multicult_alt_lag = lag(multicult_alt)
  )


party_df_rr %>%
  filter(countryname == "Czech Republic",
         partyabbrev == "ODS") %>%
  select(partyname, year, multi_neg, multi_neg_lag)



# check 
party_df_rr %>%
  filter(countryname == "Czech Republic", partyabbrev == "ČSSD") %>%
  select(party, year, multicult_alt, multicult_alt_lag)


model_rr_alt <-  feols(
  multicult_alt ~ far_right_vote_lag + pervote + factor(pgv_cabinet_party) + multicult_alt_lag + net_migration_avg | factor(party),
  data = party_df_rr)

model_rr_alt_pres <-  feols(
  multicult_alt ~ far_right_pres + pervote + factor(pgv_cabinet_party) + multicult_alt_lag + net_migration_avg | factor(party),
  data = party_df_rr)


modelsummary(list(model_rr_alt, model_rr_alt_pres),
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/modelrr_table_alt.docx")




# alternative interactions 


alt.interaction <-  feols(
  multicult_alt ~ far_right_vote_lag*log_rile + log_rile + far_right_vote_lag + pervote + factor(pgv_cabinet_party) + multicult_alt_lag + net_migration_avg | factor(party),
  data = party_df_rr)

alt.ame <- slopes(alt.interaction)

plot_slopes(alt.interaction, variables = "far_right_vote_lag", condition = "log_rile") +
  theme_classic2()+
  geom_hline()

summary(model_rr_alt)


model_rr_neg <-  feols(
  multi_neg ~ far_right_vote_lag + pervote + factor(pgv_cabinet_party) + multi_neg_lag + net_migration_avg | factor(party),
  data = party_df_rr)


# position model 

model_rr_pos <-  feols(
  multi_pos ~ far_right_vote_lag + pervote + factor(pgv_cabinet_party) + multi_pos_lag + net_migration_avg | factor(party),
  data = party_df_rr)













