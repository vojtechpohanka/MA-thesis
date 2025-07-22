library(tidyverse)
library(ggpubr)
library(ggrepel)
library(fixest)
library(patchwork)
library(countrycode)
#install.packages("ggformula")
library(ggformula)
library(modelsummary)
library(ggbeeswarm)
library(marginaleffects)


df_party <- read.csv("data/df_party_coded.csv") # party level data

df_elect <- read.csv("data/df_elect_cl.csv") # election level data

ches_elect <- read.csv("data/ches_elections.csv")


theme_set(theme_pubclean(base_size = 22))



## summary tables with elections, number of programs 

election_table <- df_elect %>% 
  group_by(countryname) %>%
  reframe(countryname, 
          first_election = min(year, na.rm = TRUE),
          last_election = max(year, na.rm = TRUE),
          n_elections = n()) %>%
  distinct(countryname, .keep_all = TRUE) 

party_table <- df_party %>% 
  group_by(countryname) %>%
  reframe(countryname, 
          n_manifestos = n()) %>%
  distinct(countryname, .keep_all = TRUE) 


manifesto_table <- election_table %>%
  left_join(party_table, 
            by = "countryname")

write_csv(manifesto_table, file = "output_final/manifesto_table.txt")



## data prep - variable indicating if share of religious pop is above average 

# calculate mean for all countries 
rel_table <- df_elect %>% 
  group_by(countryname) %>%
  summarise(mean_rel = mean(share_rel, na.rm = TRUE))

# calculate aggregate mean
mean_rel_agg <- mean(rel_table$mean_rel, na.rm = TRUE)

rel_table <- rel_table %>%
  mutate(relig_above_mean = 
         if_else(mean_rel > mean_rel_agg, 1, 0)
  )

# code into main dataset
rel_countries <- rel_table %>%
  filter(relig_above_mean == 1) %>%
  pull(countryname)
                 

# if religiosity is above mean
df_elect <- df_elect %>% 
  mutate(relig_above_mean = factor(
           if_else(
    countryname %in% rel_countries, 1, 0)
  )
  )

# check 
table(df_elect$countryname, df_elect$relig_above_mean)



#long dataset for graphing
df_long <- df_elect %>% 
  pivot_longer(cols = uncod_a:non_econ_all_a,
               names_to = "issue_area",
               values_to = "share")

# category names for later 
df_long <- df_long  %>% 
  mutate(category = fct_recode(issue_area,
                               "Aggregate economic" = "econ_agg_a",
                               "Nationalism, multiculturalism,immigration, and minorities + morality issues" = "nat_multi_mor_a", 
                               
                               "Aggregate cultural" =  "cult_agg_a",
                               "All non-economic" = "non_econ_all_a",
                               "Morality" = "mor_a",
                               "Nationalism, multiculturalism, and minorities" = "nat_multi_a",
                               "Environment" = "envi_a",
                               "Welfare and labour" = "welf_a",
                               "Economy" = "econ_a", 
                               "EU" = "EU_a"))



# Figure I - Cult vs Econ ------------------------------------------------

Fig1<- df_long %>% 
  filter(decade != "2020s") %>%
  filter(issue_area %in% c("econ_agg_a", "cult_agg_a")) %>%
  ggplot(aes(x = category, y = share)) + 
  geom_boxplot(aes(color = issue_area),
               outliers = FALSE) +
  geom_point(aes(color = issue_area),
            position = position_jitter(seed = 2),
             alpha = 0.6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_wrap(~decade) +
  theme_pubclean() +
  labs( x= "Issue area", y = "Mean salience (%)",
        title = "") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

ggsave("output_final/Fig1.jpg", Fig1, height = 5, width = 5)


# calculate differences in medians 
df_long %>% 
  filter(decade != "2020s") %>%
  filter(issue_area %in% c("econ_agg_a", "cult_agg_a")) %>%
  group_by(decade, issue_area) %>%
  summarise(med = median(share, na.rm = TRUE))%>%
  pivot_wider(names_from = issue_area,
              values_from = med) %>%
  mutate(diff = econ_agg_a - cult_agg_a)



# Figure 2 - Cult over time ------------------------------------------------

Fig2 <- df_long %>% 
  filter(issue_area %in% c("nat_multi_a", "EU_a", "envi_a","mor_a")) %>%
  ggplot(aes(x=year, y=share, color = category))+
  geom_smooth(aes(linetype=category),method = "loess",
              se=T, 
              span =0.66,
              alpha = 0.3)+
  #geom_point(aes(shape = category)) +
  scale_x_continuous(breaks = seq(1990,2020, by = 5))+
  scale_y_continuous(breaks = seq(0,10, by=1))+
  theme_pubclean()+ 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8)) +
  labs(title = "", x= "Year", y= "Mean salience (%)")+
  scale_color_brewer(palette = "Set2") 

ggsave("output_final/Fig2.jpg", Fig2, height = 5, width = 7)




# Prediction plots --------------------------------------------------------

# models
m1_y <- lm(nat_multi ~ year + countryname, data = df_party)
m2_y <-  lm(mor ~ year + countryname, data = df_party)
m3_y <-  lm(envi ~  year + countryname, data = df_party)
m4_y <-  lm(EU ~year + countryname, data = df_party)
m5_y <-  lm(econ_agg ~year + countryname, data = df_party)

theme_set(theme_pubr())

p1 = plot_predictions(m1_y, condition = "year",
                      vcov = "HC1") +
  labs(
    x = "",
    y = "",
    title = "Nationalism, multiculturalism,\nand minorities"
  )


p2 = plot_predictions(m2_y, condition = "year",
                      vcov = "HC1") +
  labs(
    x = "",
    y = "",
    title = "Morality issues"
  )

p3 = plot_predictions(m3_y, condition = "year",
                      vcov = "HC1") +
  labs(
    x = "",
    y = "% quasisentences",
    title = "Environment"
  )

p4 = plot_predictions(m4_y, condition = "year",
                      vcov = "HC1") +
  labs(
    x = "",
    y = " ",
    title = "European Union"
  )

p5 = plot_predictions(m5_y, condition = "year",
                      vcov = "HC1") +
  labs(
    x = "",
    y = "% quasisentences",
    title = "Economic issues"
  )



fig3_summ <- (p5 + p1 + p2 + p3 + p4)

ggsave("output_final/Fig3_sum_robust.png", fig3_summ, height = 7, width = 10)


modelsummary(list("Nationalism, multiculturalism, and minorities " = m1_y, 
                  "Morality" = m2_y, 
                  "Environment" = m3_y, 
                  "EU" = m4_y,
                  "Economic issues" = m5_y),
             vcov = "robust",
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/Year_pred_summ.docx")




# Fig 4 - marpor ches combo plot  -----------------------------------------------------


points <- df_elect %>% 
  filter(nat_multi_a >= 6) 

Fig4 <- df_elect %>% 
  ggplot(aes(x=year, y=nat_multi_a))+
  geom_point(aes(color = countryname),
              shape = 1) +
  geom_text_repel(data = points,
                  aes(label = elect_label),
                  alpha= 0.8,
                  size = 3) +
  geom_vline(xintercept = 2015, 
             linetype = "dashed") +
  geom_smooth(linetype = "dashed",
              alpha = 0.8,
              se = F,
              color = "grey") +
  ggplot2::annotate("text", x=2015, y=20, label="2015", angle=0,
                    hjust = - 0.2) +
  scale_y_continuous(breaks = seq(0,30, by=5))+
  scale_x_continuous(breaks = seq(2006, 2020, by =1),
                     limits = c(2006,2020)) +
  theme_classic2() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x= "", y= "Mean salience (%)",
       title = "Nationalism, multiculturalism, and minorities
       salience (MARPOR)") 


ggsave("output_final/Fig4.jpg", Fig4, height = 5, width = 5)



# combining ches and marpor 

ches_points <- ches_elect %>%
  filter(
    (immigrate_mean >4.2 & year <= 2010) |
    (immigrate_mean >6.3 & year == 2017) |
    (immigrate_mean >5.2 &year == 2019) |
    (immigrate_mean >6.2 &year == 2024)
  )

Fig4_ches <- ggplot(data = ches_elect, aes(x = year, y = immigrate_mean))+
  geom_point(aes(color = country_full),
             shape = 1) +
  geom_text_repel(data = ches_points,
                  aes(label = survey_label),
                  alpha= 0.8,
                  size = 3,
                  max.overlaps = 15
                  ) +
  geom_vline(xintercept = 2015, 
             linetype = "dashed") +
  geom_smooth(linetype = "dashed",
              alpha = 0.8,
              se = F,
              color = "grey") +
  ggplot2::annotate("text", x=2015, y=10, label="2015", angle=0,
                    hjust = - 0.2) +
  scale_y_continuous(breaks = seq(0,10, by=1))+
  scale_x_continuous(breaks = c(2006, 2010, 2015, 2017, 2019, 2024),
                     limits = c(2006,2024)) +
  theme_classic2() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x= "Year", y= "Mean salience",
       title = "Immigration salience (CHES)") 
Fig4_ches

Fig4_combo <- Fig4 / Fig4_ches

ggsave("output_final/Fig4_combo.png", Fig4_combo, height = 7, width = 7)



# Country-level factors ---------------------------------------------------

Fig5.M1 <- df_elect %>%
  ggplot(aes(x = fct_reorder(countryname, nat_multi_mor_a, .fun = median), 
             y =  nat_multi_mor_a)) +
  geom_boxplot(outliers = F) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  geom_beeswarm(alpha =0.5) +
  labs(y = "Mean salience (%)", 
       x = "country",
       title = "")+
  theme_pubclean()


ggsave("output_final/Fig5.jpg", Fig5.M1, height = 5, width = 7)


nat_multi_median <- median(df_elect$nat_multi_a)

Fig5.M3 <- df_elect %>%
  ggplot(aes(x = fct_reorder(countryname, nat_multi_a, .fun = median), 
             y =  nat_multi_a)) +
  geom_boxplot(aes(fill = minority_legacy), outliers = F, notch = F) + 
  geom_beeswarm(alpha =0.5) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#E78AC3"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  geom_hline(yintercept = nat_multi_median,
             linetype = "dashed")+
  labs(y = "Mean salience (%)", 
       x = "",
       title = "Nationalism, multiculturalism, and minorities")+
  theme_pubclean() +
  theme(legend.position = "none") 


mor_median <- median(df_elect$mor_a)

Fig5.M4 <- df_elect %>%
  ggplot(aes(x = fct_reorder(countryname, mor_a, .fun = median), 
             y =  mor_a)) +
  geom_boxplot(aes(fill = relig_above_mean), outliers = F, notch = F) +
  geom_beeswarm(alpha =0.5) +
  scale_fill_manual(values = c("0" = "#377EB8", "1" = "#E31A1C"))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  geom_hline(yintercept = mor_median,
             linetype = "dashed") +
  labs(y = "Mean salience (%)", 
       x = "Country",
       title ="Morality issues")+
  theme_pubclean() +
  theme(legend.position = "none")

Fig5_combo = Fig5.M3/Fig5.M4

ggsave("output_final/Fig5_combo.png", Fig5_combo, height = 8, width = 8)


# Table 4 - Religiosity regression   ------------------------------------------------------------


# data prep - create christian democratic party indicator
df_party <- df_party %>%
  mutate(
    christ_dem = factor(
      if_else(
      fam_fact == "CD", 1, 0
    )
    )
  )


  # lagged morality emphasis 
  df_party <- df_party %>%
    group_by(party) %>%
    arrange(party, year) %>%
    mutate(
      lag_mor = lag(mor)
      ) %>%
    ungroup()



  # check coding
  df_party %>% filter(
    countryname == "Czech Republic", partyabbrev == "ČSSD"
  ) %>%
    select(partyname, year, mor, lag_mor)



Rel.simple <- feols(mor ~ share_rel + gdp_log | factor(country),
                  cluster = "factor(party)",
                 data = df_party)

Rel.model <- feols(mor ~ share_rel + christ_dem + gdp_log | factor(country),
                   cluster = "factor(party)",
                   data = df_party)


Rel.model.lag <- feols(mor ~ share_rel + christ_dem + gdp_log + lag_mor | factor(country),
                   cluster = "factor(party)",
                   data = df_party)


modelsummary(list(
  "Model I" = Rel.simple,
  "Model II" = Rel.model, 
  "Model III" = Rel.model.lag),
             stars = c('+' = .1, 
                       '*' = 0.05,
                       '**' = .01,
                       '***' = .001),
             gof_omit = 'DF|Deviance|R2|R2 Within|R2 Within Adj|RMSE|AIC|BIC',
             output = "output_final/Rel.model.docx")



  





