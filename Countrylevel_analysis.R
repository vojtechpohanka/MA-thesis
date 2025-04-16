library(tidyverse)
library(plm)
library(gplots)
library(tseries)
library(huxtable)
library(ggpubr)
library(ggeffects)
library(jtools)
library(ggrepel)
library(fixest)
library(ragg)
library(RColorBrewer)
library(emmeans)
library(patchwork)
library(countrycode)
#install.packages("ggformula")
library(ggformula)

df_party <- read.csv("data/df_party_coded.csv") # party level data

df_elect <- read.csv("data/df_elect_cl.csv") # election level data

ches_elect <- read.csv("data/ches_elections.csv")



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

df_elect <- df_elect %>% 
  group_by(countryname) %>%
  mutate(election_number = row_number()) %>%
  ungroup()

theme_set(theme_pubclean(base_size = 22))


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


spline <- df_long %>% 
  filter(issue_area %in% c("nat_multi_a", "EU_a", "envi_a","mor_a")) %>%
  ggplot(aes(x=year, y=share, color = category))+
  geom_spline(aes(linetype = category))+
  #geom_smooth(aes(linetype=category),method = "loess",
   #           se=T, 
    #          span =0.66,
     #         alpha = 0.3)+
  #geom_point(aes(shape = category)) +
  scale_x_continuous(breaks = seq(1990,2020, by = 5))+
  scale_y_continuous(breaks = seq(0,10, by=1))+
  theme_pubclean()+ 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8)) +
  labs(title = "", x= "Year", y= "Mean salience (%)")+
  scale_color_brewer(palette = "Set2") 



# Prediction plots --------------------------------------------------------

# models

m1_y <- lm(nat_multi ~ year + factor(country),
           data = df_party)

m2_y <-  lm(mor ~ year + factor(country),
            data = df_party)

m3_y <-  lm(envi ~ year + factor(country),
            data = df_party)

m4_y <-  lm(EU ~ year + factor(country),
            data = df_party)

export_summs(m1_y, m2_y, m3_y, m4_y, robust = T,
             model.names = c("Multiculturalism",
                             "Morality",
                             "Environment",
                             "EU"), 
             coefs = "year",
             to.file ="docx",
             file.name = "output_final/Year_pred.docx")

# predictions

pred1 <- ggpredict(m1_y, terms = "year")
pred2 <- ggpredict(m2_y, terms = "year")
pred3 <- ggpredict(m3_y, terms = "year")
pred4 <- ggpredict(m4_y, terms = "year")


pred1$model <- "Multiculturalism"
pred2$model <- "Morality"
pred3$model <- "Environment"
pred4$model <- "EU"


all_preds <- bind_rows(pred1, pred2, pred3, pred4)

# Create faceted plot
Fig3<- ggplot(all_preds, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  facet_wrap(~ model, scales = "free_y") +
  labs(x = "Year", y = "% of quasisentences") +
  theme_pubclean()


ggsave("output_final/Fig3.jpg", Fig3, height = 5, width = 5)



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
  annotate("text", x=2015, y=20, label="2015", angle=0) +
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
    (immigrate_mean >5.2 &year == 2019)
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
  annotate("text", x=2015, y=10, label="2015", angle=0) +
  scale_y_continuous(breaks = seq(0,10, by=1))+
  scale_x_continuous(breaks = seq(2006, 2020, by =1),
                     limits = c(2006,2020)) +
  theme_classic2() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x= "Year", y= "Mean salience (%)",
       title = "Immigration salience (CHES)") 
Fig4_ches

Fig4_combo <- Fig4 / Fig4_ches

ggsave("output_final/Fig4_combo.jpg", Fig4_combo, height = 8, width = 8)



library(ggbeeswarm)
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



Fig5.M3 <- df_elect %>%
  ggplot(aes(x = fct_reorder(countryname, nat_multi_a, .fun = median), 
             y =  nat_multi_a)) +
  geom_boxplot(outliers = F) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  geom_beeswarm(alpha =0.5) +
  labs(y = "Mean salience (%)", 
       x = "",
       title = "nationalism, multiculturalism, and minorities")+
  theme_pubclean()


Fig5.M4 <- df_elect %>%
  ggplot(aes(x = fct_reorder(countryname, mor_a, .fun = median), 
             y =  mor_a)) +
  geom_boxplot(outliers = F) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  geom_beeswarm(alpha =0.5) +
  labs(y = "Mean salience (%)", 
       x = "country",
       title ="morality issues")+
  theme_pubclean()

Fig5_combo = Fig5.M3/Fig5.M4

ggsave("output_final/Fig5_combo.jpg", Fig5_combo, height = 8, width = 8)

summary(feols(nat_multi_a ~ relevel(factor(countryname),
                                    ref = "Czech Republic") | factor(year),
      data = df_elect))

summary(feols(mor_a ~ relevel(factor(countryname),
                                    ref = "Croatia") | factor(year),
              data = df_elect))



Fig5.Ches1 <- ches_elect %>%
  group_by(country_full) %>%
  reframe(average = mean(
    galtan_mean, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = fct_reorder(country_full, average),
             y = average))+
  geom_col() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  labs(y = "Mean salience (%)", 
       x = "country")+
  theme_pubclean()

Fig5.Ches2 <- ches_elect %>%
  group_by(country_full) %>%
  reframe(average = mean(
    immigrate_mean, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = fct_reorder(country_full, average),
             y = average))+
  geom_col() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  labs(y = "Mean salience (%)", 
       x = "country")+
  theme_pubclean()



# Multi-level  ------------------------------------------------------------

library(lme4)

summary(lmer(nat_multi ~ legacy
   + (1|countryname)+ (1|partyname) + (1|year), data = df_party)
   )

summary(lmer(nat_multi ~ ethnic_minorities + share_rel + legacy
             + (1|countryname)+ (1|partyname) + (1|year), data = df_party)
)









