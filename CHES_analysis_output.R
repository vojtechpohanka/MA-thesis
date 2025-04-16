library(tidyverse)
library(data.table)
library(ggrepel)
library(ggpubr)
library(countrycode)




# Data prep ---------------------------------------------------------------

trend <- fread("data/1999-2019.csv")
df17 <- fread("data/CHES_2017.csv")
df24 <- fread("data/CHES_2024.csv")

# unify country indicator 

country_no <- unique(trend$country)

country_name <- unique(df24$country[!df24$country%in% c("tur", "nor", "swi", "ice")])

lookup <- data.frame(country_no, cname = c(country_name, NA))
  
lookup <- lookup %>% 
  mutate(
    cname = case_when(
      country_no == 40 ~ "cyp",
      country_no == 38 ~ NA,
      TRUE ~ cname
    )
  )




trend_fix <- trend %>% 
  left_join(
    lookup,
    by = c("country" = "country_no"),
    relationship = "many-to-one") %>%
  relocate(
    cname, .after = country
  )

table(trend_fix$country, trend_fix$cname, useNA = "always")


trend_fix <- trend_fix %>%
  select(!country) %>%
  rename(country = cname)

head(trend_fix)


trend_short <- trend_fix %>% 
  select(country, party, party_id,
         year,
         electionyear, vote, seat, 
         lrgen,lrecon, lrecon_salience,
         galtan, galtan_salience,
         multiculturalism, multicult_salience,
         immigrate_policy, immigrate_salience)

df24_short <- df24 %>%
  mutate(year = 2024) %>%
  select(country, party, party_id,
         year,
         electionyear, vote, seat, 
         lrgen,lrecon, lrecon_salience,
         galtan, galtan_salience,
         multiculturalism, multicult_salience,
         immigrate_policy, immigrate_salience)


df17_short <- df17%>%
  mutate(year = 2017) %>%
  select(!immigrate_salience) %>%
  rename(immigrate_salience = immigra_salience) %>%
  select(country, party, party_id,
         year,
         electionyear, vote, seat, 
         lrgen,lrecon, lrecon_salience,
         galtan, galtan_salience,
         multiculturalism, multicult_salience,
         immigrate_policy, immigrate_salience)

trend_24 <- bind_rows(trend_short, df17_short, df24_short)

# map country names for later conversion into ISO codes 

country_map <- c(
  be = "Belgium",
  dk = "Denmark",
  ge = "Germany",
  ger = "Germany",
  gr = "Greece",
  esp = "Spain",
  fr = "France",
  irl = "Ireland",
  it = "Italy",
  nl = "Netherlands",
  uk = "United Kingdom",
  por = "Portugal",
  aus = "Austria",
  fin = "Finland",
  sv = "Sweden",
  swe = "Sweden",
  bul = "Bulgaria",
  cz = "Czech Republic",
  est = "Estonia",
  hun = "Hungary",
  lat = "Latvia",
  lith = "Lithuania",
  pol = "Poland",
  rom = "Romania",
  slo = "Slovakia",
  sle = "Slovenia", 
  cro = "Croatia",
  mal = "Malta",
  cyp = "Cyprus"
)

# create new full country name variable 

trend_24 <- trend_24 %>% 
  mutate(country_full = country_map[country])

table(trend_24$country_full, trend_24$country, useNA = "always")
  


 

# Immigration salience at survey years ---------------------------------------------------
cee_countries <- c("bul", "cz", "est", "hun", "lat", "lith", "pol", "rom", "slo", "sle", "cro")

theme_set(theme_classic2(base_size = 22))

survey_means <- trend_24 %>%
  filter(country %in% cee_countries) %>% 
  group_by(country_full, year) %>% 
  reframe(
    immigrate_mean = 
      mean(immigrate_salience, na.rm = TRUE),
    galtan_mean = 
      mean(galtan_salience, na.rm = TRUE)
  ) %>% 
  mutate(
    iso = countrycode(
    country_full,
    origin = "country.name",
    destination = "iso3c")
    )%>%
  mutate(year_short = substring(as.character(year), 3, 4),
            survey_label = paste0(iso, year_short)
  )

write_csv(survey_means, "data/ches_elections.csv")

Fig4_ches <- ggplot(data = survey_means, aes(x = year, y = immigrate_mean))+
  geom_point(aes(color = country_full)) +
  geom_text_repel(aes(label = survey_label),
                  alpha= 0.8,
                  size = 3) +
  geom_vline(xintercept = 2015, 
             linetype = "dashed") +
  geom_smooth(linetype = "dashed",
              alpha = 0.8,
              se = F,
              color = "grey") +
  annotate("text", x=2015, y=10, label="2015", angle=0) +
  scale_y_continuous(breaks = seq(0,10, by=1))+
  scale_x_continuous(breaks = seq(2006, 2020, by =2),
                     limits = c(2006,2020)) +
  theme_classic2() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x= "Year", y= "Mean salience (%)") +
  scale_color_brewer(palette = "Set2")

ggsave("output_final/Fig4A_Ches.jpg", Fig4_ches, height = 5, width = 5)






  