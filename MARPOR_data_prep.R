library(tidyverse)
#install.packages("countrycode")
library(countrycode)
##install.packages("manifestoR")
#install.packages("summarytools")
library(summarytools)
library(manifestoR)
library(ggpubr)



mp_setapikey("manifesto_apikey.txt")

#mp_load_cache(file = "manifesto_cache.RData")
mpds <- mp_maindataset(version = "MPDS2024a")

countries <- c("Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")
countries_2 <- c(11:14, 21:23, 31, 41, 42, 43, 51, 53)

CEE <- mpds %>% 
  filter(countryname %in% countries)

West <-  mpds %>% 
  filter(country %in% countries_2)


##aggregate categories to ensure comparability overtime 

CEE <- aggregate_pers(CEE, groups = cee_aggregation_relations(),
                       na.rm = FALSE,
                       keep = FALSE,
                       overwrite = names(groups),
                       verbose = TRUE)


West <- aggregate_pers(West, groups = v5_v4_aggregation_relations(),
                       na.rm = FALSE,
                       keep = FALSE,
                       overwrite = names(groups),
                       verbose = TRUE)



##################### Scale party-left right positions
# using logit_rile according to Lowe et al. (2011)

log_rile <- CEE %>% 
  logit_rile()

CEE <- CEE %>% 
  cbind(log_rile)

# Check coding
CEE %>% 
  filter(countryname == "Poland")%>%
  select(partyname, rile, log_rile)


## group countries into regions 
CEE <- CEE %>% 
  mutate(macro_region = case_when(
    countryname %in% c("Croatia", "Slovenia") ~ "South West", 
    countryname %in% c("Bulgaria", "Romania") ~ "South East", 
    countryname %in% c("Czech Republic", "Hungary", "Slovakia", "Poland") ~ "V4",
    countryname %in% c("Latvia", "Lithuania", "Estonia") ~ "Baltics")) %>%
  mutate(year = date %/% 100)



# creating aggregate issue categories -------------------------------------


nonecon_list <- c("foreign", "EU", "fd", "pol", "cor", "envi", 
                  "cult", "edu", "nat_multi", "mor", "law")

CEE <- CEE %>% 
  rowwise() %>%
  mutate(uncod = peruncod,
         econ = sum(c_across(c(per401:per404, per406:per415, per703, per704)), na.rm = TRUE),
         welf = sum(c_across(c(per405, per504, per505, per701, per702)), na.rm = TRUE),
         foreign = sum(c_across(c(per101:per107, per109)), na.rm = TRUE), 
         EU = sum(c_across(c(per108, per110)), na.rm = TRUE),
         fd = sum(c_across(c(per201:per204)), na.rm = TRUE),
         pol = sum(c_across(c(per301:per303, per305)), na.rm = TRUE),
         cor = per304,
         envi = sum(c_across(c(per416,per501)), na.rm = TRUE),
         cult = per502, 
         edu = sum(c_across(c(per506, per507)), na.rm = TRUE),
         nat_multi = sum(c_across(c(per601, per602, per607, per608, per705)), na.rm = TRUE),
         mor = sum(c_across(c(per603, per604)), na.rm = TRUE), 
         law = per605,
         econ_agg = sum(c_across(c(econ, welf)), na.rm = TRUE),
         cult_agg = sum(c_across(c(EU, nat_multi, mor, envi, law)), na.rm = TRUE),
         nat_multi_mor = sum(c_across(c(nat_multi, mor)), na.rm = TRUE),
         non_econ_all = sum(c_across(all_of(nonecon_list))))%>%
  ungroup()

library(lubridate)

CEE <- CEE %>% 
  mutate(year = year(ymd(edate)))


## add iso 3

CEE <- CEE %>%
  mutate(
    iso3 = countrycode(
      countryname,
      origin = "country.name",
      destination = "iso3c"
    ))



write.csv(CEE, "data/df_party_raw.csv")


# election level data -------------------------------------


df_elect <- CEE %>%
  group_by(countryname, year) %>% 
  reframe(uncod_a = mean(uncod, na.rm=T),
          econ_a = mean(econ, na.rm = T), 
          welf_a = mean(welf, na.rm = T), 
          foreign_a = mean(foreign, na.rm=T), 
          EU_a = mean(EU , na.rm=T), 
          fd_a = mean(fd, na.rm = T), 
          pol_a = mean(pol, na.rm = T),
          cor_a= mean(cor, na.rm = T), 
          envi_a = mean(envi, na.rm = T),
          cult_a = mean(cult, na.rm = T), 
          edu_a = mean(edu, na.rm = T), 
          nat_multi_a = mean(nat_multi, na.rm = T),
          mor_a = mean(mor, na.rm = T),
          law_a = mean(law, na.rm = T),
          econ_agg_a = mean(econ_agg, na.rm = T),
          cult_agg_a = mean(cult_agg, na.rm = T),
          nat_multi_mor_a = mean(nat_multi_mor, na.rm = T),
          non_econ_all_a = mean(non_econ_all, na.rm = T)) %>% 
          ungroup() %>%
          mutate(decade = paste0(floor(year / 10) * 10, "s") # add decade variable
  )

# add ISO-3 codes and election labels

library(countrycode)

#df_elect <- read.csv("data/df_elect.csv")

df_elect <- df_elect %>%
  mutate(
    iso3 = countrycode(
      countryname,
      origin = "country.name",
      destination = "iso3c"
    ),
    year_short = substring(as.character(year), 3, 4),
    elect_label = paste0(iso3, year_short)
  )


  
# export
write.csv(df_elect, "data/df_elect_raw.csv")







































West2 <- West %>% 
  rowwise() %>%
  mutate(uncod = peruncod,
         econ = sum(c_across(c(per401:per404, per406:per415, per703, per704)), na.rm = TRUE),
         welf = sum(c_across(c(per405, per504,per505, per701, per702)), na.rm = TRUE),
         foreign = sum(c_across(c(per101:per107, per109)), na.rm = TRUE), 
         EU = sum(c_across(c(per108, per110)), na.rm = TRUE),
         fd = sum(c_across(c(per201:per204)), na.rm = TRUE),
         pol = sum(c_across(c(per301:per303, per305)), na.rm = TRUE),
         cor = per304,
         envi = sum(c_across(c(per416, per501)), na.rm = TRUE),
         cult = per502, 
         edu = sum(c_across(c(per506, per507)), na.rm = TRUE),
         nat_multi = sum(c_across(c(per601, per602, per607, per608, per705)), na.rm = TRUE),
         mor = sum(c_across(c(per603, per604)), na.rm = TRUE), 
         law = per605,
         econ_agg = sum(c_across(c(econ, welf)), na.rm = TRUE),
         cult_agg = sum(c_across(c(EU, nat_multi, mor, envi, law), na.rm = TRUE))) %>%
  ungroup()



West2 <- West2 %>% 
  mutate(year = year(ymd(edate)))

print(West2[c("year", "edate")], n = 250)



write.csv(West2, "data/West_data.csv")















#election aggregates  - has to be fixed
gdf <- CEE %>%  
      group_by(countryname, year, macro_region) %>% 
      reframe(uncoda = mean(uncod, na.rm=T),
              econa = mean(econ, na.rm = T), 
              welfa = mean(welf, na.rm = T), 
              foreigna = mean(foreign, na.rm=T), 
              EUa = mean(EU , na.rm=T), 
              fda = mean(fd, na.rm = T), 
              pola = mean(pol, na.rm = T),
              cora= mean(cor, na.rm = T), 
              envia = mean(envi, na.rm = T),
              culta = mean(cult, na.rm = T), 
              edua = mean(edu, na.rm = T), 
              nata = mean(nat, na.rm = T),
              mora = mean(mor, na.rm = T),
              lawa = mean(law, na.rm = T),
              multia = mean(multi, na.rm = T),
              nonecona= mean(nonecon, na.rm = T),
              civica = mean(civic, na.rm = T), 
              econ_totala = mean(econ_total, na.rm=T), 
              cult_totala = mean(cult_total, na.rm=T))


gdf2 <- West2 %>%  
    mutate(year = date %/% 100)%>%
    group_by(countryname, year) %>% 
    reframe(uncoda = mean(uncod, na.rm=T),
            econa = mean(econ, na.rm = T), 
            welfa = mean(welf, na.rm = T), 
            foreigna = mean(foreign, na.rm=T), 
            EUa = mean(EU , na.rm=T), 
            fda = mean(fd, na.rm = T), 
            pola = mean(pol, na.rm = T),
            cora= mean(cor, na.rm = T), 
            envia = mean(envi, na.rm = T),
            culta = mean(cult, na.rm = T), 
            edua = mean(edu, na.rm = T), 
            nata = mean(nat, na.rm = T),
            mora = mean(mor, na.rm = T),
            lawa = mean(law, na.rm = T),
            multia = mean(multi, na.rm = T),
            nonecona= mean(nonecon, na.rm = T),
            civica = mean(civic, na.rm = T), 
            econ_totala = mean(econ_total, na.rm=T), 
            cult_totala = mean(cult_total, na.rm=T))


## top issues 


stacked_df <- gdf %>% 
  pivot_longer(cols = econa:civica,
               names_to = "topic", 
               values_to = "share")

top5 <- stacked_df %>% 
  group_by(countryname, year) %>% 
  arrange(desc(share), .by_group =TRUE)%>% 
  slice_max(share, n=5)%>% 
  summarise(top = list(topic))%>% 
  View()



mean(gdf$multia, na.rm=T)

## agregate categories graph
stacked_df2 <- gdf %>% 
  select(-econa, -welfa,-mora,-nata, -multia)  %>% 
  pivot_longer(cols = uncoda:cult_totala,
               names_to = "topic", 
               values_to = "share")

stacked_df3 <- gdf %>% 
  pivot_longer(cols = uncoda:cult_totala,
               names_to = "topic", 
               values_to = "share")


ggplot(stacked_df2, aes(x=year, y=share, color = topic))+
  geom_smooth(aes(linetype=topic),method = "loess", se=F, span =0.66)+ 
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "", x= "Year", y= "Share of the total agenda (%)")

##Western europe

stacked_dfw<- gdf2 %>% 
  pivot_longer(cols = econa:civica,
               names_to = "topic", 
               values_to = "share")
View(stacked_dfw)

stacked_df2w <- gdf2 %>% 
  select(-econa, -welfa,-mora,-nata, -multia)  %>% 
  pivot_longer(cols = uncoda:cult_totala,
               names_to = "topic", 
               values_to = "share")
stacked_df3w <- gdf2 %>% 
  pivot_longer(cols = uncoda:cult_totala,
               names_to = "topic", 
               values_to = "share")


top3w <- stacked_dfw %>% 
  group_by(countryname, year) %>% 
  arrange(desc(share), .by_group =TRUE)%>% 
  slice_max(share, n=3)%>% 
  summarise(top = list(topic))%>% 
  View()


stacked_df3w %>% 
  filter(topic %in% c("welfa", "econa", "cult_totala", "EUa", "envia") & year >1990) %>%
  mutate(topic = factor(topic, 
                        levels = c("welfa", "econa", "cult_totala", "EUa", "envia"), 
                        labels = c("Welfare", "Economy", "Cultural issues", "European Union", "Environment"))) %>%
  ggplot(aes(x=year, y=share, color = topic))+
  geom_smooth(aes(linetype=topic),method = "loess", se=F, span =0.8)+
  scale_y_continuous(breaks = seq(0,30, by=5), limits = c(0,30))+
  scale_colour_manual(values=cbbPalette) +
  theme_classic2(base_size = 17) +
  theme(
    strip.text = element_text(size = 17, face = "bold", color = "black"),
    axis.title.y = element_text(size = 17, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Development of issue content in Western Europe", x= "Year", y= "Share of the total agenda (%)")
ggsave("econ_cultWest1.jpg", width = 10, height = 5.625, units = "in", dpi = 300)





## econ vs welf vs cult plot 
stacked_df3 %>% 
  filter(topic %in% c("welfa", "econa", "cult_totala", "EUa", "envia")) %>%
  mutate(topic = factor(topic, 
                        levels = c("welfa", "econa", "cult_totala", "EUa", "envia"), 
                        labels = c("Welfare", "Economy", "Cultural issues(", "European Union", "Environment"))) %>%
  ggplot(aes(x=year, y=share, color = topic))+
  geom_smooth(aes(linetype=topic),method = "loess", se=F, span =0.8)+ 
  scale_y_continuous(breaks = seq(0,30, by=5))+
  scale_colour_manual(values=cbbPalette)+
  theme_classic2(base_size = 17) +
  theme(
    strip.text = element_text(size = 17, face = "bold", color = "black"),
    axis.title.y = element_text(size = 17, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Development of issue content in CEE", x= "Year", y= "Share of the total agenda (%)")
ggsave("plots/econ_cultCEE_1.jpg", width = 10, height = 5.625, units = "in", dpi = 300)



stacked_df3w %>% 
  filter(topic %in% c("welfa", "econa", "cult_totala", "EUa", "envia") & year >1990) %>%
  mutate(topic = factor(topic, 
                        levels = c("welfa", "econa", "cult_totala", "EUa", "envia"), 
                        labels = c("Welfare", "Economy", "Cultural issues", "European Union", "Environment"))) %>%
  ggplot(aes(x=year, y=share, color = topic))+
  geom_smooth(aes(linetype=topic),method = "loess", se=F, span =0.8)+
  scale_y_continuous(breaks = seq(0,30, by=5), limits = c(0,30))+
  scale_colour_manual(values=cbbPalette) +
  theme_classic2(base_size = 17) +
  theme(
    strip.text = element_text(size = 17, face = "bold", color = "black"),
    axis.title.y = element_text(size = 17, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Development of issue content in Western Europe", x= "Year", y= "Share of the total agenda (%)")
ggsave("econ_cultWest.jpg", width = 10, height = 5.625, units = "in", dpi = 300)




## party level analysis
CEE3 %>% 
  filter(is.na(pervote)) %>% 
  View()

CEE3_long <- CEE3 %>% 
  pivot_longer(cols = c(cult_total, econ_total),
               names_to = "topic", 
               values_to = "share")%>% 
  mutate(topic = factor(topic, levels = c("cult_total", "econ_total"), 
                        labels = c("Cultural issues", "Economic Issues")))

box1 <-  CEE3_long %>% 
    filter(year <2001)%>% 
      ggplot(aes(topic, share)) + 
      geom_boxplot() + 
      labs(title = "1990s", x="", y= "share") +
      scale_y_continuous(breaks = seq(0,80, by=20), limits = c(0,80))+
  theme_pubr()

box2 <-  CEE3_long %>% 
  filter(year %in% 2001:2010)%>% 
  ggplot(aes(topic, share)) + 
  geom_boxplot() +
  labs(title = "2000s", x="", y= "") +
  theme_pubr()

box3 <-  CEE3_long %>% 
  filter(year > 2010)%>% 
  ggplot(aes(topic, share)) + geom_boxplot() + labs(title = "2010s", x="", y= "")+
  scale_y_continuous(breaks = seq(0,80, by=20), limits = c(0,80))+
  theme_pubr()
 
boxdecades <- ggarrange(box1, box2, box3,
                                   ncol = 3, nrow = 1,
                                   common.legend = T, legend = "bottom")

boxdecades
ggsave("boxdecades.jpg", width = 10, height = 5.625, units = "in", dpi = 300)






##alternative ethnic conflict measure based on Stoll (2010)













## simple graphs 
econplot <- ggplot(gdf, aes(x=year,y=econa, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  scale_color_viridis_d()+
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of economic issues", x= "Year", y= "Share of the total agenda (%)")
econplot
ggsave("plot1.jpg", width = 10, height = 5.625, units = "in", dpi = 400)

cult_totalplot <- ggplot(gdf, aes(x=year,y=cult_totala, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  scale_color_viridis_d()+
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of cultural issues", x= "Year", y= "Share of the total agenda (%)")
cult_totalplot
ggsave("plot_cult_total.jpg", width = 10, height = 5.625, units = "in", dpi = 300)





newplot <- ggplot(gdf, aes(x=year,y=(multia + EUa + envia + nata + mora), color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of EU, Multiculturalism, Morality, Nationalism, Environment", x= "Year", y= "Share of the total agenda (%)")
newplot



EUplot <- ggplot(gdf, aes(x=year,y=EUa, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of EU", x= "Year", y= "Share of the total agenda (%)")

EUplot


welfplot <- ggplot(gdf, aes(x=year,y=welfa, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of welfare", x= "Year", y= "Share of the total agenda (%)")

welfplot


head(gdf)

enviplot <- ggplot(gdf, aes(x=year,y=envia, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of Environmentalism", x= "Year", y= "Share of the total agenda (%)")
enviplot


mnwlplot <- gdf%>% 
  filter(year > 2010)%>%
  ggplot(aes(x=year,y=multia + nata, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =1)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  scale_y_continuous(breaks= c(2,6,10,14)) + 
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of multiculturalism + national way of life", x= "Year", y= "Share of the total agenda (%)")
mnwlplot

ggsave("plot2.jpg", width = 10, height = 5.625, units = "in", dpi = 400)

euplot <- ggplot(gdf, aes(x=year,y=EUa, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of the EU", x= "Year", y= "Share of the total agenda (%)")
euplot

welfplot <- ggplot(gdf, aes(x=year,y=EUa, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of the EU", x= "Year", y= "Share of the total agenda (%)")
euplot

View(gdf)

unplot <- ggplot(gdf, aes(x=year,y=uncoda, color =countryname)) +
  geom_smooth(aes(linetype=countryname), method = "loess", se=F, span =0.8)+ 
  facet_wrap(~ macro_region, nrow =2, ncol = 2) +
  theme_classic2(base_size = 20) +
  theme(
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(size = rel(0.8))) + 
  labs(title = "Salience of uncoded issues", x= "Year", y= "Share of the total agenda (%)")
unplot




