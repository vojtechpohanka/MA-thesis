library(data.table)
library(tidyverse)
library(patchwork)
library(lubridate)
library(ggpubr)
library(ggbeeswarm)


df_party_month <- fread("data/df_sal_party_monthly.csv")

df_party_year <- fread("data/df_sal_party_yearly.csv")


# code parties into groups
df_party_year <- df_party_year %>%
  mutate(
    group = case_when(
      party_fix %in% c("ČSSD") ~ "Social Democrats",
      party_fix %in% c("LB", "KSČM") ~ "Communists",
      party_fix %in%
        c("ODS", "KDU-ČSL", "US", "US-DEU", "ODA", "TOP 09", "KDS") ~
        "Centre right",
      party_fix %in% c("SPR-RSČ", "Úsvit", "SPD") ~ "Radical right",
      party_fix %in% c("ANO", "STAN", "VV", "Piráti") ~ "New parties",
      TRUE ~ "Other"
    ),
    group_alt = case_when(
      # alternate grouping
      party_fix %in% c("ČSSD") ~ "Social Democrats",
      party_fix %in% c("LB", "KSČM") ~ "Communists",
      party_fix %in% c("ODS", "US", "US-DEU", "ODA", "TOP 09") ~ "Centre right",
      party_fix %in% c("KDU-ČSL", "KDS") ~ "Christian Democrat",
      party_fix %in% c("SPR-RSČ", "Úsvit", "SPD") ~ "Radical right",
      party_fix == "ANO" ~ "ANO",
      party_fix == "STAN" ~ "STAN",
      party_fix == "Piráti" ~ "Piráti",
      party_fix == "VV" ~ "VV",
      TRUE ~ "Other"
    )
  )


df_party_month <- df_party_month %>%
  mutate(
    group = case_when(
      party_fix %in% c("ČSSD") ~ "Social Democrats",
      party_fix %in% c("LB", "KSČM") ~ "Communists",
      party_fix %in%
        c("ODS", "KDU-ČSL", "US", "US-DEU", "ODA", "TOP 09", "KDS") ~
        "Centre right",
      party_fix %in% c("SPR-RSČ", "Úsvit", "SPD") ~ "Radical right",
      party_fix %in% c("ANO", "STAN", "VV", "Piráti") ~ "New parties",
      TRUE ~ "Other"
    )
  )

#data prep - calculate overall mean salience

column_names <- df_party_year %>%
  select(econ:religion) %>%
  names()

df_agg_year <- df_party_year %>%
  select(-islam) %>%
  group_by(year) %>%
  summarise(
    across(all_of(column_names), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )

# long format dataframe
# overall
df_agg_year <- df_agg_year %>%
  pivot_longer(cols = econ:religion, names_to = "issue", values_to = "salience")


# yearly means for party groups
df_group_year <- df_party_year %>%
  select(-islam) %>%
  group_by(group, year) %>%
  summarise(
    across(all_of(column_names), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = econ:religion,
    names_to = "issue",
    values_to = "salience"
  )

# monthly means for party groups

df_month_long <- df_party_month %>%
  group_by(group, month) %>%
  summarise(
    across(econ:islam, ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = econ:islam,
    names_to = "issue",
    values_to = "salience"
  )

# yearly means for parties - alternate
df_partydis_year <- df_party_year %>%
  select(-islam) %>%
  group_by(group_alt, year) %>%
  summarise(
    across(all_of(column_names), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = econ:religion,
    names_to = "issue",
    values_to = "salience"
  )


df_agg_year_recoded <- df_agg_year %>%
  mutate(
    issue = fct_recode(
      issue,
      "Economy" = "econ",
      "Nation" = "nation",
      "Minorities" = "minorities",
      "Multiculturalism" = "multicult",
      "Immigration" = "immigration",
      "LGBTQ" = "lgbt",
      "Gender" = "gender",
      "Reproductive rights" = "reproductive",
      "Family" = "family",
      "Religion" = "religion"
    )
  ) %>%
  mutate(issue_ordered = fct_reorder(issue, desc(salience), .fun = mean))


## plot overall salience by issue - Figure 10

facet_overall <- ggplot(
  df_agg_year_recoded,
  aes(x = year, y = salience * 100)
) +
  geom_line() +
  scale_x_continuous(
    limits = c(1993, 2023),
    breaks = seq(1993, 2023, by = 5),
    minor_breaks = waiver()
  ) +
  #geom_smooth(span = 0.1) +
  facet_wrap(~issue_ordered, scales = "free_y") +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.minor.ticks.x.bottom = element_line()
  ) +
  labs(x = "Year", y = "Mean salience (%)")

ggsave(
  "output_final/Fig_facet_overall.jpg",
  facet_overall,
  height = 7,
  width = 10
)


# summary table
df_agg_year_recoded %>%
  arrange(issue, year) %>%
  filter(issue == "Minorities")


# party level trends ------------------------------------------------------

# data prep
df_group_year_recoded <- df_group_year %>%
  mutate(
    issue = fct_recode(
      issue,
      "Economy" = "econ",
      "Nation" = "nation",
      "Minorities" = "minorities",
      "Multiculturalism" = "multicult",
      "Immigration" = "immigration",
      "LGBTQ" = "lgbt",
      "Gender" = "gender",
      "Reproductive rights" = "reproductive",
      "Family" = "family",
      "Religion" = "religion"
    )
  ) %>%
  mutate(issue_ordered = fct_reorder(issue, desc(salience), .fun = max))


df_group_means <- df_group_year_recoded %>%
  group_by(group, issue) %>%
  summarise(
    mean = mean(salience * 100),
    sd = sd(salience * 100),
    se = sd(salience * 100) / sqrt(length(salience * 100)),
    .groups = "drop"
  ) %>%
  mutate(
    issue_ordered = fct_reorder(issue, desc(mean), .fun = max)
  )


df_group_means_post2010 <- df_group_year_recoded %>%
  filter(year >= 2010) %>%
  group_by(group, issue) %>%
  summarise(
    mean = mean(salience * 100),
    sd = sd(salience * 100),
    se = sd(salience * 100) / sqrt(length(salience * 100)),
    .groups = "drop"
  ) %>%
  mutate(
    issue_ordered = fct_reorder(issue, desc(mean), .fun = max)
  )


df_party_means <- df_partydis_year %>%
  group_by(group_alt, issue) %>%
  summarise(
    mean = mean(salience * 100),
    sd = sd(salience * 100),
    se = sd(salience * 100) / sqrt(length(salience * 100)),
    .groups = "drop"
  ) %>%
  mutate(
    issue_ordered = fct_reorder(issue, desc(mean), .fun = max)
  )


df_party_means %>%
  filter(group_alt %in% c("ANO", "STAN", "Piráti", "VV")) %>%
  head(20)


## plot overall party means - Figure 11

overall_party <- df_group_means %>%
  filter(group != "Other") %>%
  ggplot(aes(x = group, y = mean, color = group, shape = group)) +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = .2,
    position = position_dodge(0.05)
  ) +
  #geom_beeswarm(alpha = 0.4, method = "compactswarm")+
  #geom_boxplot(alpha = 0, width = 0.3, notch = F, aes(color= group))+
  facet_wrap(~issue_ordered, scales = "free_y") +
  theme_pubr() +
  theme(axis.text.x = element_blank(), legend.position = "top") +
  labs(x = "", y = "Mean salience (%)")


ggsave(
  "output_final/Fig_party_points.jpg",
  overall_party,
  height = 7,
  width = 10
)


## plot monthly trends for immigration  - Figure 12

# create duplicate df for background plotting

plot_df1 <- df_month_long %>%
  filter(issue == "immigration", group != "Other", month >= "2013-01-01")

plot_df2 <- df_month_long %>%
  filter(issue == "immigration", group != "Other", month >= "2013-01-01") %>%
  rename(party_group = group)


facet_party <- ggplot() +
  stat_smooth(
    data = plot_df2,
    aes(x = month, y = salience * 100, group = party_group),
    geom = "line",
    color = "grey",
    linetype = "twodash",
    span = 0.1,
    se = F
  ) +
  geom_smooth(
    data = plot_df1,
    aes(
      x = month,
      y = salience * 100,
      linetype = "solid",
      color = group,
      fill = group
    ),
    se = F,
    span = 0.1
  ) +
  facet_wrap(~group) +
  geom_vline(xintercept = as.Date("2015-08-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-02-01"), linetype = "dotted") +
  scale_x_date(
    date_labels = "%Y",
    breaks = seq(as.Date("2013-01-01"), as.Date("2023-12-01"), by = "1 year")
  ) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  labs(y = "Monthly salience (%)", x = "Date")

ggsave("output_final/Fig_facet_party.jpg", facet_party, height = 7, width = 10)


## issues and parties temporally - Appendix No. 11

df_group_year %>%
  filter(group != "Other" & year >= 2013) %>%
  ggplot(aes(x = year, y = salience * 100, color = group)) +
  geom_line(linewidth = 0.6) +
  scale_x_continuous(,
    breaks = seq(2013, 2023, by = 2)
  ) +
  facet_wrap(~issue, scales = "free_y") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Salience (%)", x = "Year")

ggsave(
  "output_final/Fig_facet_issue_party.png",
  plot = last_plot(),
  height = 7,
  width = 10
)
