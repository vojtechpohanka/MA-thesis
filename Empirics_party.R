library(tidyverse)
library(flexplot)
library(ggrepel)

df <- read.csv("data/df_party_coded.csv")


df %>% 
  select(partyname, parfam, farright) %>%
  view()

# scatter plot - aggregate cultural issues 
plot(cult_agg ~ log_rile , data = df)
# nationalism, multiculturalism... 

df %>% 
  ggplot(aes(log_rile, nat_multi)) + 
  geom_point() + 
  geom_text_repel(
    aes(label = partyabbrev),
    alpha = 0.2
  )

# radical-right vs rest 
df %>% 
  ggplot(aes(x = as.factor(farright), y= nat_multi)) + 
  geom_boxplot()
