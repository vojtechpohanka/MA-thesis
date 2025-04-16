library(tidyverse)
library(car)
library(fixest)
library(plm) 
library(gplots) 
library(tseries) 
library(lmtest)
#install.packages("kim")
library(kim)
## Radical right analysis

df <- read.csv("data/CEE_final.csv")

unique(df$year)

?plm



#diagnostics

dfc <-  df%>% 
  filter(Country == "Czech Republic") 

coplot(cult_total ~ year | partyname, type="l", data =dfc)
scatterplot(cult_total ~ party, data=dfc)

anova<-aov(law~ party, data=df) #run ANOVA of unemployment by unit variable 'cid'
summary(anova) #view results


df1 <- df%>% 
  filter(!farright==1)


#simple OLS models
ols<- lm(nat ~ nat_lag +lagged_far_right_vote, data = df1)
summary(ols)

ols<- lm(cult_total ~ cult_total_lag +lagged_far_right_vote, data = df1)
summary(ols)

ols<- lm(multi ~ multi_lag + (lagged_far_right_vote), data = df1)
summary(ols)

ols<- lm(law ~ law_lag +lagged_far_right_vote, data = df1)
summary(ols)


# fixed effects

model<- feols(multi ~ multi_lag +lagged_far_right_vote | party, data = df1)
summary(model)


model<- plm(law ~ law_lag + lagged_far_right_vote, model = "within", 
            index = c("party", "year"), data = df1)
summary(model)


model<-  feols(cult_total ~ lagged_far_right_vote + cult_total_lag | party, data = df1)
summary(model)

model<-  feols(cult_total ~ log(lagged_far_right_vote) + cult_total_lag | party, data = df1)
summary(model)

# random effects 

re<-plm(nat ~ nat_lag +lagged_far_right_vote, data=df1, index=c("party","year"), model="random")
summary(re)

re<-plm(cult_total ~ cult_total_lag +lagged_far_right_vote, data=df1, index=c("party","year"), model="random")
summary(re)

re<-plm(multi ~ multi_lag +lagged_far_right_vote, data=df1, index=c("party","year"), model="random")
summary(re)

re<-plm(law ~ law_lag +lagged_far_right_vote, data=df1, index=c("party","year"), model="random")
summary(re)

phtest(model,re)

# PCSE 
library(pcse)

ols<- lm(law ~ law_lag +lagged_far_right_vote, data = df1)
summary(ols)

psce(ols, groupN=df1$party, groupT=df1$year)




