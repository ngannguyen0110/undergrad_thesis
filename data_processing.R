#clear everything out
rm(list=ls())

# Set directory(this should be set to user's own directory)
setwd("D:/TIU/Fall_2020/thesis/thesis data/developing")
odata=read.csv(file = "official_data.csv",header=TRUE)

odata$Latitude <-as.double(odata$Latitude)
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library("plm")
library("hdm")
library("estimatr")
#install.packages("stargazer")
library(stargazer)
library("lmtest")
odata %<>% rename (year = variable,
                  country = Country)
eu_region= odata %>% filter(Region=="Europe")
asia_region= odata %>% filter(Region=="Asia/Pacific")
america_region= odata %>% filter(Region=="America")
africa_region= odata %>% filter(Region=="Africa")

write.csv(eu_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\eu_region.csv", row.names = FALSE)
write.csv(asia_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\asia_region.csv", row.names = FALSE)
write.csv(america_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\america_region.csv", row.names = FALSE)
write.csv(africa_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\africa_region.csv", row.names = FALSE)

#Set data as panel data
data<-pdata.frame(odata,index=c("ISO_Code","year"))

#fixed effects benchmark 
fixed_africa_bench <- plm(fertility ~ female_employment, data=africa_region, index=c("country", "year"), model="within")
sum_africa_bench<-coeftest(fixed_africa_bench, vcov. = vcovHC, type = "HC1")

fixed_asia_bench <- plm(fertility ~ female_employment, data=asia_region, index=c("country", "year"), model="within")
sum_asia_bench<-coeftest(fixed_asia_bench, vcov. = vcovHC, type = "HC1")

fixed_america_bench <- plm(fertility ~ female_employment, data=america_region, index=c("country", "year"), model="within")
sum_america_bench<-coeftest(fixed_america_bench, vcov. = vcovHC, type = "HC1")

fixed_eu_bench<- plm(fertility ~ female_employment, data=eu_region, index=c("country", "year"), model="within")
sum_eu_bench<-coeftest(fixed_eu_bench, vcov. = vcovHC, type = "HC1")

stargazer(sum_africa_bench,sum_asia_bench,sum_america_bench,sum_eu_bench, title="Benchmark Table",align=TRUE)


####### LASSO double selection

lasso.reg = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                   +parents_hus_wife+Latitude+as.factor(Region)+gdp_per_cap,odata, post = FALSE) # use lasso, not-Post-lasso
sum.lasso <- summary(lasso.reg, all = FALSE)

lasso.reg_eu = rlasso(fertility ~ va_democracy+female_high_edu
                   +parents_hus_wife+Latitude+gdp_growth,eu_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_eu <- summary(lasso.reg_eu, all = FALSE)

dlasso.reg_eu = rlasso(female_employment ~ va_democracy+female_high_edu
                      +parents_hus_wife+Latitude+gdp_growth,eu_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_eu <- summary(dlasso.reg_eu, all = FALSE)

lasso.reg_asia = rlasso(fertility ~ va_democracy+female_high_edu
                      +parents_hus_wife+Latitude+gdp_per_cap,asia_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_asia <- summary(lasso.reg_asia, all = FALSE)

dlasso.reg_asia = rlasso(female_employment ~ va_democracy+female_high_edu
                        +parents_hus_wife+Latitude+gdp_growth,asia_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_asia <- summary(dlasso.reg_asia, all = FALSE)

lasso.reg_africa = rlasso(fertility ~ va_democracy+female_high_edu
                        +parents_hus_wife+Latitude+gdp_growth,africa_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_africa <- summary(lasso.reg_africa, all = FALSE)

dlasso.reg_africa = rlasso(female_employment ~ va_democracy+female_high_edu
                          +parents_hus_wife+Latitude+gdp_growth,africa_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_africa <- summary(dlasso.reg_africa, all = FALSE)

lasso.reg_america = rlasso(fertility ~ va_democracy+female_high_edu
                          +parents_hus_wife+Latitude+gdp_growth,america_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_america <- summary(lasso.reg_america, all = FALSE)

dlasso.reg_america = rlasso(female_employment ~ va_democracy+female_high_edu
                           +parents_hus_wife+Latitude+gdp_growth,america_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_america <- summary(dlasso.reg_america, all = FALSE)

stargazer(sum.lasso_africa,sum.lasso_asia,sum.lasso_america,sum.lasso_america, sum.lasso_eu, title="LASSO",align=TRUE)

stargazer(sum.dlasso_africa,sum.dlasso_asia,sum.dlasso_america,sum.dlasso_america, sum.dlasso_eu, title="Double LASSO",align=TRUE)

##### Robustness check
robust_check_eu<-lm_robust(fertility ~ female_employment+va_democracy+female_high_edu
                           +parents_hus_wife+Latitude+gdp_growth,data=eu_region,fixed_effects=~country)
robust_check_eu

robust_check_asia<-lm_robust(fertility ~ female_employment+va_democracy+female_high_edu
                             +parents_hus_wife+Latitude+gdp_growth,data=asia_region,fixed_effects=~country)
robust_check_asia

robust_check_america<-lm_robust(fertility ~ female_employment+va_democracy+female_high_edu
                                +parents_hus_wife+Latitude+gdp_growth,data=america_region,fixed_effects=~country)
robust_check_america

robust_check_africa<-lm_robust(fertility ~ female_employment+va_democracy+female_high_edu
                               +parents_hus_wife+Latitude+gdp_growth,data=africa_region,fixed_effects=~country)
robust_check_africa

########### Postlasso
#plasso.reg = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                         #+parents_hus_wife+Latitude+as.factor(Region),odata, post = TRUE) # use Post-lasso
#sum.plasso <- summary(plasso.reg, all = FALSE)

#plasso.reg_asia = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                   #+parents_hus_wife+Latitude,asia_region, post = TRUE) # use Post-lasso
#sum.plasso_asia <- summary(plasso.reg_asia, all = FALSE)

#plasso.reg_africa = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                         #+parents_hus_wife+Latitude,africa_region, post = TRUE) # use -Post-lasso
#sum.plasso_africa <- summary(plasso.reg_africa, all = FALSE)

#plasso.reg_america = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                            #+parents_hus_wife+Latitude,america_region, post = TRUE) # use -Post-lasso
#sum.plasso_america <- summary(plasso.reg_america, all = FALSE)

#plasso.reg_eu = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                            #+parents_hus_wife+Latitude,eu_region, post = TRUE) # use -Post-lasso
#sum.plasso_eu <- summary(plasso.reg_eu, all = FALSE)
################

ols_eu <-lm(fertility ~ female_employment+va_democracy+female_high_edu
         +parents_hus_wife+Latitude, data=eu_region)
summary(ols_eu)
ols_asia <-lm(fertility ~ female_employment+va_democracy+female_high_edu
                +live_with_parents_husband+live_with_parents_wife+Latitude, data=asia_region)
summary(ols_asia)
ols_america <-lm(fertility ~ female_employment+va_democracy+female_high_edu
              +live_with_parents_husband+live_with_parents_wife+Latitude, data=america_region)
summary(ols_america)
ols_africa <-lm(fertility ~ female_employment+va_democracy+female_high_edu
              +live_with_parents_husband+live_with_parents_wife+Latitude, data=africa_region)
summary(ols_africa)
#coplot(fertility ~ year|Country,type="b",data=data)
#ols <-lm(fertility ~ female_employment+va_democracy+female_high_edu
         #+live_with_parents_husband+live_with_parents_wife+as.factor(Region)+Latitude, data=data)
#summary(ols)
#Pooled PLS
#pooled_africa <- plm(fertility ~ female_employment+va_democracy+gdp_per_cap+female_high_edu
                    #+parents_hus_wife+Latitude, data=africa_region, index=c("country", "year"), model="pooling")
#suma<-coeftest(pooled_africa, vcov. = vcovHC, type = "HC1")

#pooled_asia <- plm(fertility ~ female_employment+female_high_edu
                  #+gdp_per_cap+Latitude, data=asia_region, index=c("country", "year"), model="pooling")
#sumb<-coeftest(pooled_asia, vcov. = vcovHC, type = "HC1")

#pooled_america <- plm(fertility ~ female_employment+female_high_edu
                     #+gdp_per_cap+parents_hus_wife, data=america_region, index=c("country", "year"), model="pooling")
sumc<-coeftest(pooled_america, vcov. = vcovHC, type = "HC1")

#pooled_eu<- plm(fertility ~ va_democracy+parents_hus_wife+Latitude+gdp_per_cap, data=eu_region, index=c("country", "year"), model="pooling")
#sumd<-coeftest(pooled_eu, vcov. = vcovHC, type = "HC1")


#fixed effects estimator
fixed_africa <- plm(fertility ~ female_employment+va_democracy+gdp_per_cap+female_high_edu
                     +parents_hus_wife+Latitude, data=africa_region, index=c("country", "year"), model="within")
sum1<-coeftest(fixed_africa, vcov. = vcovHC, type = "HC1")

fixed_asia <- plm(fertility ~ female_employment+gdp_per_cap+female_high_edu
                   +Latitude, data=asia_region, index=c("country", "year"), model="within")
sum2<-coeftest(fixed_asia, vcov. = vcovHC, type = "HC1")

fixed_america <- plm(fertility ~ female_employment+female_high_edu
                      +gdp_per_cap+parents_hus_wife, data=america_region, index=c("country", "year"), model="within")
sum3<-coeftest(fixed_america, vcov. = vcovHC, type = "HC1")

fixed_eu<- plm(fertility ~ female_employment+va_democracy+parents_hus_wife+Latitude+gdp_per_cap, data=eu_region, index=c("country", "year"), model="within")

sum4<-coeftest(fixed_eu, vcov. = vcovHC, type = "HC1")

#stargazer(suma,sumb,sumc,sumd, title="Results",align=TRUE)

stargazer(sum1,sum2,sum3,sum4, title="Results",align=TRUE)

#LM test fixed and pooling model
#pFtest(fixed_africa, pooled_africa)

#random_africa <- plm(fertility ~ female_employment+va_democracy+female_high_edu
              #+live_with_parents_husband+live_with_parents_wife+Latitude, data=africa_region, index=c("country", "year"), model="random")
#summary(random_africa)

#random_asia <- plm(fertility ~ female_employment+va_democracy+female_high_edu
                     #+live_with_parents_husband+live_with_parents_wife+Latitude, data=asia_region, index=c("country", "year"), model="random")
#summary(random_asia)

#random_america <- plm(fertility ~ female_employment+va_democracy+female_high_edu
                     #+live_with_parents_husband+live_with_parents_wife+Latitude, data=america_region, index=c("country", "year"), model="random")
#summary(random_america)

#random_eu <- plm(fertility ~ female_employment+va_democracy+female_high_edu
                      #+live_with_parents_husband+live_with_parents_wife+Latitude, data=eu_region, index=c("country", "year"), model="random")
#summary(random_eu)

#phtest(fixed_america, random_america)
