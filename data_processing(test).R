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
library(stargazer)
library("lmtest")
#install.packages("texreg")
odata %<>% rename (year = variable,
                   country = Country)
africa_region= odata %>% filter(Region=="Africa")
america_region= odata %>% filter(Region=="America")
asia_region= odata %>% filter(Region=="Asia/Pacific")
eu_region= odata %>% filter(Region=="Europe")

write.csv(africa_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\africa_region.csv", row.names = FALSE)
write.csv(america_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\america_region.csv", row.names = FALSE)
write.csv(asia_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\asia_region.csv", row.names = FALSE)
write.csv(eu_region,"D:\\TIU\\Fall_2020\\thesis\\thesis data\\developing\\eu_region.csv", row.names = FALSE)

#Set data as panel data
data<-pdata.frame(odata,index=c("ISO_Code","year"))

#fixed effects benchmark 
fixed_africa_bench <- plm(fertility ~ female_employment, data=africa_region, index=c("country", "year"), model="within")
#sum_africa_bench<-coeftest(fixed_africa_bench, vcov. = vcovHC, type = "HC1")

fixed_america_bench <- plm(fertility ~ female_employment, data=america_region, index=c("country", "year"), model="within")
#sum_america_bench<-coeftest(fixed_america_bench, vcov. = vcovHC, type = "HC1")

fixed_asia_bench <- plm(fertility ~ female_employment, data=asia_region, index=c("country", "year"), model="within")
#sum_asia_bench<-coeftest(fixed_asia_bench, vcov. = vcovHC, type = "HC1")

fixed_eu_bench<- plm(fertility ~ female_employment, data=eu_region, index=c("country", "year"), model="within")
#sum_eu_bench<-coeftest(fixed_eu_bench, vcov. = vcovHC, type = "HC1")

#stargazer(fixed_africa_bench, fixed_america_bench, fixed_asia_bench, fixed_eu_bench,
          #title="Benchmark Table",align=TRUE)
####### LASSO double selection

#lasso.reg = rlasso(fertility ~ female_employment+va_democracy+female_high_edu
                   #+parents_hus_wife+Latitude+as.factor(Region)+gdp_growth,odata, post = FALSE) # use lasso, not-Post-lasso
#sum.lasso <- summary(lasso.reg, all = FALSE)

#### Africa
## lasso and dlasso are for double LASSO selection 
lasso.reg_africa = rlasso(fertility ~ va_democracy+female_high_edu+female_primary_edu
                          +parents_hus_wife+gdp_growth,africa_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_africa <- summary(lasso.reg_africa, all = FALSE)

dlasso.reg_africa = rlasso(female_employment ~ va_democracy+female_high_edu+female_primary_edu
                           +parents_hus_wife+gdp_growth,africa_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_africa <- summary(dlasso.reg_africa, all = FALSE)

fixed_africa <- plm(fertility ~ female_employment+va_democracy+female_high_edu
                    +female_primary_edu+parents_hus_wife, data=africa_region, index=c("country", "year"), model="within")

fixed_africa_manual <- plm(fertility ~ female_employment+va_democracy+female_high_edu
                    +female_primary_edu+gdp_growth, data=africa_region, index=c("country", "year"), model="within")

#sum1<-coeftest(fixed_africa, vcov. = vcovHC, type = "HC1")

#sum1_manual<-coeftest(fixed_africa_manual, vcov. = vcovHC, type = "HC1")

#### America
## lasso and dlasso are for double LASSO selection 
lasso.reg_america = rlasso(fertility ~ va_democracy+female_high_edu+female_primary_edu+
                           +parents_hus_wife+gdp_growth,america_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_america <- summary(lasso.reg_america, all = FALSE)

dlasso.reg_america = rlasso(female_employment ~ va_democracy+female_high_edu+female_primary_edu
                            +parents_hus_wife+gdp_growth,america_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_america <- summary(dlasso.reg_america, all = FALSE)

fixed_america <- plm(fertility ~ female_employment+female_high_edu
                     +female_primary_edu+va_democracy, data=america_region, index=c("country", "year"), model="within")

fixed_america_manual <- plm(fertility ~ female_employment+female_high_edu
                     +female_primary_edu+gdp_growth, data=america_region, index=c("country", "year"), model="within")

#sum2 <-coeftest(fixed_america, vcov. = vcovHC, type = "HC1")

#sum2_manual <-coeftest(fixed_america_manual, vcov. = vcovHC, type = "HC1")

#### Asia
## lasso and dlasso are for double LASSO selection 
lasso.reg_asia = rlasso(fertility ~ va_democracy+female_high_edu+female_primary_edu
                        +parents_hus_wife+gdp_growth,asia_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_asia <- summary(lasso.reg_asia, all = FALSE)

dlasso.reg_asia = rlasso(female_employment ~ va_democracy+female_high_edu+female_primary_edu
                         +parents_hus_wife+gdp_growth,asia_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_asia <- summary(dlasso.reg_asia, all = FALSE)

fixed_asia <- plm(fertility ~ female_employment+female_high_edu+female_primary_edu
                  +va_democracy+parents_hus_wife, data=asia_region, index=c("country", "year"), model="within")

#sum3<-coeftest(fixed_asia, vcov. = vcovHC, type = "HC1")

fixed_asia_manual <- plm(fertility ~ female_employment+female_high_edu+female_primary_edu
                +parents_hus_wife, data=asia_region, index=c("country", "year"), model="within")

#sum3_manual <-coeftest(fixed_asia_manual, vcov. = vcovHC, type = "HC1")

#### Europe
## lasso and dlasso are for double LASSO selection 

lasso.reg_eu = rlasso(fertility ~ va_democracy+female_high_edu+female_primary_edu
                      +parents_hus_wife+gdp_growth,eu_region, post = FALSE) # use lasso, not-Post-lasso
sum.lasso_eu <- summary(lasso.reg_eu, all = FALSE)

dlasso.reg_eu = rlasso(female_employment ~ va_democracy+female_high_edu+female_primary_edu
                       +parents_hus_wife+gdp_growth,eu_region, post = FALSE) # use lasso, not-Post-lasso
sum.dlasso_eu <- summary(dlasso.reg_eu, all = FALSE)

fixed_eu<- plm(fertility ~ female_employment+va_democracy
               +parents_hus_wife+gdp_growth+female_primary_edu, data=eu_region, index=c("country", "year"), model="within")

fixed_eu_manual<- plm(fertility ~ female_employment + va_democracy
               + gdp_growth + female_high_edu, data=eu_region, index=c("country", "year"), model="within")

#sum4_mannual<-coeftest(fixed_eu_manual, vcov. = vcovHC, type = "HC1")


##### Robustness check
robust_check_africa<-lm_robust(fertility ~ female_employment+va_democracy+female_high_edu
                               +female_primary_edu+parents_hus_wife,data=africa_region,fixed_effects=~country)
robust_check_africa

robust_check_america<-lm_robust(fertility ~ female_employment+female_high_edu
                                +female_primary_edu+va_democracy,data=america_region,fixed_effects=~country)
robust_check_america

robust_check_asia<-lm_robust(fertility ~ female_employment+female_high_edu+female_primary_edu
                             +va_democracy+parents_hus_wife,data=asia_region,fixed_effects=~country)
robust_check_asia

robust_check_eu<-lm_robust(fertility ~ female_employment+va_democracy+parents_hus_wife
                           +gdp_growth+female_primary_edu,data=eu_region,fixed_effects=~country)
robust_check_eu

#stargazer(sum1,sum2,sum3,sum4, title="Including control variables", align=TRUE)

#stargazer(sum1,sum_africa_bench, title="Including control variables", align=TRUE)


