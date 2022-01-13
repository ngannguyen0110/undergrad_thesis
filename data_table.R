library("sandwich")
library("plm")
library("stargazer")

# Regression    

fixed_africa <- plm(fertility ~ female_employment+va_democracy+female_high_edu
                    +female_primary_edu+parents_hus_wife, 
                    data=africa_region, index=c("country", "year"), model="within")

fixed_america <- plm(fertility ~ female_employment+female_high_edu
                     +female_primary_edu+va_democracy, data=america_region, 
                     index=c("country", "year"), model="within")

fixed_asia <- plm(fertility ~ female_employment+female_high_edu+female_primary_edu
                  +va_democracy+parents_hus_wife, data=asia_region, 
                  index=c("country", "year"), model="within")

fixed_eu<- plm(fertility ~ female_employment+va_democracy
               +parents_hus_wife+gdp_growth+female_primary_edu, data=eu_region,
               index=c("country", "year"), model="within")

# Adjust standard errors
cov_africa         <- vcovHC(fixed_africa, type = "HC1")
robust_se_africa    <- sqrt(diag(cov_africa))

cov_africa_manual         <- vcovHC(fixed_africa_manual, type = "HC1")
robust_se_africa_manual    <- sqrt(diag(cov_africa_manual))

cov_america         <- vcovHC(fixed_america, type = "HC1")
robust_se_america    <- sqrt(diag(cov_america))

cov_america_manual         <- vcovHC(fixed_america_manual, type = "HC1")
robust_se_america_manual    <- sqrt(diag(cov_america_manual))

cov_asia         <- vcovHC(fixed_asia, type = "HC1")
robust_se_asia    <- sqrt(diag(cov_asia))

cov_asia_manual  <- vcovHC(fixed_asia_manual, type = "HC1")
robust_se_asia_manual  <- sqrt(diag(cov_asia_manual))

cov_eu        <- vcovHC(fixed_eu, type = "HC1")
robust_se_eu    <- sqrt(diag(cov_eu))

cov_eu_manual       <- vcovHC(fixed_eu_manual, type = "HC1")
robust_se_eu_manual    <- sqrt(diag(cov_eu_manual))


#Africa
stargazer(fixed_africa_bench, fixed_africa_manual,fixed_africa_manual, fixed_africa, fixed_africa,type="text",
          title="Africa", se = list(NULL,NULL,robust_se_africa_manual, NULL,robust_se_africa),align=TRUE)

#America
stargazer(fixed_america_bench, fixed_america_manual,fixed_america_manual, fixed_america, fixed_america,type="text",
          title="America", se = list(NULL,NULL,robust_se_america_manual, NULL,robust_se_america),align=TRUE)

#Asia
stargazer(fixed_asia_bench, fixed_asia_manual,fixed_asia_manual, fixed_asia, fixed_asia,type="text",
          title="Asia", se = list(NULL,NULL,robust_se_asia_manual, NULL,robust_se_asia),align=TRUE)

#Europe
stargazer(fixed_eu_bench, fixed_eu_manual,fixed_eu_manual, fixed_eu, fixed_eu,type="text",
          title="Europe", se = list(NULL,NULL,robust_se_eu_manual, NULL,robust_se_eu),align=TRUE)

stargazer(fixed_eu_bench, fixed_america_bench,fixed_asia_bench, fixed_eu_bench,type="text",
          title="Benchmark Table",align=TRUE)

