#clear everything out
rm(list=ls())
  
# Set directory(this should be set to user's own directory)
setwd("D:/TIU/Fall_2020/thesis/thesis data/developing/R_using_data")
  
# Load the data
library(reshape2)
library(tidyverse)
library(dplyr)
library(magrittr)

df=read.csv(file = "fertility_developing_1991_2018.csv",header=TRUE)
df=melt(df,id.vars=c("Country","ISO_Code"))
df<-df[order(df$ISO_Code),]
colnames(df)[ncol(df)]<-"fertility"
df %<>% mutate(lookup=paste(variable,ISO_Code))
# pipe %>%; %<>%
# assign a data.frame to data argument %$%]
va_democracy=read.csv(file = "VA_democracy_index.csv",header=TRUE)
parents_husband=read.csv(file = "live_with_parents_husband.csv",header=TRUE)
parents_wife=read.csv(file = "live_with_parents_wife.csv",header=TRUE)
female_employment=read.csv(file = "female_employment_1991_2018.csv",header=TRUE)
female_high_edu=read.csv(file = "female_high_edu.csv",header=TRUE)
gdp_per_cap=read.csv(file = "ln_GDP_per_capital_1991_2018.csv",header=TRUE)
f <- function(x,name_variable){
    x=melt(x,id.vars=c("Country","ISO_Code"))
    colnames(x)[ncol(x)]<-name_variable
    x %<>% mutate(lookup=paste(variable,ISO_Code))
    #x<-getTitle(x)
    x<-select(x,-c(Country,ISO_Code,variable))
    x<<-x
  }
wife <- function(x,name_variable,y){
    x=melt(x,id.vars=c("Country","ISO_Code","Region","Latitude"))
    colnames(x)[ncol(x)]<-name_variable
    x %<>% mutate(lookup=paste(variable,ISO_Code))
    #x<-getTitle(x)
    x<-select(x,-c(Country,ISO_Code,variable))
    x<<-x
}
f(parents_husband,"live_with_parents_husband")
parents_husband<-x
f(va_democracy,"va_democracy")
va_democracy<-x
f(female_employment,"female_employment")
female_employment<-x
f(gdp_per_cap,"gdp_per_cap")
gdp_per_cap<-x
f(female_high_edu,"female_high_edu")
female_high_edu<-x
wife(parents_wife,"live_with_parents_wife")
parents_wife<-x

##### Merge data tables
df %<>%
  full_join(female_employment,by="lookup") %>%
  full_join(female_high_edu,by="lookup") %>%
  full_join(va_democracy,by="lookup") %>%
  full_join(parents_husband,by="lookup") %>%
  full_join(gdp_per_cap,by="lookup") %>%
  full_join(parents_wife,by="lookup")

df %<>%mutate(parents_hus_wife=(df$live_with_parents_husband+df$live_with_parents_wife)/2)
  
#df$female_employment<-as.numeric(df$female_employment)
#df$va_democracy <-as.numeric(df$va_democracy)
df$va_democracy <-as.character(df$va_democracy)
df$va_democracy <- as.numeric(df$va_democracy)

df$Latitude <-as.character(df$Latitude)
df$Latitude <- as.numeric(df$Latitude)

df$female_employment <- as.numeric(df$female_employment)

for (i in (1:nrow(df))) {
  if (df$female_high_edu[i]==0) {
    df$female_high_edu[i]<-NA
    }
}

for (i in (1:nrow(df))) {
  if (df$gdp_per_cap[i]==0) {
    df$gdp_per_cap[i]<-NA
  }
}

for (i in (1:nrow(df))) {
  if (df$va_democracy[i]=="#N/A") {
    df$va_democracy[i]<-NA
  }
}

va_edited<-c("X1996","X1998","X2000","X2002","X2003","X2004","X2005")

##### Adjust the data of VA_democracy (before 2006)

#Find difference
hello<-subset(df,variable=="X2006")
bye<-subset(df,variable=="X2005")
va_edited<-subset(df,variable=="X1996"|variable=="X1998"|
                  variable=="X2000"|variable=="X2002"|
                    variable=="X2003"|variable=="X2004"|variable=="X2005")

diffc<-c()
for (i in (1:nrow(hello))){
  for (j in (1:nrow(bye)))
  {
    if (hello$ISO_Code[i]==bye$ISO_Code[j]){
      diffc<-c(diffc,hello$va_democracy[i]-bye$va_democracy[j])
      bye$diff[i]<-hello$va_democracy[i]-bye$va_democracy[j]
    }
  }
}
for (i in (1:nrow(va_edited))){
  for (j in (1:nrow(bye))){
    if ((va_edited$Country[i]==bye$Country[j]) & (is.na(va_edited$va_democracy[i])==FALSE)){
      va_edited$va_democracy[i]<-va_edited$va_democracy[i]+bye$diff[j]
    }
  }
}
for (i in (1:nrow(va_edited))){
  for (j in (1:nrow(df))){
    if (va_edited$lookup[i]==df$lookup[j]){
      df$va_democracy[j]<-va_edited$va_democracy[i]
    }
  }
}

write.csv(df,"D:\\TIU courses\\Fall_2020\\thesis\\thesis data\\developing\\official_data.csv", row.names = FALSE)

