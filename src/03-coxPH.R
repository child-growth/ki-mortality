

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(survival)


d <- readRDS(mortality_age_path)

table(d$studyid)

#Temp- subset to one study
d <- d %>% filter(studyid=="MAL-ED")



## Add survival object. status == 2 is death
d$SurvObj <- with(d, Surv(agedth, dead == 1))

## Check data
head(d)

#Note: I think I need to subset to the final obs
#Also fix code for the first 3 studies to not drop any ons with missing age of death... do that here
#make sure to only drop obs that are dead==1 but also missing agedth... impute agedth with maxage

## Fit Cox regression: age, sex, Karnofsky performance score, wt loss
res.cox1 <- coxph(SurvObj ~  sex, data =  d)
res.cox1