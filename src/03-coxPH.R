

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(survival)


d <- readRDS(mortality_age_path)

d$subjid <- as.numeric(d$subjid)
d <- d %>% arrange(studyid, subjid, agedays)
table(d$studyid)


#Steps for each analysis:
#1) drop imputed agedth if needed
#2) drop observations other than the age category of interest
#3) Create survival outcome
summary(d$agedays)
table(d$agecat)
d <- d %>% filter(agedays <= 730)
#d <- d %>% filter(studyid=="ZVITAMBO")
d <- droplevels(d)



table(d$ever_wast, d$dead, d$studyid)

table(d$agecat)

age="(181,365]"
age=NULL
Xvar="ever_stunt"


#Now expand this coding across outcomes/age categories

res1 <- d %>% 
  filter(!is.na(ever_wast)& !is.na(dead)) %>% 
  group_by(studyid) %>%
  mutate(Ndead=sum(dead)) %>% filter(Ndead>4) %>% droplevels(.) %>%
  do(ki_coxph(d=., Xvar="ever_wast"))

poolHR(res1)
  





## Solution 2: Time-varying effect
## Event status variable necessary
d$event <- (d$dead == 1)


## Counting process format creation
d.split <- survSplit(data    = d,
                        cut     = c(0, 30, 90, 181, 365, 731, 7000), # vector of timepoints to cut at
                        end     = "agedays",  # character string with name of event time variable
                        event   = "event", # character string with name of censoring indicator
                        start   = "start_age", # character string with name of start time variable (created)
                        id      = "id",    # character string with name of new id variable to create
                        zero    = 0       # If start doesn't already exist, used as start
)

d.split <- d.split %>% arrange(studyid, subjid, id, agedays)


## Recreate SurbObj
d.split$SurvObj <- with(d.split, Surv(time = (start_age), time2 = agedays, event = event))

## Check

## Time-varying effect of baseline variable by including interaction with interval
res.cox1.strata <- coxph(SurvObj ~ sex + wast + wast:factor(start_age) + survival::cluster(id),
                         data =  d.split)
summary(res.cox1.strata)


#subset to primary dataset- has age of death, deaths before 2 years, last measure at least a week prior
glimpse(d)
d <- d %>% filter(imp_agedth==0, sufficient_lag==1) %>%
  group_by(studyid) %>% filter(sum(dead, na.rm=T)>10)

table(d$studyid, d$dead)

#Temp: subset to one study
d <- d %>% filter(studyid=="JiVitA-3")

## Add survival object. status == 2 is death
d$SurvObj <- with(d, Surv(agedth, dead == 1))

## Check data
head(d)

table(d$studyid, d$dead)

#Note: I think I need to subset to the final obs
#Also fix code for the first 3 studies to not drop any ons with missing age of death... do that here
#make sure to only drop obs that are dead==1 but also missing agedth... impute agedth with maxage

## Fit Cox regression
res.cox1 <- coxph(SurvObj ~  sex, data =  d)
res.cox1

table(d$wast, d$dead)
table(d$stunt, d$dead)
res.cox2 <- coxph(SurvObj ~  stunt, data =  d)
res.cox2

res.cox3 <- coxph(SurvObj ~  swast, data =  d)
res.cox3