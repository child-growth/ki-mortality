

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))


d <- readRDS(mortality_age_path)

d$subjid <- as.numeric(d$subjid)
d <- d %>% arrange(studyid, subjid, agedays)
table(d$studyid)

#TEMP drop new studies
d <- d %>% filter(!(studyid %in% c("VITALPAK-Pregnancy","iLiNS-DYAD-G","DIVIDS")))


#Steps for each analysis:
#1) drop imputed agedth if needed
#2) drop observations other than the age category of interest
#3) Create survival outcome
summary(d$agedays)
table(d$agecat)
d <- d %>% filter(agedays <= 730)
#d <- d %>% filter(studyid=="ZVITAMBO")
d <- droplevels(d)

#Drop imputed age of death and studies with only imputed age of death
df <- d %>% filter(dead==1)
table(df$studyid, df$imp_agedth)

d <- d %>% filter(imp_agedth==0,
                  !(studyid %in% c("GMS-Nepal","SAS-CompFeed","SAS-FoodSuppl")))



#Covariates adjustment set
#Wvars <- c("agedays", "", "")


#Now expand this coding across outcomes/age categories

# res1 <- d %>% 
#   filter(!is.na(ever_wast)& !is.na(dead)) %>% 
#   group_by(studyid) %>%
#   mutate(Ndead=sum(dead)) %>% filter(Ndead>4) %>% droplevels(.) %>%
#   do(ki_coxph(d=., Xvar="ever_wast")) %>%
#   mutate(pooled=0)
# 
# poolHR <- poolHR(res1) %>% mutate(pooled=1)
# 
# res <- bind_rows(res1, poolHR) %>%
#   mutate(X= "ever_wast", Y= "dead")


#d <- d %>% filter(studyid=="VITAMIN-A")
res <- cox_meta(X="ever_stunt")



X_vector <- c("stunt", "wast","underwt",          
              "sstunt",          "swast",            "sunderwt",         "stunt_uwt",       
              "wast_uwt",         "co",              
              # "cum_stunt",        "cum_wast",         "cum_uwt",
              # "cum_sstunt",       "cum_swast",        "cum_suwt",
              # "cum_stunt_uwt",    "cum_wast_uwt",    "cum_co",
              "ever_stunt",       "ever_wast",        "ever_uwt",         "ever_sstunt",     
              "ever_swast",       "ever_suwt",        "ever_stunt_uwt",   "ever_wast_uwt",    "ever_co")

full_res <- NULL
for(i in X_vector){
  res <- cox_meta(X=i)
  full_res <- bind_rows(full_res, res)
}



dim(full_res)
table(full_res$X)           
saveRDS(full_res, file=here("results/full_cox_results.RDS"))


#TO do:

#XXX need to fix ever_... analyses. Seems to work within function, but not within do()... try by cohort

#XXXX Need to fix - using agedays in the cox regression, but often agedeath comes later
   #Need to double check I'm setting up the analysis correctly

#0)
# Make sure "ever" variables cause function to subset children to the last measure

#1) Allow for subgroup variables, including sex

#2)
# We will analyze anthropometry status using the last anthropometry measurement before death, excluding measurements 
# occurring more than 6 months prior to death, or less than 1 week to avoid bias from potential reverse causation
 #Need to recreate "ever" variables from "cumsum" variables after 

   #do this by checking the diff between agedays and death in the last obs for children who died
   #drop the child from the analysis if not. Coded like this:
   d <- d %>% group_by(studyid, subjid) %>% 
       mutate(diff_death = ifelse(dead==1, agedth - agedays, NA),
              drop_due_to_time_gap = max(ifelse(diff_death > 30.4167*6 | diff_death < 7, 1, 0))) %>%
       filter(drop_due_to_time_gap!=1)
   #Note... this may be dropping too many children... investigate further


#3)
# We also will repeat the above analyses using different age ranges, starting at birth and ending at the following ages: 
#   birth, one month, 3 months, 6 months, 12 months, and 24 months.

#4) run with and without imputed age of death

#5) add covariate adjustment

#6) double check the coding of cumulative incidence analysis
   #-also, should this impose a certain number of measurements? 
   #I.e ever stunting might be biased comparing kids with >8 measurements to neonatal deaths with 1 measurements

#7) MUAC analysis

# ## Solution 2: Time-varying effect
# ## Event status variable necessary
# d$event <- (d$dead == 1)
# 
# 
# ## Counting process format creation
# d.split <- survSplit(data    = d,
#                         cut     = c(0, 30, 90, 181, 365, 731, 7000), # vector of timepoints to cut at
#                         end     = "agedays",  # character string with name of event time variable
#                         event   = "event", # character string with name of censoring indicator
#                         start   = "start_age", # character string with name of start time variable (created)
#                         id      = "id",    # character string with name of new id variable to create
#                         zero    = 0       # If start doesn't already exist, used as start
# )
# 
# d.split <- d.split %>% arrange(studyid, subjid, id, agedays)
# 
# 
# ## Recreate SurbObj
# d.split$SurvObj <- with(d.split, Surv(time = (start_age), time2 = agedays, event = event))
# 
# ## Check
# 
# ## Time-varying effect of baseline variable by including interaction with interval
# res.cox1.strata <- coxph(SurvObj ~ sex + wast + wast:factor(start_age) + survival::cluster(id),
#                          data =  d.split)
# summary(res.cox1.strata)
# 
# 
# #subset to primary dataset- has age of death, deaths before 2 years, last measure at least a week prior
# glimpse(d)
# d <- d %>% filter(imp_agedth==0, sufficient_lag==1) %>%
#   group_by(studyid) %>% filter(sum(dead, na.rm=T)>10)
# 
# table(d$studyid, d$dead)
# 
# #Temp: subset to one study
# d <- d %>% filter(studyid=="JiVitA-3")
# 
# ## Add survival object. status == 2 is death
# d$SurvObj <- with(d, Surv(agedth, dead == 1))
# 
# ## Check data
# head(d)
# 
# table(d$studyid, d$dead)
# 
# #Note: I think I need to subset to the final obs
# #Also fix code for the first 3 studies to not drop any ons with missing age of death... do that here
# #make sure to only drop obs that are dead==1 but also missing agedth... impute agedth with maxage
# 
# ## Fit Cox regression
# res.cox1 <- coxph(SurvObj ~  sex, data =  d)
# res.cox1
# 
# table(d$wast, d$dead)
# table(d$stunt, d$dead)
# res.cox2 <- coxph(SurvObj ~  stunt, data =  d)
# res.cox2
# 
# res.cox3 <- coxph(SurvObj ~  swast, data =  d)
# res.cox3