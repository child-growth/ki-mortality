

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(janitor)


d <- readRDS(mortality_age_path)

table(d$studyid)
d %>% group_by(studyid) %>% distinct(subjid) %>% summarize(N=n())


#Included studies
tab1 <- d %>% distinct(studyid, manuscript_cohort)


#Get number of observations in different age categories
tab3 <- d %>% group_by(studyid, subjid) %>% 
  
table(d$agecat)

table(d$studyid, d$agecat)
tabyl(dat= d, studyid, agecat)
tab2 <- tabyl(dat= d, studyid, agecat)
tab3 <- tabyl(dat= d, studyid, agecat, dead)$`1`


#subset to primary dataset- has age of death, deaths before 2 years, last measure at least a week prior
tab4 <- d %>% group_by(studyid, subjid) %>% 
  filter(agedays <= 730) %>%
  filter(agedays==last(agedays)) %>%
  group_by(studyid) %>%
  mutate(N_children= length(unique(subjid)),
         mortality_rate= round(mean(dead)*100,1),
         wasting_rate= round(mean(whz < -2, na.rm=T)*100,1),
         stunting_rate= round(mean(haz < -2, na.rm=T)*100,1),
         underweight_rate= round(mean(waz < -2, na.rm=T)*100,1),
         sev_wasting_rate= round(mean(whz < -3, na.rm=T)*100,1),
         sev_stunting_rate= round(mean(haz < -3, na.rm=T)*100,1),
         sev_underweight_rate= round(mean(waz < -3, na.rm=T)*100,1),
         deaths= sum(dead)) %>%
  filter(imp_agedth ==0) %>%
  mutate(N_with_age_of_death=sum(dead),
         mean_age_death = round(mean(agedth, na.rm=T),0)) %>%
  slice(1) %>%
  select(studyid, N_children,mortality_rate, deaths, N_with_age_of_death, mean_age_death, 
         wasting_rate, stunting_rate, underweight_rate, 
         sev_wasting_rate, sev_stunting_rate, sev_underweight_rate) %>%
  arrange(mortality_rate)
tab4



save(tab1, tab2, tab3, tab4, file=here("results/summary_tables.Rdata"))