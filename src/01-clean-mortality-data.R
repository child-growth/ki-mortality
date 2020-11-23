



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))



#read csv file
d <- readRDS(paste0(ghapdata_dir, "ki-manuscript-dataset.rds"))
gc()

d <- subset(d, select= c(studyid, country, subjid, agedays, dead, agedth, causedth, haz, whz, waz))
gc()


dim(d)
d <- d %>% filter(!is.na(dead) | !is.na(agedth) | !is.na(causedth))
dim(d)

#Get maximum age that anthropometry was recorded
d <- d %>% group_by(studyid, country, subjid) %>% mutate(maxage = max(agedays))
summary(d$maxage)

table(d$dead)
table(d$causedth)

table(1*!is.na(d$dead),is.na(d$agedth))
table(1*!is.na(d$dead),(d$causedth==""))

#mark dead if have a cause or age death
d$agedth[!is.na(d$agedth) & is.na(d$dead)]
d$dead[!is.na(d$agedth) & is.na(d$dead)] <- 1


table(d$studyid, d$agedth>0)
table(d$studyid, d$dead)
table(d$dead)

head(d)

table(d$dead, is.na(d$agedth))


#drop missing agedth
d <- d %>% filter(!is.na(agedth))

#Create an indicator for when age of death is imputed.

saveRDS(d, mortality_age_path)



# 
# #make sure dataset is time-static and subset to 1 observation
# # add minimum Z-scores, mean, and number of stunted or wasted obs
# d <- d %>% 
#   group_by(studyid, country, subjid) %>%
#   mutate(dead=1*(max(dead)>0 | (agedth>0) | (!is.na(causedth) & causedth!="")), agedth=max(agedth)) %>% 
#   slice(1)
# dim(d)
# table(d$dead)
# table(d$studyid, d$dead)
# 
# #look at measurements prior to early death
# #-many seem like incorrect age-death, with anthro measures at older ages
# as.data.frame(d[d$agedth<8 & !is.na(d$agedth),])
# 
# #Drop mortality after 24 months
# summary(d$maxage)
# dim(d)
# table(d$dead)
# 
# #Mark deaths not over 24 months to keep
# d <- d %>% mutate(keep=1*(agedth <= 730 | is.na(agedth) & ((maxage <= 730 & dead==1) | dead!=1)))
# dim(d)
# table(d$dead)
# table(d$keep)
# 
# 
# 
# #Create a mortality after 6 month variable
# d$dead624 <- d$dead 
# d$dead624[d$agedth < 6*30.4167 | (is.na(d$agedth) & d$maxage<6*30.4167)] <- 2
# table(d$dead)
# table(d$dead624)
# 
# d$subjid <- as.character(d$subjid)
# 
# 
# mort <- d
# mort$dead[is.na(mort$dead)] <-0
# mort$dead624[is.na(mort$dead624)] <-0
# mort$dead624[mort$dead624==2] <- NA
# #keep variable with mortality after 24 months
# mort$dead0plus <- mort$dead
# mort$dead6plus <- mort$dead624
# #mark death as 0 if child died after 24 months
# mort$dead[mort$keep==0] <- 0
# mort$dead624[mort$keep==0] <- 0
# 
# prop.table(table(mort$dead))
# 
# mort$cohort <- paste0(mort$studyid," ", mort$country)
# 
# #drop cohorts with no mortality info
# mort <- mort %>% group_by(cohort) %>% mutate(tot_dead=sum(dead, na.rm=T)) %>% filter(tot_dead>0)
# prop.table(table(mort$dead))
# table(mort$cohort, mort$dead)
# table(mort$cohort, mort$dead624)
# table(mort$cohort, mort$dead0plus)
# table(mort$cohort, mort$dead6plus)
# 
# 
# mort <- subset(mort, select = c(studyid, country, subjid, maxage, dead, agedth, causedth, dead624, dead0plus, dead6plus))
# 
# saveRDS(mort, mortality_age_path)
# 
# 
# #mortality within studies measuring anthro before 6 months and had sufficient mortality to be in the primary analysis
# table(mort$dead[mort$studyid %in% c("iLiNS-DOSE", "iLiNS-DYAD-M","JiVitA-3","JiVitA-4","Keneba", "SAS-CompFeed","VITAMIN-A","ZVITAMBO")])
# prop.table(table(mort$dead[mort$studyid %in% c("iLiNS-DOSE", "iLiNS-DYAD-M","JiVitA-3","JiVitA-4","Keneba", "SAS-CompFeed","VITAMIN-A","ZVITAMBO")]))
# 





# Incorporate DIVIDS, VITALPAK-Pregnancy, and I-LINS Dyad Ghana into the analysis

divids<-fread(paste0(ghapdata_dir,"FINAL.csv"), header = T,
         drop = c( "AGEIMPFL",
                   "BAZ", "HCAZ",      
                   "REGCTRY", "REGCTYP",
                   "HHID",    
                   "FEEDING", "DURBRST", 
                   "ENSTUNT", "FWTKG", "FBMI",
                   "BRFEED", "SUMEP",   "SUMDIAR", "SUMDAYS",
                   "PCTDIAR", "IMPSAN",  "SOAP",    "SAFEH2O", "H2OTIME",
                   "CHICKEN", "COW",     "CATTLE",  "INCTOT", 
                   "INCTOTU", "BFEDFL",  "EXBFEDFL","WEANFL",  "ANMLKFL", "PWMLKFL",
                   "FORMLKFL","BOTTLEFL","H20FEDFL","OTHFEDFL","SLDFEDFL","NBFYES",   "CMFDINT", "DIARFL",  "LSSTLFL",
                   "NUMLS",   "BLDSTLFL","DIARFL_R","LSSTFL_R","NUMLS_R", "BLDSTL_R",
                   "DUR_R"))
gc()

colnames(divids) <- tolower(colnames(divids))
gc()


# subset to only DIVIDS study
dim(divids)
divids <- divids[(studyid %in% c("DIVIDS"))]
dim(divids)
gc()

# drop all rows where both `haz`` and `waz` are missing 
# ASK ANDREW 
divids <- divids %>% filter(!(is.na(haz) & is.na(waz)))
gc()


###############################

othermortality_dir = "/data/KI/UCB-SuperLearner/other mortality datasets/"

vitalpak_preg <- read.csv(paste0(othermortality_dir, "VITALPAK_Pregnancy.csv"))
colnames(vitalpak_preg) <- tolower(colnames(vitalpak_preg))
gc()

ilinsdyadghana <- read.csv(paste0(othermortality_dir, "full_ki1033518_DYAD_G_201809.csv"))
colnames(ilinsdyadghana) <- tolower(colnames(ilinsdyadghana))
gc()

View(ilinsdyadghana)















