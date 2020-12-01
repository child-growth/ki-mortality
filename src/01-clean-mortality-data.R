



rm(list=ls())
source(paste0(here::here(), "/0-config.R"))


# clean DIVIDS data

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

# subset to only DIVIDS study
dim(divids)
divids <- divids[(studyid %in% c("DIVIDS"))]
dim(divids)
gc()

# subset columns
divids <- subset(divids, select=c(studyid, country, subjid, sex, agedays, dead, agedth, causedth, haz, whz, waz))
gc()

# drop all rows where both `haz`` and `waz` are missing 
divids <- divids %>% filter(!(is.na(haz) & is.na(waz)))
gc()

divids$subjid <- as.character(divids$subjid)


# clean VITALPAK-Pregnancy data
vitalpak_preg <- read.csv(paste0(other_mortality_path, "VITALPAK_Pregnancy.csv"))
colnames(vitalpak_preg) <- tolower(colnames(vitalpak_preg))
gc()

# subset columns
vitalpak_preg <- subset(vitalpak_preg, select=c(studyid, country, subjid, sex, agedays, dead, agedth, causedth, haz, whz, waz))
gc()
vitalpak_preg$subjid <- as.character(vitalpak_preg$subjid)



# clean I-LINS Dyad Ghana data
ilinsdyadghana <- read.csv(paste0(other_mortality_path, "full_ki1033518_DYAD_G_201809.csv"))
colnames(ilinsdyadghana) <- tolower(colnames(ilinsdyadghana))
gc()

# subset columns
ilinsdyadghana <- subset(ilinsdyadghana, select=c(studyid, country, subjid, sex, agedays, dead, agedth, haz, whz, waz))
gc()

ilinsdyadghana$subjid <- as.character(ilinsdyadghana$subjid)
ilinsdyadghana$studyid[ilinsdyadghana$studyid == "ki1033518-iLiNS-DYAD-G"] <- "iLiNS-DYAD-G"

##########################################################

#read csv file
df <- readRDS(paste0(ghapdata_dir, "ki-manuscript-dataset.rds"))
gc()

df <- subset(df, select= c(studyid, country, subjid, sex, agedays, dead, agedth, causedth, haz, whz, waz))
gc()


d <- bind_rows(df, divids, vitalpak_preg, ilinsdyadghana)

dim(d)
d <- d %>% filter(!is.na(dead) | !is.na(agedth) | !is.na(causedth))
dim(d)
table(d$studyid)

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

#drop missing death variable - should I do this for coxPH model?
d <- d %>% filter(!is.na(dead))

#Create an indicator for when age of death is imputed.
d$imp_agedth <- ifelse(is.na(agedth), 1, 0)

#impute missing agedth with max age
d$agedth[is.na(d$agedth)] <- d$maxage[is.na(d$agedth)] 


saveRDS(d, mortality_age_path)





 
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

data <- d[d$studyid=="Burkina Faso Zn"]
  
recorded_i <- subset(data, agedth > 0 & agedth < 29, select = c(agedth))
recorded_ii <- subset(data, agedth > 28 & agedth < 91, select = c(agedth))
recorded_iii <- subset(data, agedth >90 & agedth < 181, select = c(agedth))
recorded_iv <- subset(data, agedth > 180 & agedth < 361, select = c(agedth))
recorded_v <- subset(data, agedth > 360 & agedth < 721, select = c(agedth))
  
imputed_i <- subset(data, maxage > 0 & maxage < 29, select = c(maxage))
imputed_ii <- subset(data, maxage > 28 & maxage < 91, select = c(maxage))
imputed_iii <- subset(data, maxage >90 & maxage < 181, select = c(maxage))
imputed_iv <- subset(data, maxage > 180 & maxage < 361, select = c(maxage))
imputed_v <- subset(data, maxage > 360 & maxage < 721, select = c(maxage))
  
cat("recorded\n")
dim(recorded_i)
dim(recorded_ii)
dim(recorded_iii)
dim(recorded_iv)
dim(recorded_v)
  
cat("\nimputed\n")
dim(imputed_i)
dim(imputed_ii)
dim(imputed_iii)
dim(imputed_iv)
dim(imputed_v)

