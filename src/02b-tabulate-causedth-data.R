
# Setup
rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(janitor)

# Read in data
d <- readRDS(mortality_age_path)

#######################################################################
### Check which studies available, and number of missing in each study 
#######################################################################

# Check that there are no null values in `causedth`
assertthat::are_equal(sum(is.na(d$causedth)), 0) 

# Add new binary column if causedth is missing
d$causedth_missing = ifelse(d$causedth %in% c(""), 1, 0)
studies_and_causes = d %>% 
                      group_by(studyid, causedth) %>% 
                      summarise(count = n(), num_missing = sum(causedth_missing))
studies_and_causes

# Tabulate causes of death
causedth = d$causedth
unique_causedth = unique(causedth)
table(causedth)

# Categories
# Accidents
# Infection
  # Diarrhea
  # Acute respiratory infection (ARI)/Pneumonia
  # Tuberculosis
  # Malaria
  # HIV
  # Meningitis
# Neonatal sepsis
# Malnutrition
# Congenital
# Other (for rarer causes)


# A function that takes in a pattern to search `causedth` and returns a 
# vector of 0s and 1s if the `causedth` matches the pattern
get_causedth_binary <- function(pattern) {
  return(as.integer(grepl(pattern, causedth, ignore.case=TRUE, perl=TRUE)))
}

# Create binary vectors
accidents = get_causedth_binary("accident")
infection_diarrhea_dysentery = get_causedth_binary("(diarrh)|(dysentery)")
infection_ari_pneumonia = get_causedth_binary("(ari /pneumonia)|(alri)")
infection_tb = get_causedth_binary("tuberculosis")
infection_malaria = get_causedth_binary("malaria")
infection_hiv = get_causedth_binary("hiv") # not a recorded cause of death
infection_meningitis = get_causedth_binary("meningitis")
malnutrition = get_causedth_binary("malnutrition")
sepsis = get_causedth_binary("(sepsis)|(septicemia)")
congenital = get_causedth_binary("cong")
other = get_causedth_binary("(((other:)|(sids)|(anemia)))")
other = ifelse(!(congenital & other), other, 0) # exclude congenital from other category
other_infection = get_causedth_binary("other specific infection") # separate the other categories?
uncertain = get_causedth_binary("uncertain|(censored)") # check if censored belongs here


# sanity check:
# d$dd = get_causedth_binary("(diarrh)|(dysentery)")
# temp = d %>% filter(dd == 1) %>% group_by(causedth)%>% summarize()
# View(temp)


# Add columns to table
d$causedth_accident = accidents
d$causedth_diarrhea_dysentery = infection_diarrhea_dysentery
d$causedth_ari_pneumonia = infection_ari_pneumonia
d$causedth_tb = infection_tb
d$causedth_malaria = infection_malaria
# d$causedth_hiv = infection_hiv # do we want to include this? 
d$causedth_meningitis = infection_meningitis
d$causedth_malnutrition = malnutrition
d$causedth_sepsis = sepsis
d$causedth_other = other
d$causedth_other_infection = other_infection
d$causedth_uncertain = uncertain

# do we want to save the data?
# mortality_causedth_path = paste0(ghapdata_dir,"mortality_causedth.rds") # add to config
# saveRDS(d, mortality_causedth_path)

