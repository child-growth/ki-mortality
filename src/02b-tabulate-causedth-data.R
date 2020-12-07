
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
d$causedth_missing = ifelse(d$causedth %in% c("", "Censored"), 1, 0)
studies_and_causes = d %>% 
                      group_by(studyid, causedth) %>% 
                      summarise(count = n(), num_missing = sum(causedth_missing))
View(studies_and_causes)

# Filter studies with missing data
d = d %>% filter(causedth_missing == 0)

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
# (Use your judgment for other large categories)
# Other (for rarer causes)


# A function that takes in a pattern to search `causedth` and returns a 
# vector of 0s and 1s if the `causedth` matches the pattern
get_causedth_binary <- function(pattern) {
  return(as.integer(grepl(pattern, causedth, ignore.case=TRUE)))
}

# Create binary vectors
accidents = get_causedth_binary("accident")
infection_diarrhea = get_causedth_binary("diarrh")
infection_dysentery = get_causedth_binary("dysentery") 
# infection_diarrhea_dysentery = get_causedth_binary("(diarrh)|(dysentery)")
infection_ari_pneumonia = get_causedth_binary("(ari /pneumonia)|(alri)")
infection_tb = get_causedth_binary("tuberculosis")
infection_malaria = get_causedth_binary("malaria")
infection_hiv = get_causedth_binary("hiv") # not a recorded cause of death
infection_meningitis = get_causedth_binary("meningitis")
malnutrition = get_causedth_binary("malaria")
neonatal_sepsis = get_causedth_binary("neonatal sepsis")
sepsis = get_causedth_binary("(sepsis)|(septicemia)") # Non-specific /generalised sepsis
other = get_causedth_binary("(other:)|(sids)|(anemia)|(congenital)")
other_infection = get_causedth_binary("other specific infection")
uncertain = get_causedth_binary("uncertain")

# cols to check:
# added dysentery 
# did not add gastroenteritis -- always shows up with diarrhea
# sepsis - neonatal vs generalised
# congenital ?


# sanity check:
# d$dd = get_causedth_binary("(diarrh)|(dysentery)")
# temp = d %>% filter(dd == 1) %>% group_by(causedth)%>% summarize()
# View(temp)

# Add columns to table
d$causedth_accident = accidents
d$causedth_diarrhea = infection_diarrhea
d$causedth_dysentery = infection_dysentery
# d$causedth_diarrhea_dysentery = infection_diarrhea_dysentery
d$causedth_ari_pneumonia = infection_ari_pneumonia
d$causedth_tb = infection_tb
d$causedth_malaria = infection_malaria
# d$causedth_hiv = infection_hiv
d$causedth_meningitis = infection_meningitis
d$causedth_malnutrition = malnutrition
d$causedth_neonatal_sepsis = neonatal_sepsis
d$causedth_sepsis = sepsis
d$causedth_other = other
d$causedth_other_infection = other_infection
d$causedth_uncertain = uncertain

mortality_causedth_path = paste0(ghapdata_dir,"mortality_causedth.rds")
saveRDS(d, mortality_causedth_path)

