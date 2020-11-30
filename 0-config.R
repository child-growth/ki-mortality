#-------------------------------------
# ki longitudinal analysis manuscripts

# configure data directories
# source base functions
# load libraries
#-------------------------------------
kiPath <- c("/data/KI/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths())
.libPaths(kiPath)

library(tidyverse)
library(here)
#library(dplyr)
library(metafor)
library(data.table)
library(viridis)
library(ggthemes)
library(survival)
library(survminer)



# Define directories
res_dir                           = here::here("results/")
ghapdata_dir                      = "/data/KI/UCB-SuperLearner/Manuscript analysis data/"



project_functions_dir             = here::here("0-project-functions/")
mortality_age_path                = paste0(ghapdata_dir,"mortality_age.rds")
other_mortality_path              = "/data/KI/UCB-SuperLearner/other mortality datasets/"


##################################

# # Source base functions  
# source(paste0(project_functions_dir, "/0_clean_study_data_functions.R"))
# source(paste0(project_functions_dir, "/0_descriptive_epi_shared_functions.R"))
# source(paste0(project_functions_dir, "/0_descriptive_epi_stunt_functions.R"))
# source(paste0(project_functions_dir, "/0_descriptive_epi_wast_functions.R"))
# source(paste0(project_functions_dir, "/0_helper_sampling_weights.R")) 
# source(paste0(project_functions_dir, "/0_risk_factor_functions.R")) 
# 
# 
# # Set theme
# source(paste0(here::here(), "/5-visualizations/0-plot-themes.R"))
# theme_set(theme_ki())
