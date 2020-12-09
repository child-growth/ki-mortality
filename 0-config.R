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


source(here("0-analysis-functions.R"))


# Define directories
res_dir                           = here::here("results/")
ghapdata_dir                      = "/data/KI/UCB-SuperLearner/Manuscript analysis data/"



project_functions_dir             = here::here("0-project-functions/")
mortality_age_path                = paste0(ghapdata_dir,"mortality_age.rds")
other_mortality_path              = "/data/KI/UCB-SuperLearner/other mortality datasets/"


#Plotting functions

scaleFUN <- function(x) sprintf("%.2f", x)



theme_ki <- function() {
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

theme_set(theme_ki())


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
