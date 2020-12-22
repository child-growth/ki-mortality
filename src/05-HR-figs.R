

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))

d <- readRDS(here("results/full_cox_results.RDS"))


head(d)

d %>% distinct(studyid)
d %>% distinct(X, Y)

d <- d %>%
  mutate(
    Xname = case_when(X=="stunt"  ~ "Stunted", 
                X=="wast"  ~ "Wasted",
                X=="underwt"  ~ "Underweight",          
                X=="sstunt"  ~ "Severely stunted",          
                X=="swast"  ~ "Severely wasted",           
                X=="sunderwt"  ~ "Severely underweight",        
                X=="stunt_uwt"  ~ "Stunted + underweight",       
                X=="wast_uwt"  ~ "Wasted + underweight",        
                X=="co"  ~ "Wasted + stunted",              
                X=="ever_stunt"  ~ "Ever stunted",      
                X=="ever_wast"  ~   "Ever wasted",        
                X=="ever_uwt"  ~   "Ever underweight",         
                X=="ever_sstunt"  ~   "Ever severely stunted",     
                X=="ever_swast"  ~   "Ever severely wasted",       
                X=="ever_suwt"  ~   "Ever severely underweight",        
                X=="ever_stunt_uwt"  ~   "Ever stunted + underweight",   
                X=="ever_wast_uwt"  ~   "Ever wasted + underweight",    
                X=="ever_co"  ~ "Ever wasted + stunted"),
  Xname = factor(Xname, levels = rev(c(
      "Stunted", 
      "Wasted",
      "Underweight",          
      "Severely stunted",          
      "Severely wasted",           
      "Severely underweight",        
      "Stunted + underweight",       
      "Wasted + underweight",        
      "Wasted + stunted",              
      "Ever stunted",      
      "Ever wasted",        
      "Ever underweight",         
      "Ever severely stunted",     
      "Ever severely wasted",       
      "Ever severely underweight",        
      "Ever stunted + underweight",   
      "Ever wasted + underweight",    
      "Ever wasted + stunted" ))))


# #Clean results for primary figures
# d <- d %>% 
#   mutate(
#     Y=case_when(
#       Y=="dead" ~ "Mortality <24mo"
#     ),
#     Y=factor(Y, levels=c("Diarrhea", "Stunting", "Wasting", "ARI", "HAZ", "WHZ","Mortality")),
#     country=case_when(
#       country=="pooled" ~ "Pooled",
#       country=="PakistanPunjab" ~ "Pakistan",
#       country==country ~ country
#     ),
#     country=factor(country, levels=rev(c("Bangladesh", "Pakistan", "Zimbabwe", "Pooled"))),
#     X = case_when(X=="EC_H" ~ "Uncontaminated\nHH water", 
#                   X=="EC_S" ~ "Uncontaminated\nsource water", 
#                   X=="san_imp" ~ "Improved\nsanitation", 
#                   X=="wat_imp" ~ "Improved\nwater supply", 
#                   X=="hyg_imp" ~ "Improved\nhygiene", 
#                   X=="WASH" ~ "Improved WASH,\nno contamination",
#                   X=="WASH_noEC" ~ "Improved\nWASH",
#                   X=="safely_manH20" ~ "Safely managed\ndrinking water",
#                   X=="EC_risk_H" ~ "HH water\ncontamination", 
#                   X=="EC_risk_S" ~ "Source water\ncontamination", 
#                   X=="san_imp_cat" ~ "Sanitation\ncategory", 
#                   X=="wat_imp_cat" ~ "Water supply\ncategory", 
#                   X=="hyg_imp_cat" ~ "Hygiene\ncategory"),
#     X=factor(X, levels = c(
#       "Improved\nwater supply", 
#       "Improved\nsanitation", 
#       "Improved\nhygiene", 
#       "Improved\nWASH",
#       "Uncontaminated\nHH water", 
#       "Uncontaminated\nsource water", 
#       "Safely managed\ndrinking water",
#       "Improved WASH,\nno contamination",
#       "HH water\ncontamination", 
#       "Source water\ncontamination", 
#       "Sanitation\ncategory", 
#       "Water supply\ncategory", 
#       "Hygiene\ncategory")),
#     contrast = case_when(
#       contrast=="1" ~ "Unimproved",
#       contrast=="2" ~ "Moderate risk",
#       contrast=="3" ~ "High risk", 
#       contrast=="4" ~ "Very high risk", 
#       contrast==contrast ~ contrast 
#     ),
#     contrast=factor(contrast, levels=rev(c("Moderate risk", "High risk",  "Very high risk",   "Basic", "Limited",  "No facility", "None",  "Unimproved", "Surface water"))),
#     ref = case_when(
#       ref=="0" ~ "Improved",
#       ref=="1" ~ "Low risk",
#       ref==ref ~ ref 
#     ))
# 
# table(d$X)
# table(d$Y)



p_prim_pooled <- d %>% filter(pooled==1, !grepl("ever_",X)) %>% 
  droplevels(.) %>%
  #mutate(X=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=HR, x=Xname),color="black") +
  #facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("") + ylab("Hazard Ratio")

p_prim_pooled



p_ci_pooled <- d %>% filter(pooled==1, grepl("ever_",X)) %>% 
  droplevels(.) %>%
  #mutate(X=factor(X, levels = rev(levels(X)))) %>%
  ggplot(., aes(y=HR, x=Xname),color="black") +
  #facet_grid(~Y) +
  geom_point() + 
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
  #scale_color_manual(values=tableau10) +
  geom_hline(yintercept = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
  coord_flip() +
  xlab("") + ylab("Hazard Ratio")

p_ci_pooled



# p_temp_pooled <- d %>% filter(pooled==1, grepl("cum_",X)) %>% 
#   droplevels(.) %>%
#   #mutate(X=factor(X, levels = rev(levels(X)))) %>%
#   ggplot(., aes(y=HR, x=X),color="black") +
#   #facet_grid(~Y) +
#   geom_point() + 
#   geom_linerange(aes(ymin=ci.lb, ymax=ci.ub )) +
#   #scale_color_manual(values=tableau10) +
#   geom_hline(yintercept = 1) +
#   scale_y_continuous(breaks=c(0.25, 0.5,1, 2, 4, 8), trans='log10', labels=scaleFUN) +
#   coord_flip() +
#   xlab("") + ylab("Hazard Ratio")
# 
# p_temp_pooled

