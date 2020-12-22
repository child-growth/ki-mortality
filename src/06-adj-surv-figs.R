

rm(list=ls())
source(paste0(here::here(), "/0-config.R"))
library(adjcurve)

cfit4a <- coxph(Surv(futime, death) ~ age + sex + strata(group),
                data=fdata)
surv4a <- survfit(cfit4a)
plot(surv4a, col=c(1,2,4), mark.time=F, xscale=365.25,
       xlab="Years post sample", ylab="Survival")
