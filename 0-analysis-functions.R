



##############################################
#  wrapper function for Cox PH models
##############################################

ki_coxph <- function(d, Xvar, age=NULL){
  cat(d$studyid[1])
  if(!is.null(age)){
    d <- d %>% filter(agecat==!!(age))
  }
  d$X <- d[[Xvar]]
  
  d$event <- with(d, Surv(agedays, dead == 1))
  fit <- coxph(event ~ X + survival::cluster(subjid), data =  d)
  fit <- summary(fit)
  res <- as.data.frame(t(fit$conf.int[1,c(1,3,4)]))
  colnames(res) <- c("HR", "ci.lb", "ci.ub")
  res$est <- as.data.frame(fit$coefficients)$coef[1]
  res$se <- as.data.frame(fit$coefficients)$`se(coef)`[1]
  
  res$N <- nrow(d)
  res$sparseN <- min(table(d$X, d$dead))
  
  if(!is.null(age)){
    res$agecat <- d$agecat[1]
  }else{
    res$agecat <- "overall"
  }
  return(res)
}






#-----------------------------------------------------------------------------------------
# Meta-analysis function
#-----------------------------------------------------------------------------------------

poolHR <- function(d, method="REML"){
  d <- d %>% ungroup()
  
  #drop out studies with sparse strata
  d <- d %>% filter(sparseN >0)
  
  nstudies <- d %>% summarise(N=n())
  
    
    fit<-NULL
    try(fit<-rma(yi=est, sei=se, data=d, method=method, measure="RR"))
    if(method=="REML"){
      if(is.null(fit)){try(fit<-rma(yi=est, sei=se, data=d, method="ML", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=est, sei=se, data=d, method="DL", measure="RR"))}
      if(is.null(fit)){try(fit<-rma(yi=est, sei=se, data=d, method="HE", measure="RR"))}
    }
    if(is.null(fit)){
      est<-data.frame(logHR.psi=NA, logSE=NA, HR=NA, HR.CI1=NA,  HR.CI2=NA)
    }else{
      
      est<-data.frame(fit$b, fit$se)
      colnames(est)<-c("est","se")
      
      est$HR<-exp(est$est)
      est$HR.CI1<-exp(est$est - 1.96 * est$se)
      est$HR.CI2<-exp(est$est + 1.96 * est$se)
      
      est$Nstudies <- nstudies$N
    }
  
  
  return(est)
}
