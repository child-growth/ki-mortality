



##############################################
#  wrapper function for Cox PH models
##############################################

ki_coxph <- function(d, Xvar, Yvar="dead", age=NULL){
  cat(d$studyid[1])
  if(!is.null(age)){
    d <- d %>% filter(agecat==!!(age))
  }else{
    d$agecat <- "overall"
  }
  d$X <- d[[Xvar]]
  d$Y <- d[[Yvar]]
  
  
  d$agedays <- ifelse(d$dead==1, d$agedth, d$agedays)
  
  d$event <- with(d, Surv(agedays, Y == 1))
  fit <- coxph(event ~ X,  data =  d, cluster=subjid, id=subjid)
  fit <- summary(fit)
  res <- as.data.frame(t(fit$conf.int[1,c(1,3,4)]))
  colnames(res) <- c("HR", "ci.lb", "ci.ub")
  res$est <- as.data.frame(fit$coefficients)$coef[1]
  res$se <- as.data.frame(fit$coefficients)$`se(coef)`[1]
  
  res$N <- nrow(d)
  res$sparseN <- min(table(d$X, d$dead))
  res$agecat <- d$agecat[1]
  
  return(res)
}


#---------------------------------------------------------
# Wrapper function for cox PH meta-analysis
#---------------------------------------------------------

cox_meta <- function(X, Y="dead"){
  
  #Subset to last obs if using CI
  if(grepl("ever_",X)){
    d <- d %>% group_by(studyid, subjid) %>% 
      filter(agedays==max(agedays)) %>%
      ungroup()
  }
  
  d$X <- d[[X]]
  d$Y <- d[[Y]]
  
  res1 <- d %>% 
    filter(!is.na(X) & !is.na(Y)) %>% 
    group_by(studyid) %>%
    mutate(Ndead=sum(Y)) %>% filter(Ndead>4) %>% droplevels(.) %>%
    do(ki_coxph(d=., Xvar=X, Yvar=Y)) %>%
    mutate(pooled=0)
  
  #Drop estimates from sparse data
  res1 <- res1 %>% filter(sparseN > 0)
  
  pooled <- poolHR(res1) %>% mutate(pooled=1)
  
  res <- bind_rows(res1, pooled) %>%
    mutate(X= !!(X), Y= !!(Y))
  
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
      est$ci.lb<-exp(est$est - 1.96 * est$se)
      est$ci.ub<-exp(est$est + 1.96 * est$se)
      
      est$Nstudies <- nstudies$N
      est$studyid  <- "pooled"
      est$agecat <- d$agecat[1]
    }
  
  
  return(est)
}
