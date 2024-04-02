library(survival)
library(zoo)

############################################ Estimation


### pi

glm0 <- glm(ia.base ~  base.age +  factor(diag.main) + factor(educ4) + factor(gender), data = df)

pi_df <- data.frame(ia.base = df$ia.base)
pi_df$pi <- df$ia.base * glm0$fitted.values + (1 - df$ia.base) * (1 - glm0$fitted.values)


### CDE


CDE_fun <- function(a, n, dataframe, pi){
  
  df <- subset(dataframe, ia.base == a)
  
  cfit <- coxph(Surv(Tstart, Tstop, to == 2) ~ base.age +  factor(diag.main) +  factor(educ4) + factor(gender), data = df)
  tgrid <- sort(unique(df$Tstop))
  sfit <- summary(survfit(cfit, newdata = df, id = id), times = tgrid)
  
  ## weight dataframe
  c_df <- data.frame(id = rep(NA, times = length(sfit$time)))
  c_df$id[which(sfit$time == sfit$time[1])] <- unique(df$id)
  c_df$id <- na.locf(c_df$id)
  c_df$time <- sfit$time
  c_df$surv <- sfit$surv
  
  ### risk and weight matrix individual x timegrid 
  RiskMat <- WMat <- matrix(NA, nrow = length(unique(c_df$id)), ncol = 365)
  RiskMat[,1] <- WMat[,1] <- 1
  Ids <- unique(c_df$id)
  Nobs <- length(Ids)
  
  for (i in 1:Nobs){
    RiskMat[i, df$Tstop[i]] <- 0
    RiskMat[i,] <- na.locf(RiskMat[i,])
    WMat[i,subset(c_df, id == Ids[i])$time] <- subset(c_df, id == Ids[i])$surv
    WMat[i,] <- na.locf(WMat[i,])
  }
  
  CDE <- apply((1/(pi$pi[pi$ia.base == a] * WMat)) * RiskMat, 2, sum) / n
  
  rtrn <- list(CDE = CDE, WMat = WMat, RiskMat = RiskMat)
  rtrn
}


cdea0 <- CDE_fun(a = 0, n = nrow(df), dataframe = df, pi = pi_df)
cdea1 <- CDE_fun(a = 1, n = nrow(df), dataframe = df, pi = pi_df)






















