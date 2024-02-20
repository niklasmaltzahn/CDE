library(ggplot2)
library(survival)
library(zoo)

############################################ data 


# 1: work
# 2: psick
# 3: sick
# 4: unemp
# 5: dead
# 6: educ

# 7: your administrative censoring state


sbs <- msdata_rs[msdata_rs$status == 1,] 
# only one obs pr person i.e. competing risk situation:
sbs2 <- sbs[!duplicated(sbs$id),]
### setting enrollment period
sbs2 <- subset(sbs2, (time >= "2004-01-01") & (time <= "2006-12-31"))
# administrative censoring as a separate state if Tstop is above 365 days:
sbs2$to[sbs2$Tstop >= 365] <- 7
# Setting end of follow up to 300 if Tstop is above 365 days:
sbs2$Tstop[sbs2$Tstop >= 365] <- 365
sbs2$Tstart[sbs2$Tstart > 0] <- 0

sbs3 <- sbs2

############################################ 



############################################ estimation


### pi

glm0 <- glm(ia.base ~  base.age +  factor(diag.main) + factor(educ4) + factor(gender), data = sbs3)

pi_df <- data.frame(ia.base = sbs3$ia.base)
pi_df$pi <- sbs3$ia.base * glm0$fitted.values + (1 - sbs3$ia.base) * (1 - glm0$fitted.values)


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
  
  CDE
}

cdea0 <- CDE_fun(a = 0, n = nrow(sbs3), dataframe = sbs3, pi = pi_df)
cdea1 <- CDE_fun(a = 1, n = nrow(sbs3), dataframe = sbs3, pi = pi_df)




############## plot 
p1 <- ggplot() + geom_step(aes(1:365, cdea0)) 
p2 <- ggplot() + geom_step(aes(1:365, cdea1 - cdea0)) 
gridExtra::grid.arrange(p1,p2)

save(cdea0, cdea1, file = "N:/durable/scrpts/OCBE/Working_files/Separable_effects/Sep_eff_final/For_Git/GitSave1.Rdata")





