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
#sbs3 <- subset(sbs3, id %in% sample(unique(sbs3$id), 2000))

############################################ 



############################################ estimation


################################# pi

glm0 <- glm(ia.base ~  base.age +  factor(diag.main) + factor(educ4) + factor(gender), data = sbs3)

pi_df <- data.frame(ia.base = sbs3$ia.base)
pi_df$pi <- sbs3$ia.base * glm0$fitted.values + (1 - sbs3$ia.base) * (1 - glm0$fitted.values)


################################# SE

######## fit 
sbs2_a0 <- subset(sbs3, ia.base == 0)
sbs2_a1 <- subset(sbs3, ia.base == 1)

cfit2a0 <- coxph(Surv(Tstart, Tstop, to == 2) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = sbs2_a0)
cfit2a1 <- coxph(Surv(Tstart, Tstop, to == 2) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = sbs2_a1)
cfit1a0 <- coxph(Surv(Tstart, Tstop, to == 1) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = sbs2_a0)
cfit1a1 <- coxph(Surv(Tstart, Tstop, to == 1) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = sbs2_a1)

tgrid <- sort(unique(sbs3$Tstop))
sfit2a0 <- summary(survfit(cfit2a0, newdata = sbs2_a0, id = id), times = tgrid)
sfit2a1 <- summary(survfit(cfit2a1, newdata = sbs2_a1, id = id), times = tgrid)
sfit1a0 <- summary(survfit(cfit1a0, newdata = sbs2_a0, id = id), times = tgrid)
sfit1a1 <- summary(survfit(cfit1a1, newdata = sbs2_a1, id = id), times = tgrid)


sfitSE1 <- summary(survfit(cfit1a1, newdata = sbs2_a0, id = id), times = tgrid)
sfitSE2 <- summary(survfit(cfit2a0, newdata = sbs2_a1, id = id), times = tgrid)


######## weight dataframe
weight_fun <- function(fit, df){
  c_df <- data.frame(id = rep(NA, times = length(fit$time)))
  c_df$id[which(fit$time == fit$time[1])] <- unique(df$id)
  c_df$id <- na.locf(c_df$id)
  c_df$time <- fit$time
  c_df$surv <- fit$surv
  
  c_df
}

w1a0 <- weight_fun(sfit1a0, sbs2_a0)
w2a1 <- weight_fun(sfit2a1, sbs2_a1)

w1SE1 <- weight_fun(sfitSE1, sbs2_a0)
w2SE2 <- weight_fun(sfitSE2, sbs2_a1)

################################# SE1

######## risk and weight matrix individual x timegrid 
RiskMata0 <- WMata0 <- WMatLa0 <- WMatUa0 <- matrix(NA, nrow = length(unique(sbs2_a0$id)), ncol = 365)
RiskMata0[,1] <- WMatLa0[,1] <- WMatUa0[,1] <- WMata0[,1] <- 1
Idsa0 <- unique(sbs2_a0$id)
Nobsa0 <- length(Idsa0)

for (i in 1:Nobsa0){
  RiskMata0[i, sbs2_a0$Tstop[i]] <- 0
  RiskMata0[i,] <- na.locf(RiskMata0[i,])
  WMatLa0[i,subset(w1a0, id == Idsa0[i])$time] <- subset(w1a0, id == Idsa0[i])$surv
  WMatLa0[i,] <- na.locf(WMatLa0[i,])
  WMatUa0[i,subset(w1SE1, id == Idsa0[i])$time] <- subset(w1SE1, id == Idsa0[i])$surv
  WMatUa0[i,] <- na.locf(WMatUa0[i,])
  WMata0[i,] <- WMatUa0[i,] / WMatLa0[i,]
}


S1 <- apply(RiskMata0 * WMata0 * (1 / pi_df$pi[pi_df$ia.base == 0]), 2, sum) / nrow(sbs3)
SE1 <- S1 - apply(RiskMata0 * (1 / pi_df$pi[pi_df$ia.base == 0]), 2, sum) / nrow(sbs3)

p1 <- ggplot() + geom_step(aes(1:365, S1))
p2 <- ggplot() + geom_step(aes(1:365, SE1))

gridExtra::grid.arrange(p1,p2)



################################# SE2

######## risk and weight matrix individual x timegrid 
RiskMata1 <- WMata1 <- WMatLa1 <- WMatUa1 <- matrix(NA, nrow = length(unique(sbs2_a1$id)), ncol = 365)
RiskMata1[,1] <- WMatLa1[,1] <- WMatUa1[,1] <- WMata1[,1] <- 1
Idsa1 <- unique(sbs2_a1$id)
Nobsa1 <- length(Idsa1)

for (i in 1:Nobsa1){
  RiskMata1[i, sbs2_a1$Tstop[i]] <- 0
  RiskMata1[i,] <- na.locf(RiskMata1[i,])
  WMatLa1[i,subset(w2a1, id == Idsa1[i])$time] <- subset(w2a1, id == Idsa1[i])$surv
  WMatLa1[i,] <- na.locf(WMatLa1[i,])
  WMatUa1[i,subset(w2SE2, id == Idsa1[i])$time] <- subset(w2SE2, id == Idsa1[i])$surv
  WMatUa1[i,] <- na.locf(WMatUa1[i,])
  WMata1[i,] <- WMatUa1[i,] / WMatLa1[i,]
}


S2 <- apply(RiskMata1 * WMata1 * (1 / pi_df$pi[pi_df$ia.base == 1]), 2, sum) / nrow(sbs3)
SE2 <- (apply(RiskMata1 * (1 / pi_df$pi[pi_df$ia.base == 1]), 2, sum) / nrow(sbs3)) - S2

p3 <- ggplot() + geom_step(aes(1:365, S2))
p4 <- ggplot() + geom_step(aes(1:365, SE2))

gridExtra::grid.arrange(p3,p4)


gridExtra::grid.arrange(p1,p2,p3,p4)

ggplot() + geom_step(aes(S1, S2)) + geom_abline(intercept = 0, slope = 1)


save(SE1, SE2, S1, S2, WMata0, WMata1, RiskMata0, RiskMata1, file = "N:/durable/scrpts/OCBE/Working_files/Separable_effects/Sep_eff_final/For_Git/GitSave2.Rdata")

