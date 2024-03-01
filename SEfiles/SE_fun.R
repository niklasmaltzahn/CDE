library(survival)
library(zoo)

############################################ data discription
## df is the dataframe containing the registry data
#
#to =  1: is the state of work
#to =  2: is the state of graded sickness absence



############################################ estimation


################################# pi 

glm0 <- glm(ia.base ~  base.age +  factor(diag.main) + factor(educ4) + factor(gender), data = df)

pi_df <- data.frame(ia.base = df$ia.base)
pi_df$pi <- df$ia.base * glm0$fitted.values + (1 - df$ia.base) * (1 - glm0$fitted.values)


################################# SE

######## fit 
df_a0 <- subset(df, ia.base == 0)
df_a1 <- subset(df, ia.base == 1)

cfit2a0 <- coxph(Surv(Tstart, Tstop, to == 2) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = df_a0)
cfit2a1 <- coxph(Surv(Tstart, Tstop, to == 2) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = df_a1)
cfit1a0 <- coxph(Surv(Tstart, Tstop, to == 1) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = df_a0)
cfit1a1 <- coxph(Surv(Tstart, Tstop, to == 1) ~ base.age + factor(diag.main) + factor(educ4) + factor(gender), data = df_a1)

tgrid <- sort(unique(df$Tstop))
# a0 approx 12 h, a1 approx 2h
sfit2a0 <- summary(survfit(cfit2a0, newdata = df_a0, id = id), times = tgrid)
sfit2a1 <- summary(survfit(cfit2a1, newdata = df_a1, id = id), times = tgrid)
sfit1a0 <- summary(survfit(cfit1a0, newdata = df_a0, id = id), times = tgrid)
sfit1a1 <- summary(survfit(cfit1a1, newdata = df_a1, id = id), times = tgrid)
sfitSE1 <- summary(survfit(cfit1a1, newdata = df_a0, id = id), times = tgrid)

sfitSE2 <- list()
ntst <- nrow(df_a1)
for(i in 1:ntst){
  tryCatch({
    print(i)
    sfitSE2[[i]] <- summary(survfit(cfit2a0, newdata = df_a1[i,]), times = tgrid)
  }, error=function(e){})
}




######## weight dataframe
weight_fun <- function(fit, df){
  c_df <- data.frame(id = rep(NA, times = length(fit$time)))
  c_df$id[which(fit$time == fit$time[1])] <- unique(df$id)
  c_df$id <- na.locf(c_df$id)
  c_df$time <- fit$time
  c_df$surv <- fit$surv
  
  c_df
}

w1a0 <- weight_fun(sfit1a0, df_a0)
w2a1 <- weight_fun(sfit2a1, df_a1)

w1SE1 <- weight_fun(sfitSE1, df_a0)
w2SE2 <- weight_fun(sfitSE2, df_a1)


### alternative for w2SE2
tst_time <- lapply(sfitSE2, function(x) x$time)
tst_time <- unlist(tst_time)

df <- data.frame(time = tst_time, id = rep(unique(df_a1$id), each = 365))
df$surv <- unlist(lapply(sfitSE2, function(x) x$surv))
w2SE2 <- df[,c("id", "time", "surv")]


################################# SE1

######## risk and weight matrix individual x timegrid 
RiskMata0 <- WMata0 <- WMatLa0 <- WMatUa0 <- matrix(NA, nrow = length(unique(df_a0$id)), ncol = 365)
RiskMata0[,1] <- WMatLa0[,1] <- WMatUa0[,1] <- WMata0[,1] <- 1
Idsa0 <- unique(df_a0$id)
Nobsa0 <- length(Idsa0)

for (i in 1:Nobsa0){
  print(i)
  RiskMata0[i, df_a0$Tstop[i]] <- 0
  RiskMata0[i,] <- na.locf(RiskMata0[i,])
  WMatLa0[i,subset(w1a0, id == Idsa0[i])$time] <- subset(w1a0, id == Idsa0[i])$surv
  WMatLa0[i,] <- na.locf(WMatLa0[i,])
  WMatUa0[i,subset(w1SE1, id == Idsa0[i])$time] <- subset(w1SE1, id == Idsa0[i])$surv
  WMatUa0[i,] <- na.locf(WMatUa0[i,])
  WMata0[i,] <- WMatUa0[i,] / WMatLa0[i,]
}

S1Mat <- RiskMata0 * WMata0 * (1 / pi_df$pi[pi_df$ia.base == 0])
S1 <- apply(S1Mat, 2, sum) / nrow(df)
Sa0Mat <- RiskMata0 * (1 / pi_df$pi[pi_df$ia.base == 0])
Sa0 <- apply(Sa0Mat, 2, sum) / nrow(df)
SE1 <- S1 - Sa0 


################################# SE2

######## risk and weight matrix individual x timegrid 
RiskMata1 <- WMata1 <- WMatLa1 <- WMatUa1 <- matrix(NA, nrow = length(unique(df_a1$id)), ncol = 365)
RiskMata1[,1] <- WMatLa1[,1] <- WMatUa1[,1] <- WMata1[,1] <- 1
Idsa1 <- unique(df_a1$id)
Nobsa1 <- length(Idsa1)

for (i in 1:Nobsa1){
  print(i)
  RiskMata1[i, df_a1$Tstop[i]] <- 0
  RiskMata1[i,] <- na.locf(RiskMata1[i,])
  WMatLa1[i,subset(w2a1, id == Idsa1[i])$time] <- subset(w2a1, id == Idsa1[i])$surv
  WMatLa1[i,] <- na.locf(WMatLa1[i,])
  WMatUa1[i,subset(w2SE2, id == Idsa1[i])$time] <- subset(w2SE2, id == Idsa1[i])$surv
  WMatUa1[i,] <- na.locf(WMatUa1[i,])
  WMata1[i,] <- WMatUa1[i,] / WMatLa1[i,]
}

S2Mat <- RiskMata1 * WMata1 * (1 / pi_df$pi[pi_df$ia.base == 1])
S2 <- apply(S2Mat, 2, sum) / nrow(df)
Sa1Mat <- RiskMata1 * (1 / pi_df$pi[pi_df$ia.base == 1])
Sa1 <- apply(Sa1Mat, 2, sum) / nrow(df)
SE2 <- Sa1 - S2

########### Total effect:
TE <- Sa1 - Sa0



#save(S1Mat, S2Mat, Sa0Mat, Sa1Mat,
#Sa1, Sa0, TE, SE1, SE2, S1, S2, 
#WMata0, WMata1, RiskMata0, RiskMata1, 
#file = "N:/durable/scrpts/OCBE/Working_files/Separable_effects/Sep_eff_final/For_Git/GitSave2.Rdata")

