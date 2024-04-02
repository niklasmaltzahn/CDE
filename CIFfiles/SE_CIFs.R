######################################### CIFs

####################### CIFs a = 0


GEventMata0 <- WEventMata0 <- matrix(0, nrow = length(unique(df_a0$id)), ncol = 365)
GEventMata0[,1] <- WEventMata0[,1] <-  0
Idsa0 <- unique(df_a0$id)
Nobsa0 <- length(Idsa0)

for (i in 1:Nobsa0){
  print(i)
  GEventMata0[i, df_a0$Tstop[i]] <- 1 * (df_a0$to[i] == 2)
  WEventMata0[i, df_a0$Tstop[i]] <- 1 * (df_a0$to[i] == 1)
}


#CIFs a0
NGa0Mat <- GEventMata0 * (1 / pi_df$pi[pi_df$ia.base == 0])
CIFGa0 <- cumsum(apply(NGa0Mat, 2, sum) / nrow(df))
NWa0Mat <- WEventMata0 * (1 / pi_df$pi[pi_df$ia.base == 0])
CIFWa0 <- cumsum(apply(NWa0Mat, 2, sum) / nrow(df))

#CIFG_aG1aW0
NGaW1aG0 <- GEventMata0 * WMata0 * (1 / pi_df$pi[pi_df$ia.base == 0])
CIFG_aW1aG0 <- cumsum(apply(NGaW1aG0, 2, sum) / nrow(df))


################ a=1

GEventMata1 <- WEventMata1 <- matrix(0, nrow = length(unique(df_a1$id)), ncol = 365)
GEventMata1[,1] <- WEventMata1[,1] <- 0
Idsa1 <- unique(df_a1$id)
Nobsa1 <- length(Idsa1)

for (i in 1:Nobsa1){
  print(i)
  GEventMata1[i, df_a1$Tstop[i]] <- 1 * (df_a1$to[i] == 2)
  WEventMata1[i, df_a1$Tstop[i]] <- 1 * (df_a1$to[i] == 1)
}

#CIFs a1
NGa1Mat <- GEventMata1 * (1 / pi_df$pi[pi_df$ia.base == 1])
CIFGa1 <- cumsum(apply(NGa1Mat, 2, sum) / nrow(df))
NWa1Mat <- WEventMata1 * (1 / pi_df$pi[pi_df$ia.base == 1])
CIFWa1 <- cumsum(apply(NWa1Mat, 2, sum) / nrow(df))

#CIFW_aW1aG0
NWaW1aG0 <- WEventMata1 * WMata1 * (1 / pi_df$pi[pi_df$ia.base == 1])
CIFW_aW1aG0 <- cumsum(apply(NWaW1aG0, 2, sum) / nrow(df))


### Effect measures 
TE_G <- CIFGa1 - CIFGa0
SE1_G <- CIFG_aW1aG0 - CIFGa0
SE2_G <- CIFGa1 - CIFG_aW1aG0

TE_W <- CIFWa1 - CIFWa0
SE1_W <- CIFW_aW1aG0 - CIFWa0
SE2_W <- CIFWa1 - CIFW_aW1aG0
