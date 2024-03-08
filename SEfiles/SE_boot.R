n_boot <- 1000

ids <- unique(c(df_a0$id,df_a1$id))
n_ids <- length(ids)

boot_SE <- lapply(1:n_boot, function(x) list())


for(i in 1:n_boot){
  
  print(i)
  
  boot_ids_a0 <- sample(1:length(df_a0$id), size = length(df_a0$id), replace = T)
  boot_S1 <- apply(S1Mat[boot_ids_a0,], 2, sum) / n_ids
  boot_ids_a0 <- sample(1:length(df_a0$id), size = length(df_a0$id), replace = T)
  boot_Sa0 <- apply(Sa0Mat[boot_ids_a0,], 2, sum) / n_ids
  
  boot_ids_a1 <- sample(1:length(df_a1$id), size = length(df_a1$id), replace = T)
  boot_S2 <- apply(S2Mat[boot_ids_a1,], 2, sum) / n_ids
  boot_ids_a1 <- sample(1:length(df_a1$id), size = length(df_a1$id), replace = T)
  boot_Sa1 <- apply(Sa1Mat[boot_ids_a1,], 2, sum) / n_ids
  
  boot_TE <- boot_Sa1 - boot_Sa0
  boot_SE1 <- boot_S1 - boot_Sa0
  boot_SE2 <- boot_Sa1 - boot_S2
  
  boot_SE[[i]]$boot_TE <- boot_TE
  boot_SE[[i]]$boot_SE1 <- boot_SE1
  boot_SE[[i]]$boot_SE2 <- boot_SE2
  
  
}



mat_TE <- t(sapply(1:1000, function(x) boot_SE[[x]][[1]]))
mat_SE1 <- t(sapply(1:1000, function(x) boot_SE[[x]][[2]]))
mat_SE2 <- t(sapply(1:1000, function(x) boot_SE[[x]][[3]]))


sd_te <- sd_se1 <- sd_se2 <- numeric(365)

for(i in 1:365){
  sd_te[i] <- sd(mat_TE[,i])
  sd_se1[i] <- sd(mat_SE1[,i])
  sd_se2[i] <- sd(mat_SE2[,i])
}

quant_te <- quant_se1 <- quant_se2 <- matrix(NA, nrow = 365, ncol = 2)

for(i in 1:365){
  quant_te[i,] <-  quantile(mat_TE[,i], probs = c(0.025, 0.975))
  quant_se1[i,] <- quantile(mat_SE1[,i], probs = c(0.025, 0.975))
  quant_se2[i,] <- quantile(mat_SE2[,i], probs = c(0.025, 0.975))
}



#save(boot_SE, file = "N:/durable/scrpts/OCBE/Working_files/Separable_effects/Sep_eff_final/For_Git/scripts/scriptsNew/NewBootPlot/BootSE.Rdata")















