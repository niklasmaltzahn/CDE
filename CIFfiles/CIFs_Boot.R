n_boot <- 1000

ids <- unique(c(df_a0$id,df_a1$id))
n_ids <- length(ids)

boot_SE <- lapply(1:n_boot, function(x) list())


for(i in 1:n_boot){
  
  print(i)
  
  boot_ids_a0 <- sample(1:length(df_a0$id), size = length(df_a0$id), replace = T)
  boot_CIFG_aW1aG0 <- cumsum(apply(NGaW1aG0[boot_ids_a0,], 2, sum) / n_ids)
  boot_ids_a0 <- sample(1:length(df_a0$id), size = length(df_a0$id), replace = T)
  boot_CIFGa0 <- cumsum(apply(NGa0Mat[boot_ids_a0,], 2, sum) / n_ids)
  boot_CIFWa0 <- cumsum(apply(NWa0Mat[boot_ids_a0,], 2, sum) / n_ids)
  
  boot_ids_a1 <- sample(1:length(df_a1$id), size = length(df_a1$id), replace = T)
  boot_CIFW_aW1aG0 <- cumsum(apply(NWaW1aG0[boot_ids_a1,], 2, sum) / n_ids)
  boot_ids_a1 <- sample(1:length(df_a1$id), size = length(df_a1$id), replace = T)
  boot_CIFWa1 <- cumsum(apply(NWa1Mat[boot_ids_a1,], 2, sum) / n_ids)
  boot_CIFGa1 <- cumsum(apply(NGa1Mat[boot_ids_a1,], 2, sum) / n_ids)
  
  
  boot_TE_G <- boot_CIFGa1 - boot_CIFGa0
  boot_SE1_G <- boot_CIFG_aW1aG0 - boot_CIFGa0
  boot_SE2_G <- boot_CIFGa1 - boot_CIFG_aW1aG0
  
  boot_TE_W <- boot_CIFWa1 - boot_CIFWa0
  boot_SE1_W <- boot_CIFW_aW1aG0 - boot_CIFWa0
  boot_SE2_W <- boot_CIFWa1 - boot_CIFW_aW1aG0
  
  boot_SE[[i]]$boot_TE_G <- boot_TE_G
  boot_SE[[i]]$boot_SE1_G <- boot_SE1_G
  boot_SE[[i]]$boot_SE2_G <- boot_SE2_G
  boot_SE[[i]]$boot_TE_W <- boot_TE_W
  boot_SE[[i]]$boot_SE1_W <- boot_SE1_W
  boot_SE[[i]]$boot_SE2_W <- boot_SE2_W
  
  
}


mat_TE_G <- t(sapply(1:1000, function(x) boot_SE[[x]][[1]]))
mat_SE1_G <- t(sapply(1:1000, function(x) boot_SE[[x]][[2]]))
mat_SE2_G <- t(sapply(1:1000, function(x) boot_SE[[x]][[3]]))
mat_TE_W <- t(sapply(1:1000, function(x) boot_SE[[x]][[4]]))
mat_SE1_W <- t(sapply(1:1000, function(x) boot_SE[[x]][[5]]))
mat_SE2_W <- t(sapply(1:1000, function(x) boot_SE[[x]][[6]]))




sd_teG <- sd_se1G <- sd_se2G <- sd_teW <- sd_se1W <- sd_se2W <- numeric(365)

for(i in 1:365){
  sd_teG[i] <- sd(mat_TE_G[,i])
  sd_se1G[i] <- sd(mat_SE1_G[,i])
  sd_se2G[i] <- sd(mat_SE2_G[,i])
  sd_teW[i] <- sd(mat_TE_W[,i])
  sd_se1W[i] <- sd(mat_SE1_W[,i])
  sd_se2W[i] <- sd(mat_SE2_W[,i])  
}


quant_teG <- quant_se1G <- quant_se2G <- quant_teW <- quant_se1W <- quant_se2W <- matrix(NA, nrow = 365, ncol = 2)

for(i in 1:365){
  quant_teG[i,] <-  quantile(mat_TE_G[,i], probs = c(0.025, 0.975))
  quant_se1G[i,] <- quantile(mat_SE1_G[,i], probs = c(0.025, 0.975))
  quant_se2G[i,] <- quantile(mat_SE2_G[,i], probs = c(0.025, 0.975))
  quant_teW[i,] <-  quantile(mat_TE_W[,i], probs = c(0.025, 0.975))
  quant_se1W[i,] <- quantile(mat_SE1_W[,i], probs = c(0.025, 0.975))
  quant_se2W[i,] <- quantile(mat_SE2_W[,i], probs = c(0.025, 0.975))
}



save(boot_SE, file = "N:/durable/scrpts/OCBE/Working_files/Separable_effects/Sep_eff_final/For_Git/scripts/scriptsNew/NewBootPlot/BootSE_CIFs.Rdata")




